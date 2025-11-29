open! Core
open! Unboxed
open Rb_intf

module%template
  [@kind
    k = (value, bits64, bits64 & bits64, bits64 & float64)
    , v = (value, bits64, bits64 & bits64)] Make
    (Key : Key
  [@kind k])
    (Value : Value
  [@kind v]) =
struct
  module Key = Key
  module Value = Value

  type color : value mod external_ =
    | Red
    | Black

  module Node_elt = struct
    type t =
      #{ key : Key.t
       ; data : Value.t
       ; left : Ptr.t
       ; right : Ptr.t
       ; parent : Ptr.t
       ; color : color
       }

    let create_for_pool () =
      #{ key = Key.create_for_rb ()
       ; data = Value.create_for_rb ()
       ; left = Ptr.null ()
       ; right = Ptr.null ()
       ; parent = Ptr.null ()
       ; color = Black
       }
    ;;
  end

  module Node_pool =
    Pool.Make [@kind k & v & bits64 & bits64 & bits64 & value] (Node_elt)

  module Node = Node_elt

  type t =
    { pool : Node_pool.t
    ; mutable root : Ptr.t
    ; mutable size : int
    }

  let create () = { pool = Node_pool.create (); root = Ptr.null (); size = 0 }
  let length t = t.size
  let is_empty t = t.size = 0

  let clear t =
    t.root <- Ptr.null ();
    t.size <- 0
  ;;

  let get_node t ptr =
    if Ptr.is_null ptr then failwith "Attempted to get null node";
    Node_pool.get t.pool ptr
  ;;

  let set_node t ptr node =
    if Ptr.is_null ptr
    then failwith "Attempted to set null node"
    else Node_pool.set t.pool ptr node
  ;;

  let alloc_node t ~key ~data ~color =
    let ptr = Node_pool.alloc t.pool in
    let node =
      #{ Node.key
       ; Node.data
       ; Node.left = Ptr.null ()
       ; Node.right = Ptr.null ()
       ; Node.parent = Ptr.null ()
       ; Node.color
       }
    in
    Node_pool.set t.pool ptr node;
    ptr
  ;;

  let free_node t ptr = if not (Ptr.is_null ptr) then Node_pool.free t.pool ptr

  let rotate_left t node_ptr =
    let node = get_node t node_ptr in
    let right_ptr = node.#right in
    if Ptr.is_null right_ptr
    then failwith "Cannot rotate left with null right child"
    else (
      let right = get_node t right_ptr in
      set_node t node_ptr #{ node with right = right.#left };
      if not (Ptr.is_null right.#left)
      then (
        let left = get_node t right.#left in
        set_node t right.#left #{ left with parent = node_ptr });
      set_node t right_ptr #{ right with parent = node.#parent };
      if Ptr.is_null node.#parent
      then t.root <- right_ptr
      else (
        let parent = get_node t node.#parent in
        let parent_left = parent.#left in
        if (not (Ptr.is_null parent_left)) && Ptr.equal parent_left node_ptr
        then set_node t node.#parent #{ parent with left = right_ptr }
        else set_node t node.#parent #{ parent with right = right_ptr });
      let right = get_node t right_ptr in
      set_node t right_ptr #{ right with left = node_ptr };
      let node = get_node t node_ptr in
      set_node t node_ptr #{ node with parent = right_ptr })
  ;;

  let rotate_right t node_ptr =
    let node = get_node t node_ptr in
    let left_ptr = node.#left in
    if Ptr.is_null left_ptr
    then failwith "Cannot rotate right with null left child"
    else (
      let left = get_node t left_ptr in
      set_node t node_ptr #{ node with left = left.#right };
      if not (Ptr.is_null left.#right)
      then (
        let right = get_node t left.#right in
        set_node t left.#right #{ right with parent = node_ptr });
      set_node t left_ptr #{ left with parent = node.#parent };
      if Ptr.is_null node.#parent
      then t.root <- left_ptr
      else (
        let parent = get_node t node.#parent in
        let parent_right = parent.#right in
        if (not (Ptr.is_null parent_right)) && Ptr.equal parent_right node_ptr
        then set_node t node.#parent #{ parent with right = left_ptr }
        else set_node t node.#parent #{ parent with left = left_ptr });
      let left = get_node t left_ptr in
      set_node t left_ptr #{ left with right = node_ptr };
      let node = get_node t node_ptr in
      set_node t node_ptr #{ node with parent = left_ptr })
  ;;

  let rec insert_fixup t node_ptr =
    let node = get_node t node_ptr in
    let parent_ptr = node.#parent in
    if Ptr.is_null parent_ptr
    then set_node t node_ptr #{ node with color = Black }
    else (
      let parent = get_node t parent_ptr in
      match parent.#color with
      | Black -> ()
      | Red ->
        let grandparent_ptr = parent.#parent in
        if Ptr.is_null grandparent_ptr
        then failwith "Red node cannot be root"
        else (
          let grandparent = get_node t grandparent_ptr in
          let uncle_ptr =
            if Ptr.equal grandparent.#left parent_ptr
            then grandparent.#right
            else grandparent.#left
          in
          if not (Ptr.is_null uncle_ptr)
          then (
            let uncle = get_node t uncle_ptr in
            match uncle.#color with
            | Red ->
              set_node t parent_ptr #{ parent with color = Black };
              set_node t uncle_ptr #{ uncle with color = Black };
              set_node t grandparent_ptr #{ grandparent with color = Red };
              insert_fixup t grandparent_ptr
            | Black ->
              let #(_node_ptr, parent_ptr') =
                if Ptr.equal grandparent.#left parent_ptr
                then
                  if Ptr.equal parent.#right node_ptr
                  then (
                    rotate_left t parent_ptr;
                    #(parent_ptr, node_ptr))
                  else #(node_ptr, parent_ptr)
                else if Ptr.equal parent.#left node_ptr
                then (
                  rotate_right t parent_ptr;
                  #(parent_ptr, node_ptr))
                else #(node_ptr, parent_ptr)
              in
              let parent = get_node t parent_ptr' in
              set_node t parent_ptr' #{ parent with color = Black };
              let grandparent = get_node t grandparent_ptr in
              set_node t grandparent_ptr #{ grandparent with color = Red };
              if Ptr.equal grandparent.#left parent_ptr'
              then rotate_right t grandparent_ptr
              else rotate_left t grandparent_ptr)
          else (
            let #(_node_ptr, parent_ptr') =
              if Ptr.equal grandparent.#left parent_ptr
              then
                if Ptr.equal parent.#right node_ptr
                then (
                  rotate_left t parent_ptr;
                  #(parent_ptr, node_ptr))
                else #(node_ptr, parent_ptr)
              else if Ptr.equal parent.#left node_ptr
              then (
                rotate_right t parent_ptr;
                #(parent_ptr, node_ptr))
              else #(node_ptr, parent_ptr)
            in
            let parent = get_node t parent_ptr' in
            set_node t parent_ptr' #{ parent with color = Black };
            let grandparent = get_node t grandparent_ptr in
            set_node t grandparent_ptr #{ grandparent with color = Red };
            if Ptr.equal grandparent.#left parent_ptr'
            then rotate_right t grandparent_ptr
            else rotate_left t grandparent_ptr)))
  ;;

  let insert t ~key ~data =
    if Ptr.is_null t.root
    then (
      let new_node = alloc_node t ~key ~data ~color:Black in
      t.root <- new_node;
      t.size <- t.size + 1)
    else (
      let rec find_insertion_point current_ptr =
        let current = get_node t current_ptr in
        let cmp = Key.compare key current.#key in
        if cmp = 0
        then set_node t current_ptr #{ current with data }
        else if cmp < 0
        then
          if Ptr.is_null current.#left
          then (
            let new_node = alloc_node t ~key ~data ~color:Red in
            set_node t current_ptr #{ current with left = new_node };
            let new_node_data = get_node t new_node in
            set_node t new_node #{ new_node_data with parent = current_ptr };
            t.size <- t.size + 1;
            insert_fixup t new_node)
          else find_insertion_point current.#left
        else if Ptr.is_null current.#right
        then (
          let new_node = alloc_node t ~key ~data ~color:Red in
          set_node t current_ptr #{ current with right = new_node };
          let new_node_data = get_node t new_node in
          set_node t new_node #{ new_node_data with parent = current_ptr };
          t.size <- t.size + 1;
          insert_fixup t new_node)
        else find_insertion_point current.#right
      in
      find_insertion_point t.root)
  ;;

  let rec iter_helper t node_ptr ~f =
    if not (Ptr.is_null node_ptr)
    then (
      let node = get_node t node_ptr in
      iter_helper t node.#left ~f;
      f ~key:node.#key ~data:node.#data;
      iter_helper t node.#right ~f)
  ;;

  let iter t ~f = iter_helper t t.root ~f
  let iteri = iter

  let fold t ~init ~f =
    let acc = ref init in
    iter t ~f:(fun ~key ~data -> acc := f ~acc:!acc ~key ~data);
    !acc
  ;;

  let rec fold_helper t node_ptr ~f ~done_ ~(acc : _ or_null) =
    if Ptr.is_null node_ptr || Local_ref.get done_
    then acc
    else (
      let node = get_node t node_ptr in
      let acc = fold_helper t node.#left ~f ~done_ ~acc in
      let acc = f ~done_ ~acc ~key:node.#key ~data:node.#data in
      fold_helper t node.#right ~f ~done_ ~acc)
  ;;

  let fold_or_null t ~init ~(local_ f) =
    let done_ = Local_ref.create false in
    fold_helper t t.root ~done_ ~acc:init ~f [@nontail]
  ;;

  let%template to_array t : #(Key.t * Value.t) array @ m =
    let len = length t in
    if len = 0
    then [||]
    else (
      (let arr =
         (Array.create [@alloc a])
           ~len
           #(Key.create_for_rb (), Value.create_for_rb ())
       in
       let idx = ref 0 in
       iter t ~f:(fun ~key ~data ->
         arr.(!idx) <- #(key, data);
         incr idx);
       arr)
      [@exclave_if_stack a])
  [@@alloc a @ m = (stack_local, heap_global)]
  ;;

  let%template to_keys_array t : Key.t array @ m =
    let len = length t in
    if len = 0
    then [||]
    else (
      (let arr = (Array.create [@alloc a]) ~len (Key.create_for_rb ()) in
       let idx = ref 0 in
       iter t ~f:(fun ~key ~data:_ ->
         arr.(!idx) <- key;
         incr idx);
       arr)
      [@exclave_if_stack a])
  [@@alloc a @ m = (stack_local, heap_global)]
  ;;

  let of_array_exn arr =
    let t = create () in
    for i = 0 to Array.length arr - 1 do
      let #(key, data) = arr.(i) in
      insert t ~key ~data
    done;
    t
  ;;

  let get_color t ptr =
    if Ptr.is_null ptr then Black else (get_node t ptr).#color
  ;;

  (* holy this function... *)
  let rec delete_fixup t node_ptr =
    if Ptr.is_null node_ptr || Ptr.equal t.root node_ptr
    then (
      if not (Ptr.is_null node_ptr)
      then (
        let node = get_node t node_ptr in
        if phys_equal node.#color Red
        then set_node t node_ptr #{ node with color = Black }))
    else (
      let node = get_node t node_ptr in
      if phys_equal node.#color Red
      then set_node t node_ptr #{ node with color = Black }
      else (
        let parent_ptr = node.#parent in
        if Ptr.is_null parent_ptr
        then ()
        else (
          let parent = get_node t parent_ptr in
          let is_left = Ptr.equal parent.#left node_ptr in
          let sibling_ptr = if is_left then parent.#right else parent.#left in
          if Ptr.is_null sibling_ptr
          then ()
          else (
            let sibling = get_node t sibling_ptr in
            if phys_equal sibling.#color Red
            then (
              set_node t parent_ptr #{ parent with color = Red };
              set_node t sibling_ptr #{ sibling with color = Black };
              if is_left
              then rotate_left t parent_ptr
              else rotate_right t parent_ptr;
              delete_fixup t node_ptr)
            else (
              let sibling_left_color = get_color t sibling.#left in
              let sibling_right_color = get_color t sibling.#right in
              if phys_equal sibling_left_color Black
                 && phys_equal sibling_right_color Black
              then (
                set_node t sibling_ptr #{ sibling with color = Red };
                let parent = get_node t parent_ptr in
                if phys_equal parent.#color Black
                then delete_fixup t parent_ptr
                else set_node t parent_ptr #{ parent with color = Black })
              else if is_left
              then
                if phys_equal sibling_right_color Black
                then (
                  let sibling_left = get_node t sibling.#left in
                  set_node t sibling.#left #{ sibling_left with color = Black };
                  set_node t sibling_ptr #{ sibling with color = Red };
                  rotate_right t sibling_ptr;
                  let parent = get_node t parent_ptr in
                  let new_sibling_ptr = parent.#right in
                  let new_sibling = get_node t new_sibling_ptr in
                  set_node
                    t
                    new_sibling_ptr
                    #{ new_sibling with color = parent.#color };
                  set_node t parent_ptr #{ parent with color = Black };
                  let new_sibling = get_node t new_sibling_ptr in
                  if not (Ptr.is_null new_sibling.#right)
                  then (
                    let right = get_node t new_sibling.#right in
                    set_node t new_sibling.#right #{ right with color = Black });
                  rotate_left t parent_ptr)
                else (
                  let parent = get_node t parent_ptr in
                  set_node t sibling_ptr #{ sibling with color = parent.#color };
                  set_node t parent_ptr #{ parent with color = Black };
                  let sibling_right = get_node t sibling.#right in
                  set_node
                    t
                    sibling.#right
                    #{ sibling_right with color = Black };
                  rotate_left t parent_ptr)
              else if phys_equal sibling_left_color Black
              then (
                let sibling_right = get_node t sibling.#right in
                set_node t sibling.#right #{ sibling_right with color = Black };
                set_node t sibling_ptr #{ sibling with color = Red };
                rotate_left t sibling_ptr;
                let parent = get_node t parent_ptr in
                let new_sibling_ptr = parent.#left in
                let new_sibling = get_node t new_sibling_ptr in
                set_node
                  t
                  new_sibling_ptr
                  #{ new_sibling with color = parent.#color };
                set_node t parent_ptr #{ parent with color = Black };
                let new_sibling = get_node t new_sibling_ptr in
                if not (Ptr.is_null new_sibling.#left)
                then (
                  let left = get_node t new_sibling.#left in
                  set_node t new_sibling.#left #{ left with color = Black });
                rotate_right t parent_ptr)
              else (
                let parent = get_node t parent_ptr in
                set_node t sibling_ptr #{ sibling with color = parent.#color };
                set_node t parent_ptr #{ parent with color = Black };
                let sibling_left = get_node t sibling.#left in
                set_node t sibling.#left #{ sibling_left with color = Black };
                rotate_right t parent_ptr))))))
  ;;

  let remove t key =
    (* Helper to physically delete a node with at most one child *)
    let delete_node_with_one_child node_ptr =
      let node = get_node t node_ptr in
      let parent_ptr = node.#parent in
      let left_is_null = Ptr.is_null node.#left in
      let right_is_null = Ptr.is_null node.#right in
      (* Determine the child (or null) *)
      let child_ptr =
        if not left_is_null
        then node.#left
        else if not right_is_null
        then node.#right
        else Ptr.null ()
      in
      (* Replace node_ptr with child_ptr in the tree structure *)
      if Ptr.is_null parent_ptr
      then (
        (* Deleting the root *)
        t.root <- child_ptr;
        if not (Ptr.is_null child_ptr)
        then (
          let child = get_node t child_ptr in
          set_node
            t
            child_ptr
            #{ child with parent = Ptr.null (); color = Black }))
      else (
        let parent = get_node t parent_ptr in
        let is_left = Ptr.equal parent.#left node_ptr in
        if is_left
        then set_node t parent_ptr #{ parent with left = child_ptr }
        else set_node t parent_ptr #{ parent with right = child_ptr };
        if not (Ptr.is_null child_ptr)
        then (
          let child = get_node t child_ptr in
          set_node t child_ptr #{ child with parent = parent_ptr }));
      (* If the deleted node was black, we need to fix the tree *)
      let deleted_was_black = phys_equal node.#color Black in
      let child_is_red =
        if Ptr.is_null child_ptr
        then false
        else phys_equal (get_node t child_ptr).#color Red
      in
      free_node t node_ptr;
      if deleted_was_black
      then
        if child_is_red
        then (
          (* Child is red, just recolor it to black *)
          let child = get_node t child_ptr in
          set_node t child_ptr #{ child with color = Black })
        else if not (Ptr.is_null parent_ptr)
        then (
          (* Need to run fixup with a temporary black node *)
          let parent = get_node t parent_ptr in
          let fake_child =
            #{ Node.key = Key.create_for_rb ()
             ; Node.data = Value.create_for_rb ()
             ; Node.left = Ptr.null ()
             ; Node.right = Ptr.null ()
             ; Node.parent = parent_ptr
             ; Node.color = Black
             }
          in
          let temp_ptr = Node_pool.alloc t.pool in
          Node_pool.set t.pool temp_ptr fake_child;
          let is_left = Ptr.equal parent.#left child_ptr in
          if is_left
          then set_node t parent_ptr #{ parent with left = temp_ptr }
          else set_node t parent_ptr #{ parent with right = temp_ptr };
          delete_fixup t temp_ptr;
          let parent = get_node t parent_ptr in
          if Ptr.equal parent.#left temp_ptr
          then set_node t parent_ptr #{ parent with left = Ptr.null () }
          else if Ptr.equal parent.#right temp_ptr
          then set_node t parent_ptr #{ parent with right = Ptr.null () };
          Node_pool.free t.pool temp_ptr)
    in
    let rec find_and_remove node_ptr =
      if not (Ptr.is_null node_ptr)
      then (
        let node = get_node t node_ptr in
        let cmp = Key.compare key node.#key in
        if cmp < 0
        then find_and_remove node.#left
        else if cmp > 0
        then find_and_remove node.#right
        else (
          (* Found the node to delete *)
          t.size <- t.size - 1;
          let left_is_null = Ptr.is_null node.#left in
          let right_is_null = Ptr.is_null node.#right in
          if left_is_null || right_is_null
          then
            (* Node has at most one child *)
            delete_node_with_one_child node_ptr
          else (
            (* Node has two children - find successor *)
            let rec find_min ptr =
              let n = get_node t ptr in
              if Ptr.is_null n.#left then ptr else find_min n.#left
            in
            let successor_ptr = find_min node.#right in
            let successor = get_node t successor_ptr in
            (* Copy successor's key and data to current node *)
            set_node
              t
              node_ptr
              #{ node with key = successor.#key; data = successor.#data };
            (* Now directly delete the successor node (which has at most one child) *)
            delete_node_with_one_child successor_ptr)))
    in
    find_and_remove t.root
  ;;

  let min_exn t : #(Key.t * Value.t) =
    if Ptr.is_null t.root then raise (Not_found_s [%message "Tree is empty"]);
    let rec find_min ptr =
      let node = get_node t ptr in
      if Ptr.is_null node.#left then ptr else find_min node.#left
    in
    let min_ptr = find_min t.root in
    let min_node = get_node t min_ptr in
    #(min_node.#key, min_node.#data)
  ;;

  let max_exn t : #(Key.t * Value.t) =
    if Ptr.is_null t.root then raise (Not_found_s [%message "Tree is empty"]);
    let rec find_max ptr =
      let node = get_node t ptr in
      if Ptr.is_null node.#right then ptr else find_max node.#right
    in
    let max_ptr = find_max t.root in
    let max_node = get_node t max_ptr in
    #(max_node.#key, max_node.#data)
  ;;

  let pop_min_exn t : #(Key.t * Value.t) =
    if Ptr.is_null t.root then raise (Not_found_s [%message "Tree is empty"]);
    let rec find_min ptr =
      let node = get_node t ptr in
      if Ptr.is_null node.#left then ptr else find_min node.#left
    in
    let min_ptr = find_min t.root in
    let min_node = get_node t min_ptr in
    let result = #(min_node.#key, min_node.#data) in
    remove t min_node.#key;
    result
  ;;

  let pop_max_exn t : #(Key.t * Value.t) =
    if Ptr.is_null t.root then raise (Not_found_s [%message "Tree is empty"]);
    let rec find_max ptr =
      let node = get_node t ptr in
      if Ptr.is_null node.#right then ptr else find_max node.#right
    in
    let max_ptr = find_max t.root in
    let max_node = get_node t max_ptr in
    let result = #(max_node.#key, max_node.#data) in
    remove t max_node.#key;
    result
  ;;

  let validate t =
    let rec check_red_property node_ptr =
      if not (Ptr.is_null node_ptr)
      then (
        let node = get_node t node_ptr in
        (match node.#color with
         | Red ->
           if not (Ptr.is_null node.#left)
           then (
             let left_node = get_node t node.#left in
             if phys_equal left_node.#color Red
             then failwith "Red node has red left child");
           if not (Ptr.is_null node.#right)
           then (
             let right_node = get_node t node.#right in
             if phys_equal right_node.#color Red
             then failwith "Red node has red right child")
         | Black -> ());
        check_red_property node.#left;
        check_red_property node.#right)
    in
    let rec check_black_height node_ptr =
      if Ptr.is_null node_ptr
      then 1
      else (
        let node = get_node t node_ptr in
        let left_height = check_black_height node.#left in
        let right_height = check_black_height node.#right in
        if left_height <> right_height
        then
          failwith
            [%string
              "Black height mismatch: left=%{left_height#Int} \
               right=%{right_height#Int}"];
        left_height
        +
        match node.#color with
        | Black -> 1
        | Red -> 0)
    in
    let rec check_bst node_ptr min_ptr max_ptr =
      if not (Ptr.is_null node_ptr)
      then (
        let node = get_node t node_ptr in
        if not (Ptr.is_null min_ptr)
        then (
          let min_node = get_node t min_ptr in
          if Key.compare node.#key min_node.#key <= 0
          then failwith "BST property violated: key <= min");
        if not (Ptr.is_null max_ptr)
        then (
          let max_node = get_node t max_ptr in
          if Key.compare node.#key max_node.#key >= 0
          then failwith "BST property violated: key >= max");
        check_bst node.#left min_ptr node_ptr;
        check_bst node.#right node_ptr max_ptr)
    in
    if not (Ptr.is_null t.root)
    then (
      let root = get_node t t.root in
      if phys_equal root.#color Red then failwith "Root is red");
    check_red_property t.root;
    ignore (check_black_height t.root : int);
    check_bst t.root (Ptr.null ()) (Ptr.null ())
  ;;

  module Kv_option =
    Optional_pair.Make [@kind k v]
      (struct
        include Key

        let trivial_create_for_none = create_for_rb
      end)
      (struct
        include Value

        let trivial_create_for_none = create_for_rb
      end)

  let find t key =
    let rec search node_ptr =
      if Ptr.is_null node_ptr
      then Kv_option.none ()
      else (
        let node = get_node t node_ptr in
        let cmp = Key.compare key node.#key in
        if cmp = 0
        then Kv_option.some #(node.#key, node.#data)
        else if cmp < 0
        then search node.#left
        else search node.#right)
    in
    search t.root
  ;;

  let find_exn t key =
    let result = find t key in
    if Kv_option.is_none result
    then raise (Not_found_s [%message "Key not found"]);
    let #(_, data) = Kv_option.value_exn result in
    data
  ;;

  let mem t key = Kv_option.is_some (find t key)

  let min t =
    if Ptr.is_null t.root
    then Kv_option.none ()
    else (
      let rec find_min ptr =
        let node = get_node t ptr in
        if Ptr.is_null node.#left then ptr else find_min node.#left
      in
      let min_ptr = find_min t.root in
      let min_node = get_node t min_ptr in
      Kv_option.some #(min_node.#key, min_node.#data))
  ;;

  let max t =
    if Ptr.is_null t.root
    then Kv_option.none ()
    else (
      let rec find_max ptr =
        let node = get_node t ptr in
        if Ptr.is_null node.#right then ptr else find_max node.#right
      in
      let max_ptr = find_max t.root in
      let max_node = get_node t max_ptr in
      Kv_option.some #(max_node.#key, max_node.#data))
  ;;

  let pop_min t =
    if Ptr.is_null t.root
    then Kv_option.none ()
    else (
      let rec find_min ptr =
        let node = get_node t ptr in
        if Ptr.is_null node.#left then ptr else find_min node.#left
      in
      let min_ptr = find_min t.root in
      let min_node = get_node t min_ptr in
      let result = Kv_option.some #(min_node.#key, min_node.#data) in
      remove t min_node.#key;
      result)
  ;;

  let pop_max t =
    if Ptr.is_null t.root
    then Kv_option.none ()
    else (
      let rec find_max ptr =
        let node = get_node t ptr in
        if Ptr.is_null node.#right then ptr else find_max node.#right
      in
      let max_ptr = find_max t.root in
      let max_node = get_node t max_ptr in
      let result = Kv_option.some #(max_node.#key, max_node.#data) in
      remove t max_node.#key;
      result)
  ;;

  module Iter = struct
    type tree = t

    module Ptr_vec = Vec.Make [@kind bits64] (struct
        type t = Ptr.t

        let create_for_vec = Ptr.null
      end)

    type nonrec t =
      { tree : tree
      ; stack : Ptr_vec.t
      }

    let create tree =
      let stack = Ptr_vec.create () in
      (* Push all left-most nodes onto stack *)
      let rec push_left ptr =
        if not (Ptr.is_null ptr)
        then (
          Ptr_vec.push stack ptr;
          let node = get_node tree ptr in
          push_left node.#left)
      in
      push_left tree.root;
      { tree; stack }
    ;;

    let create_from tree key =
      let stack = Ptr_vec.create () in
      (* Navigate to the first node >= key, pushing ancestors onto stack *)
      let rec find_starting_point ptr =
        if not (Ptr.is_null ptr)
        then (
          let node = get_node tree ptr in
          let cmp = Key.compare key node.#key in
          if cmp = 0
          then (
            (* Found exact match - push this node and all its left ancestors *)
            Ptr_vec.push stack ptr;
            let rec push_left p =
              if not (Ptr.is_null p)
              then (
                let n = get_node tree p in
                push_left n.#left;
                Ptr_vec.push stack p)
            in
            push_left node.#left)
          else if cmp < 0
          then (
            (* Key is less than current node - current might be our answer *)
            (* Push current node and search left *)
            Ptr_vec.push stack ptr;
            find_starting_point node.#left)
          else
            (* Key is greater than current node - search right *)
            find_starting_point node.#right)
      in
      find_starting_point tree.root;
      { tree; stack }
    ;;

    let is_done iter = Ptr_vec.length iter.stack = 0

    let peek iter =
      if is_done iter
      then Kv_option.none ()
      else (
        let top_ptr = Ptr_vec.last_exn iter.stack in
        let node = get_node iter.tree top_ptr in
        Kv_option.some #(node.#key, node.#data))
    ;;

    let next iter =
      if is_done iter
      then Kv_option.none ()
      else (
        let current_ptr = Ptr_vec.pop_exn iter.stack in
        let current = get_node iter.tree current_ptr in
        let result = Kv_option.some #(current.#key, current.#data) in
        (* Push all left-most nodes of right subtree *)
        let rec push_left ptr =
          if not (Ptr.is_null ptr)
          then (
            Ptr_vec.push iter.stack ptr;
            let node = get_node iter.tree ptr in
            push_left node.#left)
        in
        if not (Ptr.is_null current.#right) then push_left current.#right;
        result)
    ;;
  end
end
