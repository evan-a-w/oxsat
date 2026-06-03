include Theory_intf

module Packed = struct
  type t = T : (module S with type t = 's) * 's -> t

  let create (type s) (module M : S with type t = s) (impl : s) =
    T ((module M), impl)
  ;;

  let assert_literal (T ((module M), t)) literal = M.assert_literal t literal
  let check_consistent (T ((module M), t)) = M.check_consistent t
  let pop (T ((module M), t)) ~to_decision_level = M.pop t ~to_decision_level
  let on_new_var (T ((module M), t)) ~var = M.on_new_var t ~var
end
