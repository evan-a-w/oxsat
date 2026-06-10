include Theory_intf

module Packed = struct
  type t = T : (module S with type t = 's) * 's -> t
end

let pack (type s) (module M : S with type t = s) (impl : s) : Packed.t =
  T ((module M), impl)
;;
