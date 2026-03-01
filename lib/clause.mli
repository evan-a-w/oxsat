include Clause_intf.S

val negate : t -> unit

module Pool : sig
  type pool

  val create : ?chunk_size:int -> unit -> pool
  val alloc : pool -> Ds.Ptr.t
  val free : pool -> Ds.Ptr.t -> unit
  val get : pool -> Ds.Ptr.t -> t
  val set : pool -> Ds.Ptr.t -> t -> unit
  val iter : pool -> f:(Ds.Ptr.t -> unit) -> unit
  val outstanding : pool -> int
end
