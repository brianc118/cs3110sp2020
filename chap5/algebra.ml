module type Ring = sig
  type t

  val zero : t

  val one : t

  val ( + ) : t -> t -> t

  val ( ~- ) : t -> t

  val ( * ) : t -> t -> t

  val to_string : t -> string

  val of_int : int -> t
end

module type Field = sig
  include Ring

  val ( / ) : t -> t -> t
end

module IntRingImpl = struct
  type t = int

  let zero = 0

  let one = 1

  let ( + ) = ( + )

  let ( ~- ) = ( ~- )

  let ( * ) = ( * )

  let to_string = string_of_int

  let of_int n = n
end

module IntRing : Ring = IntRingImpl

module IntField : Field = struct
  include IntRingImpl

  let ( / ) = ( / )
end

module FloatRingImpl = struct
  type t = float

  let zero = 0.

  let one = 1.

  let ( + ) = ( +. )

  let ( ~- ) = ( ~-. )

  let ( * ) = ( *. )

  let to_string = string_of_float

  let of_int n = float_of_int n
end

module FloatRing : Ring = FloatRingImpl

module FloatField = struct
  include FloatRingImpl

  let ( / ) = ( /. )
end

module RationalField (M : Field) = struct
  type t = M.t * M.t

  let zero = (M.zero, M.zero)

  let one = (M.one, M.one)

  let ( + ) (a, b) (c, d) = (M.( + ) (M.( * ) a b) (M.( * ) c b), M.( * ) b d)

  let ( ~- ) (a, b) : t = (M.( ~- ) a, b)

  let ( / ) (a, b) (c, d) = (M.( * ) a d, M.( * ) b c)

  let ( * ) (a, b) (c, d) = (M.( * ) a c, M.( * ) b d)

  let to_string (a, b) = M.to_string a ^ "/" ^ M.to_string b

  let of_int n = (M.of_int n, M.one)
end

module IntRational : Field = RationalField (IntField)

module FloatRational : Field = RationalField (FloatField)
