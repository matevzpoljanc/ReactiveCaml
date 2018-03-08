type 'a t

val read: 'a t -> 'a option
val read_exn: 'a t -> 'a
val set_value: 'a t -> 'a -> unit

val return: 'a -> 'a t
val make_variable: 'a -> 'a t

val map: 'a t -> f:('a -> 'b) -> 'b t
val map2: 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
val map3: 'a t -> 'b t -> 'c t -> f:('a -> 'b -> 'c -> 'd) -> 'd t
val map4: 'a t -> 'b t -> 'c t -> 'd t -> f:('a -> 'b -> 'c -> 'd -> 'e) -> 'e t

val unordered_list_fold: f:('a -> 'b -> 'a) -> f_inv:('a -> 'b -> 'a) -> init:('a t) -> (('b t) list) -> 'a t