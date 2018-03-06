(** In this library ddg is represented with nodes which are connected via dependency relation. Each node is of a certain kind which is either
Variable or Map. Variable is used to store changable values and Map is used to perform computation on Values.  *)

type 'a t = 'a DynamicDependencyGraph.t
let read : ('a t -> 'a option) = DynamicDependencyGraph.read
let read_exn : ('a t -> 'a) = DynamicDependencyGraph.read_exn
let set_value : ('a t -> 'a -> unit) = DynamicDependencyGraph.set_value
let return : ('a -> 'a t) = DynamicDependencyGraph.return
let make_variable : ('a -> 'a t) = DynamicDependencyGraph.make_variable
let map : ('a t -> f:('a -> 'b) -> 'b t) = DynamicDependencyGraph.map
let map2 : ('a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t) = DynamicDependencyGraph.map2
let map3 : ('a t -> 'b t -> 'c t -> f:('a -> 'b -> 'c -> 'd) -> 'd t) = DynamicDependencyGraph.map3
let map4 : ('a t -> 'b t -> 'c t -> 'd t -> f:('a -> 'b -> 'c -> 'd -> 'e) -> 'e t) = DynamicDependencyGraph.map4
let unordered_list_fold : ( f:('a -> 'b -> 'a) -> f_inv:('a -> 'b -> 'a) -> init:('a t) -> (('b t) list) -> 'a t)= DynamicDependencyGraph.unordered_list_fold