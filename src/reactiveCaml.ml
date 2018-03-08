(** In this library ddg is represented with nodes which are connected via dependency relation. Each node is of a certain kind which is either
Variable or Map. Variable is used to store changable values and Map is used to perform computation on Values.  *)

type 'a t = 'a DynamicDependencyGraph.t
let read  = DynamicDependencyGraph.read
let read_exn = DynamicDependencyGraph.read_exn
let set_value = DynamicDependencyGraph.set_value
let return = DynamicDependencyGraph.return
let make_variable = DynamicDependencyGraph.make_variable
let map = DynamicDependencyGraph.map
let map2 = DynamicDependencyGraph.map2
let map3 = DynamicDependencyGraph.map3
let map4 = DynamicDependencyGraph.map4
let unordered_list_fold = DynamicDependencyGraph.unordered_list_fold