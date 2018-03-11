(** In this library ddg is represented with nodes which are connected via dependency relation. Each node is of a certain kind which is either
Variable or Map. Variable is used to store changable values and Map is used to perform computation on Values.  *)
type 'a node_value = 
    | Value of 'a
    | Err of exn

let unpack_node_value nv = match nv with
    | Value v -> v
    | Err e -> raise e

module rec Kind : sig 
    type 'a t =
    | Variable of 'a
    | Map: (('a0 -> 'a) * 'a0 Node.t) -> 'a t
    | Map2: ('a0 -> 'a1 -> 'a) * 'a0 Node.t * 'a1 Node.t -> 'a t
    | Map3: ('a0 -> 'a1 -> 'a2 -> 'a) * 'a0 Node.t * 'a1 Node.t * 'a2 Node.t -> 'a t
    | Map4: ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a) * 'a0 Node.t * 'a1 Node.t * 'a2 Node.t * 'a3 Node.t -> 'a t
    | If_then_else: bool Node.t * 'a Node.t * 'a Node.t -> 'a t
    | Unordered_list_fold: ('a -> 'a0 -> 'a) * ('a -> 'a0 -> 'a) * 'a Node.t * ('a0 Node.t list) -> 'a t
    
    val value: 'a t -> 'a node_value
    val value_list_fold: 'a t -> old_node:'b node_value -> new_node:'b node_value -> old_result:'a node_value -> 'a node_value

end = struct
    type 'a t =
    | Variable of 'a
    | Map: (('a0 -> 'a) * 'a0 Node.t) -> 'a t
    | Map2: ('a0 -> 'a1 -> 'a) * 'a0 Node.t * 'a1 Node.t -> 'a t
    | Map3: ('a0 -> 'a1 -> 'a2 -> 'a) * 'a0 Node.t * 'a1 Node.t * 'a2 Node.t -> 'a t
    | Map4: ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a) * 'a0 Node.t * 'a1 Node.t * 'a2 Node.t * 'a3 Node.t -> 'a t
    | If_then_else: bool Node.t * 'a Node.t * 'a Node.t -> 'a t
    | Unordered_list_fold: ('a -> 'a0 -> 'a) * ('a -> 'a0 -> 'a) * 'a Node.t * ('a0 Node.t list) -> 'a t 
    
    exception OneOrMoreArgumentsInvalid
    (** Function is called whenever node recomputes *)
    let value t =
        match t with
        | Variable v -> Value v
        | Map (f, n1) -> (match Node.read n1 with 
                        | Value v -> (try Value (f v) with e -> Err e)
                        | Err e -> Err e)
        | Map2 (f, n1, n2) -> (match Node.read n1, Node.read n2 with
            | (Value v1, Value v2) -> (try Value (f v1 v2) with e -> Err e)
            | _ -> Err OneOrMoreArgumentsInvalid)
        | Map3 (f, n1, n2, n3) -> (match Node.read n1, Node.read n2, Node.read n3 with
            | (Value v1, Value v2, Value v3) -> (try Value (f v1 v2 v3) with e -> Err e)
            | _ -> Err OneOrMoreArgumentsInvalid)
        | Map4 (f, n1, n2, n3, n4) -> (match Node.read n1, Node.read n2, Node.read n3, Node.read n4 with
            | (Value v1, Value v2, Value v3, Value v4) -> (try Value (f v1 v2 v3 v4) with e -> Err e)
            | _ -> Err OneOrMoreArgumentsInvalid)
        | If_then_else (b, if_true, if_false) -> 
            if Node.read_exn b then 
                (match Node.read if_true with 
                | Value v -> (try Value v with e -> Err e)
                | Err e -> Err e) 
            else (match Node.read if_false with 
                | Value v -> (try Value v with e -> Err e)
                | Err e -> Err e) 
        | Unordered_list_fold (f, f_inv, init, l) -> (try Value (List.fold_left f (Node.read_exn init) @@ List.map (Node.read_exn) l) with e -> Err e)
    
    exception NodeNotListFold
    let value_list_fold t ~old_node ~new_node ~old_result =
        match t with
        | Unordered_list_fold (f, f_inv, _, _) -> (
            match old_node, new_node, old_result with
            | Value old_n, Value new_n, Value old_r -> (try Value (f (f_inv old_r (Obj.magic old_n)) (Obj.magic new_n)) with e -> Err e)
            | _ -> Err OneOrMoreArgumentsInvalid
        )
        | _ -> raise NodeNotListFold
end
and Node: sig 
    type 'a t = {
        mutable value : 'a node_value;
        mutable kind: 'a Kind.t;
        mutable dependencies: ('a node_value * 'a node_value -> unit) list
    }
    val create: 'a Kind.t -> unit -> 'a t
    val read: 'a t -> 'a node_value
    val read_exn: 'a t -> 'a
    val write: 'a t -> 'a node_value -> unit
    val write_exn: 'a t -> 'a -> unit
    val add_dependency: ?update_last:bool -> 'a t -> ('a node_value * 'a node_value -> unit) -> unit
    val recompute: 'a t -> unit
    val recompute_fold: 'a t -> old: 'b node_value -> new_v: 'b node_value -> unit
end = struct 
     type 'a t = {
        mutable value : 'a node_value;
        mutable kind: 'a Kind.t;
        mutable dependencies: ('a node_value * 'a node_value -> unit) list
    }
    (** Create new node of a kind and initialize it's value  *)    
    let create kind () = {
        value =  Kind.value kind;
        kind;
        dependencies = []
    }
    (** Read node's value *)
    let read t = t.value

    exception EmptyNode
    (** Read node's value. *)
    let read_exn t = unpack_node_value t.value
   
    (** Update value of the node. If node is of kind Variable then also update value of Kind *)
    let update_value t r =
        match t.kind with
        | Variable _ -> t.kind <- Variable (unpack_node_value r); t.value <- r
        | _ -> t.value <- r
    (** Write new value into node. If node's value actually change then call recompute on all of it's dependencies *)
    let write t v = 
        let old_value = read t in
        if old_value != v then
            update_value t v;
            List.iter (fun f -> f (old_value, v)) t.dependencies

    let write_exn t v =
        let old_value = read_exn t in
        if old_value != v then
            update_value t (Value v);
            List.iter (fun f -> f (Value old_value, Value v)) t.dependencies

    (** Add new dependency for a node. Dependency is a function which when called recomputes some other node *)
    let add_dependency ?(update_last = false) t dep = 
        if update_last then
            t.dependencies <- t.dependencies @ [dep]
        else
            t.dependencies <- dep::t.dependencies
            
    (** Function used to recompute and update current node's value *)
    let recompute t =
        write t @@ Kind.value t.kind

    let recompute_fold t ~old ~new_v = 
        write t @@ Kind.value_list_fold t.kind ~old_node:old ~new_node:new_v ~old_result:(read t)

end
type 'a t = 'a Node.t
(** Read value option of the node *)
let read a = Node.read a

(** Read the value of the node *)
let read_exn a = Node.read_exn a

(** Set the value of a node *)
let set_value t v = Node.write_exn t v
(** Return node representation of a value *)
let return v = Node.create (Kind.Variable v) ()
(** Same as return with clearer name *)
let make_variable v = return v

(** Map a function over a node *)
let map n1 ~f = 
    let b = Node.create (Kind.Map (f,n1)) () in
    Node.add_dependency n1 (fun x1 -> Node.recompute b);
    b

(** Map two argument function over two nodes *)
let map2 n1 n2 ~f = 
    let c = Node.create (Kind.Map2 (f, n1, n2)) () in
    Node.add_dependency n1 (fun x1 -> Node.recompute c);
    Node.add_dependency n2 (fun x2 -> Node.recompute c);
    c

(** Map three argument function over three nodes *)
let map3 n1 n2 n3 ~f =
    let d = Node.create (Kind.Map3 (f, n1, n2, n3)) () in
    Node.add_dependency n1 (fun x1 -> Node.recompute d);
    Node.add_dependency n2 (fun x2 -> Node.recompute d);
    Node.add_dependency n3 (fun x3 -> Node.recompute d);
    d

(** Map four argument function over four nodes *)
let map4 n1 n2 n3 n4 ~f =
    let e = Node.create (Kind.Map4 (f, n1, n2, n3, n4)) () in
    Node.add_dependency n1 (fun x1 -> Node.recompute e);
    Node.add_dependency n2 (fun x2 -> Node.recompute e);
    Node.add_dependency n3 (fun x3 -> Node.recompute e);
    Node.add_dependency n4 (fun x4 -> Node.recompute e);
    e

let unordered_list_fold ~f ~f_inv ~init l = 
    let r = Node.create (Kind.Unordered_list_fold (f,f_inv,init,l)) () in
    Node.add_dependency init (fun x1 -> Node.recompute r);
    List.iter (fun n -> Node.add_dependency n (fun (old,new_v) -> Node.recompute_fold r ~old ~new_v)) l;
    r

let if_then_else b ~if_true ~if_false =
    let c = Node.create (Kind.If_then_else (b, if_true, if_false)) () in
    Node.add_dependency b (fun x -> Node.recompute c);
    Node.add_dependency ~update_last:(true) if_true (fun x -> Node.recompute c);
    Node.add_dependency ~update_last:(true) if_false (fun x -> Node.recompute c);
    c