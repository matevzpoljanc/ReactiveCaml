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
    | Unordered_list_fold: ('a -> 'a0 -> 'a) * ('a -> 'a0 -> 'a) * 'a Node.t * ('a0 Node.t list) -> 'a t
    
    val value: 'a t -> 'a node_value
    val value_list_fold: 'a t -> old_node:'b -> new_node:'b -> old_result:'a -> 'a node_value

end = struct
    type 'a t =
    | Variable of 'a
    | Map: (('a0 -> 'a) * 'a0 Node.t) -> 'a t
    | Map2: ('a0 -> 'a1 -> 'a) * 'a0 Node.t * 'a1 Node.t -> 'a t
    | Map3: ('a0 -> 'a1 -> 'a2 -> 'a) * 'a0 Node.t * 'a1 Node.t * 'a2 Node.t -> 'a t
    | Map4: ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a) * 'a0 Node.t * 'a1 Node.t * 'a2 Node.t * 'a3 Node.t -> 'a t
    | Unordered_list_fold: ('a -> 'a0 -> 'a) * ('a -> 'a0 -> 'a) * 'a Node.t * ('a0 Node.t list) -> 'a t 
    
    (** Function is called whenever node recomputes *)
    let value t =
        match t with
        | Variable v -> Value v
        | Map (f, n1) -> (try Value (f @@ Node.read_exn n1) with e -> Err e)
        | Map2 (f, n1, n2) -> (try Value (f (Node.read_exn n1) (Node.read_exn n2)) with e -> Err e)
        | Map3 (f, n1, n2, n3) -> (try Value (f (Node.read_exn n1) (Node.read_exn n2) (Node.read_exn n3)) with e -> Err e)
        | Map4 (f, n1, n2, n3, n4) -> (try Value (f (Node.read_exn n1) (Node.read_exn n2) (Node.read_exn n3) (Node.read_exn n4)) with e -> Err e)
        | Unordered_list_fold (f, f_inv, init, l) -> (try Value (List.fold_left f (Node.read_exn init) @@ List.map (Node.read_exn) l) with e -> Err e)
    
    exception NodeNotListFold
    let value_list_fold t ~old_node ~new_node ~old_result =
        match t with
        | Unordered_list_fold (f, f_inv, _, _) -> (try Value (f (f_inv old_result (Obj.magic old_node)) (Obj.magic new_node)) with e -> Err e)
        | _ -> raise NodeNotListFold
end
and Node: sig 
    type 'a t = {
        mutable value : 'a node_value option;
        mutable kind: 'a Kind.t;
        mutable dependencies: ('a * 'a -> unit) list
    }
    val create: 'a Kind.t -> unit -> 'a t
    val read: 'a t -> 'a option
    val read_exn: 'a t -> 'a
    val write: 'a t -> 'a -> unit
    val add_dependency: 'a t -> ('a * 'a -> unit) -> unit
    val recompute: 'a t -> unit
    val recompute_fold: 'a t -> old: 'b -> new_v: 'b -> unit
    val update_entire_node: 'a t -> 'a t -> unit
end = struct 
     type 'a t = {
        mutable value : 'a node_value option;
        mutable kind: 'a Kind.t;
        mutable dependencies: ('a * 'a -> unit) list
    }
    (** Create new node of a kind and initialize it's value  *)    
    let create kind () = {
        value =  Some (Kind.value kind);
        kind;
        dependencies = []
    }

    (** Read node's value *)
    let read t = match t.value with
        | Some nv -> Some (unpack_node_value nv)
        | None -> None 

    exception EmptyNode
    (** Read node's value. If value is None then raise EmptyNode exception *)
    let read_exn t = match read t with
        | Some z -> z
        | None -> raise EmptyNode
   
    (** Update value of the node. If node is of kind Variable then also update value of Kind *)
    let update_value t r =
        match t.kind with
        | Variable _ -> t.kind <- Variable r; t.value <- Some (Value r)
        | _ -> t.value <- Some (Value r)
    (** Write new value into node. If node's value actually change then call recompute on all of it's dependencies *)
    let write t v = 
        let old_value = read_exn t in
        if old_value != v then
            update_value t v;
            List.iter (fun f -> f (old_value, v)) t.dependencies
    let update_entire_node t0 t1 =
        t0.value <- t1.value; t0.kind <- t1.kind 

    (** Add new dependency for a node. Dependency is a function which when called recomputes some other node *)
    let add_dependency t dep = t.dependencies <- dep::t.dependencies
    (** Function used to recompute and update current node's value *)
    let recompute t =
        write t @@ unpack_node_value @@ Kind.value t.kind

    let recompute_fold t ~old ~new_v = 
        write t @@ unpack_node_value @@ Kind.value_list_fold t.kind ~old_node:old ~new_node:new_v ~old_result:(read_exn t)

end
type 'a t = 'a Node.t
(** Read value option of the node *)
let read a = Node.read a

(** Read the value of the node *)
let read_exn a = Node.read_exn a

(** Set the value of a node *)
let set_value t v = Node.write t v
(** Return node representation of a value *)
let return v = Node.create (Kind.Variable v) ()
(** Same as return with clearer name *)
let make_variable v = return v

(** Map a function over a node *)
let map n1 ~f = 
    let b = Node.create (Kind.Map (f,n1)) () in
    Node.add_dependency n1 (fun x1 -> Node.recompute b);
    b

let bind n1 ~f =
    let b = f @@ read_exn n1 in
    Node.add_dependency n1 (fun x -> let b1 = f @@ read_exn n1 in Node.update_entire_node b b1);
    b
(** Map two argument function over two nodes *)
let map2 n1 n2 ~f = 
    let c = Node.create (Kind.Map2 (f, n1, n2)) () in
    Node.add_dependency n1 (fun x1 -> Node.recompute c);
    Node.add_dependency n2 (fun x2 -> Node.recompute c);
    c

let bind2 n1 n2 ~f =
    let b = f (read_exn n1) (read_exn n2) in
    Node.add_dependency n1 (fun x -> let b1 = f (read_exn n1) (read_exn n2) in Node.update_entire_node b b1);
    Node.add_dependency n2 (fun x -> let b1 = f (read_exn n1) (read_exn n2) in Node.update_entire_node b b1);
    b

(** Map three argument function over three nodes *)
let map3 n1 n2 n3 ~f =
    let d = Node.create (Kind.Map3 (f, n1, n2, n3)) () in
    Node.add_dependency n1 (fun x1 -> Node.recompute d);
    Node.add_dependency n2 (fun x2 -> Node.recompute d);
    Node.add_dependency n3 (fun x3 -> Node.recompute d);
    d

let bind3 n1 n2 n3 ~f =
    let b = f (read_exn n1) (read_exn n2) (read_exn n3) in
    Node.add_dependency n1 (fun x -> let b1 = f (read_exn n1) (read_exn n2) (read_exn n3) in Node.update_entire_node b b1);
    Node.add_dependency n2 (fun x -> let b1 = f (read_exn n1) (read_exn n2) (read_exn n3) in Node.update_entire_node b b1);
    Node.add_dependency n3 (fun x -> let b1 = f (read_exn n1) (read_exn n2) (read_exn n3) in Node.update_entire_node b b1);
    b

(** Map four argument function over four nodes *)
let map4 n1 n2 n3 n4 ~f =
    let e = Node.create (Kind.Map4 (f, n1, n2, n3, n4)) () in
    Node.add_dependency n1 (fun x1 -> Node.recompute e);
    Node.add_dependency n2 (fun x2 -> Node.recompute e);
    Node.add_dependency n3 (fun x3 -> Node.recompute e);
    Node.add_dependency n4 (fun x4 -> Node.recompute e);
    e

let bind4 n1 n2 n3 n4 ~f =
    let b = f (read_exn n1) (read_exn n2) (read_exn n3) (read_exn n4) in
    Node.add_dependency n1 (fun x -> let b1 = f (read_exn n1) (read_exn n2) (read_exn n3) (read_exn n4) in Node.update_entire_node b b1);
    Node.add_dependency n2 (fun x -> let b1 = f (read_exn n1) (read_exn n2) (read_exn n3) (read_exn n4) in Node.update_entire_node b b1);
    Node.add_dependency n3 (fun x -> let b1 = f (read_exn n1) (read_exn n2) (read_exn n3) (read_exn n4) in Node.update_entire_node b b1);
    Node.add_dependency n4 (fun x -> let b1 = f (read_exn n1) (read_exn n2) (read_exn n3) (read_exn n4) in Node.update_entire_node b b1);
    b

let unordered_list_fold ~f ~f_inv ~init l = 
    let r = Node.create (Kind.Unordered_list_fold (f,f_inv,init,l)) () in
    Node.add_dependency init (fun x1 -> Node.recompute r);
    List.iter (fun n -> Node.add_dependency n (fun (old,new_v) -> Node.recompute_fold r ~old ~new_v)) l;
    r
