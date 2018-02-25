(** In this library ddg is represented with nodes which are connected via dependency relation. Each node is of a certain kind which is either
Variable or Map. Variable is used to store changable values and Map is used to perform computation on Values.  *)
module rec Kind : sig 
    type 'a t =
    | Variable of 'a
    | Map: (('a0 -> 'a) * 'a0 Node.t) -> 'a t
    | Map2: ('a0 -> 'a1 -> 'a) * 'a0 Node.t * 'a1 Node.t -> 'a t
    | Map3: ('a0 -> 'a1 -> 'a2 -> 'a) * 'a0 Node.t * 'a1 Node.t * 'a2 Node.t -> 'a t
    | Map4: ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a) * 'a0 Node.t * 'a1 Node.t * 'a2 Node.t * 'a3 Node.t -> 'a t
    val value: 'a t -> 'a
end = struct
    type 'a t =
    | Variable of 'a
    | Map: (('a0 -> 'a) * 'a0 Node.t) -> 'a t
    | Map2: ('a0 -> 'a1 -> 'a) * 'a0 Node.t * 'a1 Node.t -> 'a t
    | Map3: ('a0 -> 'a1 -> 'a2 -> 'a) * 'a0 Node.t * 'a1 Node.t * 'a2 Node.t -> 'a t
    | Map4: ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a) * 'a0 Node.t * 'a1 Node.t * 'a2 Node.t * 'a3 Node.t -> 'a t
(** Function is whenever node recomputes *)
    let value t =
        match t with
        | Variable v -> v
        | Map (f, n1) -> f @@ Node.read_exn n1
        | Map2 (f, n1, n2) -> f (Node.read_exn n1) (Node.read_exn n2)
        | Map3 (f, n1, n2, n3) -> f (Node.read_exn n1) (Node.read_exn n2) (Node.read_exn n3)
        | Map4 (f, n1, n2, n3, n4) -> f (Node.read_exn n1) (Node.read_exn n2) (Node.read_exn n3) (Node.read_exn n4)
end
and Node: sig 
    type 'a t = {
        mutable value : 'a option;
        mutable kind: 'a Kind.t;
        mutable dependencies: (unit -> unit) list
    }
    val create: 'a Kind.t -> unit -> 'a t
    val read: 'a t -> 'a option
    val read_exn: 'a t -> 'a
    val write: 'a t -> 'a -> unit
    val add_dependency: 'a t -> (unit -> unit) -> unit
    val recompute: 'a t -> unit
end = struct 
     type 'a t = {
        mutable value : 'a option;
        mutable kind: 'a Kind.t;
        mutable dependencies: (unit -> unit) list
    }
    (** Create new node of a kind and initialize it's value  *)    
    let create kind () = {
        value =  Some (Kind.value kind);
        kind;
        dependencies = []
    }
    (** Read node's value *)
    let read t = t.value

    exception EmptyNode
    (** Read node's value. If value is None then raise EmptyNode exception *)
    let read_exn t = match read t with
        | Some z -> z
        | None -> raise EmptyNode
   
    (** Update value of the node. If node is of kind Variable then also update value of Kind *)
    let update_value t r =
        match t.kind with
        | Variable _ -> t.kind <- Variable r; t.value <- Some r
        | _ -> t.value <- Some r
    (** Write new value into node. If node's value actually change then call recompute on all of it's dependencies *)
    let write t v = 
        if t.value != (Some v) then
            update_value t v;
            List.iter (fun f -> f ()) t.dependencies

    (** Add new dependency for a node. Dependency is a function which when called recomputes some other node *)
    let add_dependency t dep = t.dependencies <- dep::t.dependencies
    (** Function used to recompute and update current node's value *)
    let recompute t =
        write t @@ Kind.value t.kind

end

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
    Node.add_dependency n1 (fun () -> Node.recompute b);
    b

(** Map two argument function over two nodes *)
let map2 n1 n2 ~f = 
    let c = Node.create (Kind.Map2 (f, n1, n2)) () in
    Node.add_dependency n1 (fun () -> Node.recompute c);
    Node.add_dependency n2 (fun () -> Node.recompute c);
    c

(** Map three argument function over three nodes *)
let map3 n1 n2 n3 ~f =
    let d = Node.create (Kind.Map3 (f, n1, n2, n3)) () in
    Node.add_dependency n1 (fun () -> Node.recompute d);
    Node.add_dependency n2 (fun () -> Node.recompute d);
    Node.add_dependency n3 (fun () -> Node.recompute d);
    d

(** Map four argument function over four nodes *)
let map4 n1 n2 n3 n4 ~f =
    let e = Node.create (Kind.Map4 (f, n1, n2, n3, n4)) () in
    Node.add_dependency n1 (fun () -> Node.recompute e);
    Node.add_dependency n2 (fun () -> Node.recompute e);
    Node.add_dependency n3 (fun () -> Node.recompute e);
    Node.add_dependency n4 (fun () -> Node.recompute e);
    e