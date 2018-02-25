module rec Kind : sig 
    type 'a t =
    | Variable of 'a
    | Map: (('a0 -> 'a) * 'a0 Node.t) -> 'a t
    | Map2: ('a0 -> 'a1 -> 'a) * 'a0 Node.t * 'a1 Node.t -> 'a t
    | Map3: ('a0 -> 'a1 -> 'a2 -> 'a) * 'a0 Node.t * 'a1 Node.t * 'a2 Node.t -> 'a t
    | Map4: ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a) * 'a0 Node.t * 'a1 Node.t * 'a2 Node.t * 'a3 Node.t -> 'a t
    val value: 'a t -> 'a
    val to_string: 'a t -> string
end = struct
    type 'a t =
    | Variable of 'a
    | Map: (('a0 -> 'a) * 'a0 Node.t) -> 'a t
    | Map2: ('a0 -> 'a1 -> 'a) * 'a0 Node.t * 'a1 Node.t -> 'a t
    | Map3: ('a0 -> 'a1 -> 'a2 -> 'a) * 'a0 Node.t * 'a1 Node.t * 'a2 Node.t -> 'a t
    | Map4: ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a) * 'a0 Node.t * 'a1 Node.t * 'a2 Node.t * 'a3 Node.t -> 'a t

    let to_string t =
        match t with
        | Variable _ -> "Variable"
        | Map _ -> "Map"
        | Map2 _ -> "Map2"
        | Map3 _ -> "Map3"
        | Map4 _ -> "Map4"

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
    let create kind () = {
        value =  Some (Kind.value kind);
        kind;
        dependencies = []
    }
    let read t = t.value

    exception EmptyNode
    let read_exn t = match read t with
        | Some z -> z
        | None -> raise EmptyNode
   
    let set_variable t r =
        match t.kind with
        | Variable _ -> t.kind <- Variable r; t.value <- Some r
        | _ -> t.value <- Some r
    let write t v = 
        if t.value != (Some v) then
            set_variable t v;
            List.iter (fun f -> f ()) t.dependencies

    let add_dependency t dep = t.dependencies <- dep::t.dependencies
    let recompute t =
        write t @@ Kind.value t.kind

end

let x0 = Node.create (Kind.Variable 0) ()
let x = Node.create (Kind.Map ((fun x-> x+1), x0)) ()

let read a = Node.read a

let read_exn a = Node.read_exn a


let set_value t v = Node.write t v
let return v = Node.create (Kind.Variable v) ()

let make_variable v = return v
let map n1 ~f = 
    let b = Node.create (Kind.Map (f,n1)) () in
    Node.add_dependency n1 (fun () -> Node.recompute b);
    b
let map2 n1 n2 ~f = 
    let c = Node.create (Kind.Map2 (f, n1, n2)) () in
    Node.add_dependency n1 (fun () -> Node.recompute c);
    Node.add_dependency n2 (fun () -> Node.recompute c);
    c

let map3 n1 n2 n3 ~f =
    let d = Node.create (Kind.Map3 (f, n1, n2, n3)) () in
    Node.add_dependency n1 (fun () -> Node.recompute d);
    Node.add_dependency n2 (fun () -> Node.recompute d);
    Node.add_dependency n3 (fun () -> Node.recompute d);
    d

let map4 n1 n2 n3 n4 ~f =
    let e = Node.create (Kind.Map4 (f, n1, n2, n3, n4)) () in
    Node.add_dependency n1 (fun () -> Node.recompute e);
    Node.add_dependency n2 (fun () -> Node.recompute e);
    Node.add_dependency n3 (fun () -> Node.recompute e);
    Node.add_dependency n4 (fun () -> Node.recompute e);
    e