module Chapter03_tree

type Tree<'a> =
    | Leaf of 'a
    | Branch of Tree<'a> * Tree<'a>

// ex 3.25
let rec size (tree: Tree<'a>): int =
    match tree with
    | Leaf(_) -> 1
    | Branch(left, right) -> 1 + size(left) + size(right)

// ex 3.26
let rec maximum (tree: Tree<int>): int =
    match tree with
    | Leaf(a) -> a
    | Branch(left, right) -> max (maximum left) (maximum right)

// ex 3.27
let rec depth (tree: Tree<'a>): int =
    match tree with
    | Leaf(_) -> 1
    | Branch(left, right) ->1 + max (depth left) (depth right)

// ex 3.28
let rec map (f: 'a ->'b) (tree: Tree<'a>): Tree<'b> =
    match tree with
    | Leaf(a) -> Leaf(f a)
    | Branch(left, right) -> Branch(map f left, map f right)

// ex 3.29
let rec fold (f: 'a->'b) (g: 'b->'b->'b) (tree: Tree<'a>): 'b =
    match tree with
    | Leaf(a) -> f a
    | Branch(left, right) -> g (fold f g left) (fold f g right)

let size' (tree: Tree<'a>): int =
    tree |> fold (fun x -> 1) (fun x y -> 1 + x + y)

let maximum' (tree: Tree<int>): int =
    tree |> fold (fun x-> x) (fun x y -> max x y)

let depth' (tree: Tree<'a>): int=
    tree |> fold (fun x->1) (fun x y -> 1+ max x y)

let map' (f: 'a->'b) (tree: Tree<'a>): Tree<'b> =
    tree |> fold (fun x-> Leaf(f x)) (fun x y -> Branch(x, y))
