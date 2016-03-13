module Chapter03_list

type List<'a> =
    | Nil
    | Cons of 'a * List<'a>

// Ex3.2
let tail (list: List<'a>): List<'a> = 
    match list with
    | Nil -> Nil
    | Cons(_, tail) -> tail

// Ex3.3
let setHead (head: 'a) (list: List<'a>): List<'a> = 
    match list with
    | Nil -> Cons(head, Nil)
    | Cons(_, tail) -> Cons(head, tail)

// Ex3.4
let rec drop (n: int) (list: List<'a>): List<'a> =
    if n<= 0 then list
    else
        match list with
        | Nil -> Nil
        | Cons(_, tail) -> drop (n-1) tail

// Ex3.5
let rec dropWhile (f: 'a->bool) (list: List<'a>): List<'a> =
    match list with
    | Nil -> Nil
    | Cons(h, _) when not (f h) -> list
    | Cons(_, tail) -> dropWhile f tail

// Ex3.6
let rec init (list: List<'a>): List<'a> =
    match list with
    | Nil -> Nil
    | Cons(_, Nil) -> Nil
    | Cons(head, tail) -> Cons(head, init tail)

let rec foldRight (f: 'a->'b->'b) (z: 'b) (list: List<'a>): 'b =
    match list with
    | Nil -> z
    | Cons(head, tail) -> f head (foldRight f z tail)

// Ex 3.9
let length (list: List<'a>): int = list |> foldRight (fun x y -> 1+y) 0

// Ex 3.10
let rec foldLeft (f: 'b->'a->'b) (z: 'b) (list: List<'a>): 'b =
    match list with
    | Nil -> z
    | Cons(head, tail) -> tail |> foldLeft f (f z head)

// Ex 3.11
let sum (list: List<int>): int = list |> foldLeft (fun x y -> x+y) 0
let product (list: List<int>): int = list |> foldLeft (fun x y -> x*y) 1
let length' (list: List<'a>): int = list |> foldLeft (fun x y -> 1+x) 0

// Ex 3.12
let reverse (list: List<'a>): List<'a> = list |> foldLeft (fun x y -> Cons(y, x)) Nil

// ex 3.13


// ex 3.14
let append (a: 'a) (list: List<'a>): List<'a> = 
    list |> foldRight (fun x y -> Cons(x, y)) (Cons(a, Nil))

let concat (la: List<'a>) (lb: List<'a>): List<'a> =
    la |> foldRight (fun x y -> Cons(x, y)) lb

// ex 3.15
let concatLists (lists: List<List<'a>>): List<'a> = 
    lists |> foldLeft concat Nil

// ex 3.16
let addOne (list: List<int>): List<int> =
    list |> foldRight (fun x y -> Cons(x+1, y)) Nil

// ex 3.17
let toString (list: List<double>): List<string> =
    list |> foldRight (fun x y -> Cons(string x, y)) Nil

// ex 3.18
let map (f: 'a->'b) (list: List<'a>): List<'b> =
    list |> foldRight (fun x y -> Cons(f x, y)) Nil

// ex 3.19
let filter (f: 'a->bool) (list: List<'a>): List<'a> =
    list |> foldRight (fun x y -> if f x then Cons(x,y) else y) Nil

// ex 3.20
let flatMap (f: 'a->List<'b>) (list: List<'a>): List<'b> =
    list |> foldRight (fun x y -> concat (f x) y ) Nil

// ex 3.21
let filter' (f: 'a->bool) (list: List<'a>): List<'a> =
    list |> flatMap (fun x -> if f x then Cons(x, Nil) else Nil)

// ex 3.22
let rec pairAdd (la: List<int>) (lb: List<int>): List<int> =
    match (la, lb) with
    | (Nil, _) -> Nil
    | (_, Nil) -> Nil
    | (Cons(h1, t1), Cons(h2, t2)) -> Cons(h1+h2, pairAdd t1 t2)

// ex 3.23
let rec zipWith (la: List<'a>) (lb: List<'b>) (f: 'a->'b->'c): List<'c> =
    match (la, lb) with
    | (Nil, _) -> Nil
    | (_, Nil) -> Nil
    | (Cons(h1, t1), Cons(h2, t2)) -> Cons(f h1 h2, zipWith t1 t2 f)

// ex 3.24
let rec hasSubsequence (sub: List<'a>) (list: List<'a>): bool =
    let rec isStartWith a b =
        match (a,b) with
        | (Nil, _) -> true
        | (_, Nil) -> false
        | (Cons(ah, at), Cons(bh, bt)) -> ah = bh && isStartWith at bt 

    match list with
    | Nil -> sub = Nil
    | Cons(lh, lt) when not (isStartWith sub list) -> hasSubsequence sub lt
    | _ -> true

    
   