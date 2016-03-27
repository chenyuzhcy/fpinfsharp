module Chapter04_option

open System

type Option<'a> =
    | None
    | Some of 'a

// Ex4.1

let map (f: 'a->'b) (opt: Option<'a>): Option<'b> =
    match opt with
    | None -> None
    | Some(a) -> Some(f a)

let flatMap (f: 'a->Option<'b>) (opt: Option<'a>): Option<'b> =
    match opt with
    | None -> None
    | Some(a) -> f a

let getOrElse (defaultValue: 'a) (opt: Option<'a>): 'a =
    match opt with
    | None -> defaultValue
    | Some(a) -> a

let orElse (opt2: Option<'a>) (opt: Option<'a>): Option<'a> =
    match opt with
    | None -> opt2
    | Some(a) -> Some(a)

let filter (f: 'a->bool) (opt: Option<'a>): Option<'a> =
    match opt with
    | Some(a) when f a -> Some(a)
    | _ -> None   

// Ex4.2
let variance (xs: seq<double>): Option<double> =
    let mean seq =
        if Seq.isEmpty seq then None
        else Some(Seq.sum seq/double(Seq.length seq))

    let m = mean xs

    m |> flatMap (fun m -> mean (xs |>Seq.map (fun x -> Math.Pow(x-m, 2.0))))

// Ex4.3
let map2 (oa: Option<'a>) (ob: Option<'b>) (f: 'a->'b->'c): Option<'c> =
    match (oa, ob) with
    | (Some(a), Some(b)) -> Some (f a b)
    | _ -> None

// Ex4.4
let sequence (optList: List<Option<'a>>): Option<List<'a>> =
    let rec getValid acc list =
        match list with
        | [] -> Some(List.rev acc)
        | h::t ->
            match h with
            | None -> None
            | Some(a) -> getValid (a::acc) t

    getValid [] optList

// Ex4.5
let traverse (list: List<'a>) (f: 'a->Option<'b>): Option<List<'b>> =
    let rec getValid acc list =
        match list with
        | [] -> Some(List.rev acc)
        | h::t ->
            match f h with
            | None -> None
            | Some(b) -> getValid (b::acc) t

    getValid [] list

let sequence' (optList: List<Option<'a>>): Option<List<'a>> = 
    traverse optList (fun x->x)