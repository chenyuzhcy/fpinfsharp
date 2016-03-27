module Chapter04_either

type Either<'e,'a> =
    |Left of 'e
    |Right of 'a

// Ex4.6
let map (f: 'a->'b) (either: Either<'e,'a>): Either<'e,'b> =
    match either with
    | Left(e) -> Left(e)
    | Right(a) -> Right(f a)

let flatMap (f: 'a->Either<'e,'b>) (either: Either<'e,'a>): Either<'e,'b> =
    match either with
    | Left(e) -> Left(e)
    | Right(a) -> f a

let orElse (another: unit -> Either<'e,'a>) (either: Either<'e,'a>): Either<'e,'a> =
    match either with
    | Left(e) -> another()
    | Right(a) -> Right(a)

let map2 (f: 'a->'b->'c) (eb: Either<'e,'b>) (ea: Either<'e,'a>): Either<'e,'c> =
    match (eb, ea) with
    | (Right(b), Right(a)) -> Right(f a b)
    | (Left(ea), _) -> Left(ea)
    | (_, Left(eb)) -> Left(eb)

// Ex4.7
let traverse (list: List<'a>) (f: 'a->Either<'e,'b>): Either<'e, List<'b>> =
    let rec getValid acc list =
        match list with
        | [] -> Right(List.rev acc)
        | h::t ->
            match f h with
            | Left(e) -> Left(e)
            | Right(b) -> getValid (b::acc) t

    getValid [] list

let sequence (es: List<Either<'e,'a>>): Either<'e, List<'a>> =
    traverse es (fun x->x)