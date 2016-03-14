module Chapter06_rng

open System

type RNG = RNG of (unit -> (int * RNG))
type Rand<'a> = (RNG -> ('a*RNG))

let rec SimpleRNG (seed: int64): RNG =
    let genNext (): (int * RNG) = 
        let newSeed = (seed*0x5DEECE66DL + 0xBL) &&& ((1L <<< 48) - 1L)
        let nextRNG = SimpleRNG newSeed
        let nextVal = (newSeed >>> 16) |> int 
        (nextVal, nextRNG)
    RNG genNext


let nextInt (rng: RNG):(int * RNG) =
    match rng with
    | RNG nextInt -> nextInt()


// Ex 6.1
let nonNegativeInt (rng: RNG): (int*RNG) =
    let (value, newRng) = rng |> nextInt
    if value < 0 then (-(value+1), newRng)
    else (value, newRng)

// Ex 6.2
let nextDouble (rng: RNG): (double*RNG) =
    let (value, newRng) = rng |> nonNegativeInt
    ((double value)/(double System.Int32.MaxValue + 1.0), newRng)

// Ex 6.3
let intDouble (rng: RNG): ((int*double)*RNG) =
    let (intValue, rng2) = rng |> nextInt
    let (doubleValue, rng3) = rng2 |> nextDouble
    ((intValue, doubleValue), rng3)

let doubleInt (rng: RNG): ((double*int)*RNG) =
    let (doubleValue, rng2) = rng |> nextDouble
    let (intValue, rng3) = rng2 |> nextInt
    ((doubleValue, intValue), rng3)

let double3 (rng: RNG): ((double*double*double)*RNG) =
    let (d1, rng1) = rng |> nextDouble
    let (d2, rng2) = rng1 |> nextDouble
    let (d3, rng3) = rng2 |> nextDouble
    ((d1,d2,d3), rng3)

// Ex 6.4
let rec ints (count: int) (rng: RNG): (List<int> * RNG) =
    if count<=0 then ([], rng)
    else 
        let (intValue, rng2) = rng |> nextInt
        let (restInts, rng3) = rng2 |> ints (count-1)
        (intValue::restInts, rng3)

// Ex 6.5
let map (s: Rand<'a>) (f: 'a->'b): Rand<'b> =
    fun rng ->
        let (a, rng') = s rng
        (f a, rng')

let nextDouble' = map (nextInt) (fun i->(double i)/(double System.Int32.MaxValue + 1.0))

// Ex 6.6
let map2 (ra: Rand<'a>) (rb: Rand<'b>) (f: 'a->'b->'c): Rand<'c> =
    fun rng ->
        let (va, rng1) = ra rng
        let (vb, rng2) = rb rng1
        (f va vb, rng2)

let intDouble' = map2 nextInt nextDouble (fun a b -> (a, b))
let doubleInt' = map2 nextDouble nextInt (fun a b -> (a, b))

// Ex 6.7
let rec sequence (fs: List<Rand<'a>>): Rand<List<'a>> =
    fun rng ->
        match fs with
        | [] -> ([], rng)
        | h::t -> 
            let (val_h, rng1) = h rng
            let (val_list, rng2) = sequence t rng1
            (val_h::val_list, rng2)

let ints' (count: int) (rng: RNG): (List<int> * RNG) =
    rng |> sequence (List.replicate count nextInt)

// Ex 6.8
let flatMap (f: Rand<'a>) (g: 'a-> Rand<'b>): Rand<'b> =
    fun rng ->
        let (value, rng1) = f rng
        (g value) rng1

let rec nonNegativeLessThan (n: int): Rand<int> =
    let choose (i: int): Rand<int> =
        let m = i%n
        if i + (n-1) - m >=0 then
            fun rng -> (m, rng)
        else nonNegativeLessThan n

    flatMap nonNegativeInt choose

// Ex 6.9
let map' (s: Rand<'a>) (f: 'a->'b): Rand<'b> =
    flatMap s (fun a-> fun rng -> (f a, rng))

let map2' (ra: Rand<'a>) (rb: Rand<'b>) (f: 'a->'b->'c): Rand<'c> =
    let func = fun a -> 
        fun rng -> 
            let (b, rng1) = rb rng
            (f a b, rng1)
    flatMap ra func