module Chapter06_state

type RNG = RNG of (unit -> (int * RNG))
type State<'s, 'a> = ('s -> ('a * 's))
type Rand<'a> = State<RNG,'a>

let unit (a: 'a): State<'s, 'a> =
    fun s -> (a, s)

let map (state: State<'s, 'a>) (f: 'a->'b): State<'s, 'b> =
    fun s ->
        let (a, nextState) = state s
        (f a, nextState)

let map2 (sa: State<'s, 'a>) (sb: State<'s, 'b>) (f: 'a->'b->'c): State<'s, 'c> =
    fun s ->
        let (a, nextState1) = sa s
        let (b, nextState2) = sb nextState1
        (f a b, nextState2)

let flatMap (state: State<'s, 'a>) (g: 'a -> State<'s, 'b>): State<'s, 'b> =
    fun s ->
        let (a, nextState1) = state s
        (g a) nextState1

let rec sequence (ls: List<State<'s, 'a>>): State<'s, List<'a>> =
    fun s ->
        match ls with
        | [] -> ([], s)
        | h::t ->
            let (a, nextState1) = h s
            let (res, nextState2) = (sequence t) nextState1
            (a::res, nextState2)

