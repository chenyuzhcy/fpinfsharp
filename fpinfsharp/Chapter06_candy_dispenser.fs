module Chapter06_candy_dispenser

open Chapter06_state

type Input = 
    | Coin
    | Turn
type Machine = Machine of (bool*int*int)

let updateMachine (machine: Machine) (input: Input): Machine =
    match (input, machine) with
    | (_, Machine(_,0,_)) -> machine
    | (Coin, Machine(true, candies, coins)) -> Machine(false, candies, coins+1)
    | (Turn, Machine(false, candies, coins)) -> Machine(true, candies-1, coins)
    | (Coin, Machine(false, _, _)) -> machine
    | (Turn, Machine(true,_,_)) -> machine

let simulateMachine (inputs: List<Input>): State<Machine, (int*int)> =
    fun machine ->
        let final = inputs |> List.fold (fun state i -> updateMachine state i) machine
        match final with
        | Machine(locked, candies, coins) -> ((candies, coins), final)


let m = Machine(true, 5, 10)
let results = (simulateMachine [Coin; Turn; Coin; Turn]) m
