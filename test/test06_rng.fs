module test06_rng

open Chapter06_rng

let rng = SimpleRNG(42L)

// Ex 6.1
let (n1, rng2) = rng |> nonNegativeInt
let (n2, rng3) = rng2 |> nonNegativeInt

// Ex 6.2
let (d1, rng_d1) = rng |> nextDouble
let (d2, rng_d2) = rng_d1 |> nextDouble

// Ex 6.3
let ((id1, id2), rng_intDouble) = rng |> intDouble
let ((di1, di2), rng_doubleInt) = rng |> doubleInt
let ((d31, d32, d33), rng_double3) = rng |> double3

// Ex 6.4
let (list, rng_list) = rng |> ints 3

// Ex 6.5
let (d1', rng_d1') = rng |> nextDouble'
let (d2', rng_d2') = rng_d1' |> nextDouble'

// Ex 6.6
let ((id1', id2'), rng_intDouble') = rng |> intDouble'
let ((di1', di2'), rng_doubleInt') = rng |> doubleInt'

// Ex 6.7
let (list', rng_list') = rng |> ints' 3

// Ex 6.8
let (nn1, rng_nn1) = rng |> nonNegativeLessThan 5
let (nn2, rng_nn2) = rng_nn1 |> nonNegativeLessThan 5
let (nn3, rng_nn3) = rng_nn2 |> nonNegativeLessThan 5
let (nn4, rng_nn4) = rng_nn3 |> nonNegativeLessThan 5
let (nn5, rng_nn5) = rng_nn4 |> nonNegativeLessThan 5

// Ex 6.9
let nextDouble2' = map' (nextInt) (fun i->(double i)/(double System.Int32.MaxValue + 1.0))

let (nextdb1', rng2_d1') = rng |> nextDouble2'
let (nextdb2', rng3_d2') = rng2_d1' |> nextDouble2'


