module Test03_list

open Xunit
open Swensen.Unquote
open Chapter03_list

// Ex3.2
[<Fact>]
let ``Ex 3.02``() =
    let result = Cons(1, Cons(2, Cons(3, Nil))) |> tail
    test <@ result = Cons(2, Cons(3, Nil)) @>

// Ex3.3
[<Fact>]
let ``Ex 3.03``() =
    let result = Cons(1, Cons(2, Cons(3, Nil))) |> setHead 7
    test <@ result = Cons(7, Cons(2, Cons(3, Nil))) @>

// Ex3.4
[<Fact>]
let ``Ex 3.04``() =
    let result = Cons(1, Cons(2, Cons(3, Nil))) |> drop 2
    test <@ result = Cons(3, Nil) @>

// Ex3.5
[<Fact>]
let ``Ex 3.05``() =
    let result = Cons(1, Cons(2, Cons(3, Nil))) |> dropWhile (fun x -> x<2)
    test <@ result = Cons(2, Cons(3, Nil)) @>

// Ex3.6
[<Fact>]
let ``Ex 3.06``() =
    let result = Cons(1, Cons(2, Cons(3, Nil))) |> init
    test <@ result = Cons(1, Cons(2, Nil)) @>

// Ex3.9
[<Fact>]
let ``Ex 3.09``() =
    let result = Cons(1, Cons(2, Cons(3, Nil))) |> length
    test <@ result = 3 @>

// Ex3.11
[<Fact>]
let ``Ex 3.11 sum``() =
    let result = Cons(1, Cons(2, Cons(3, Nil))) |> sum
    test <@ result = 6 @>
[<Fact>]
let ``Ex 3.11 product``() =
    let result = Cons(1, Cons(2, Cons(3, Cons(4, Nil)))) |> product
    test <@ result = 24 @>
[<Fact>]
let ``Ex 3.11 length``() =
    let result = Cons(1, Cons(2, Cons(3, Nil))) |> length'
    test <@ result = 3 @>

// Ex3.12
[<Fact>]
let ``Ex 3.12``() =
    let result = Cons(1, Cons(2, Cons(3, Nil))) |> reverse
    test <@ result = Cons(3, Cons(2, Cons(1, Nil))) @>

// Ex3.14
[<Fact>]
let ``Ex 3.14``() =
    let result = Cons(1, Cons(2, Cons(3, Nil))) |> append 4
    test <@ result = Cons(1, Cons(2, Cons(3, Cons(4, Nil)))) @>

// Ex3.15
[<Fact>]
let ``Ex 3.15``() =
    let list1 = Cons(1, Cons(2, Nil))
    let list2 = Cons(3, Cons(4, Cons(5, Nil)))
    let result = Cons(list1, Cons(list2, Nil)) |> concatLists
    test <@ result =  Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil))))) @>

// Ex3.16
[<Fact>]
let ``Ex 3.16``() =
    let result = Cons(1, Cons(2, Cons(3, Nil))) |> addOne
    test <@ result = Cons(2, Cons(3, Cons(4, Nil))) @>

// Ex3.17
[<Fact>]
let ``Ex 3.17``() =
    let result = Cons(1.1, Cons(2.2, Cons(3.3, Nil))) |> toString
    test <@ result = Cons("1.1", Cons("2.2", Cons("3.3", Nil))) @>

// Ex3.18
[<Fact>]
let ``Ex 3.18``() =
    let result = Cons(1, Cons(2, Cons(3, Nil))) |> map (fun x -> x+2)
    test <@ result = Cons(3, Cons(4, Cons(5, Nil))) @>

// Ex3.19
[<Fact>]
let ``Ex 3.19``() =
    let result = Cons(1, Cons(2, Cons(3, Nil))) |> filter (fun x -> x%2 = 0)
    test <@ result = Cons(2, Nil) @>

// Ex3.20
[<Fact>]
let ``Ex 3.20``() =
    let result = Cons(1, Cons(2, Cons(3, Nil))) |> flatMap (fun x -> Cons(x, Cons(x+1, Nil)))
    test <@ result = Cons(1, Cons(2, Cons(2, Cons(3, Cons(3, Cons(4, Nil)))))) @>

// Ex3.21
[<Fact>]
let ``Ex 3.21``() =
    let result = Cons(1, Cons(2, Cons(3, Nil))) |> filter' (fun x -> x%2 = 0)
    test <@ result = Cons(2, Nil) @>

// Ex3.22
[<Fact>]
let ``Ex 3.22``() =
    let result = pairAdd (Cons(1, Cons(2, Cons(3, Nil)))) (Cons(4, Cons(5, Cons(6, Nil))))
    test <@ result = Cons(5, Cons(7, Cons(9, Nil))) @>

// Ex3.23
[<Fact>]
let ``Ex 3.23``() =
    let list1 = Cons(1, Cons(2, Cons(3, Nil)))
    let list2 = Cons(4, Cons(5, Cons(6, Nil)))
    let result = zipWith list1 list2 (fun x y-> x*y )
    test <@ result = Cons(4, Cons(10, Cons(18, Nil))) @>

// Ex3.24
[<Fact>]
let ``Ex 3.24 test 1``() =
    let list1 = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    let list2 = Cons(1, Cons(2, Nil))
    let result = list1 |> hasSubsequence list2
    test <@ result = true @>
[<Fact>]
let ``Ex 3.24 test 2``() =
    let list1 = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    let list2 = Cons(2, Cons(3, Nil))
    let result = list1 |> hasSubsequence list2
    test <@ result = true @>
[<Fact>]
let ``Ex 3.24 test 3``() =
    let list1 = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    let list2 = Cons(3, Cons(4, Nil))
    let result = list1 |> hasSubsequence list2
    test <@ result = true @>
[<Fact>]
let ``Ex 3.24 test 4``() =
    let list1 = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    let list2 = Cons(2, Cons(4, Nil))
    let result = list1 |> hasSubsequence list2
    test <@ result = false @>