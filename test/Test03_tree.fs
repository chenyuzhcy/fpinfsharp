module Test03_tree

open Xunit
open Swensen.Unquote
open Chapter03_tree

// Ex3.25
[<Fact>]
let ``Ex 3.25``() =
    let left = Branch(Leaf(1), Leaf(2))
    let right = Branch(Leaf(3), Branch(Leaf(4), Leaf(5)))
    let result = Branch(left, right) |> size
    test <@ result = 9 @>

// Ex3.26
[<Fact>]
let ``Ex 3.26``() =
    let left = Branch(Leaf(1), Leaf(2))
    let right = Branch(Leaf(3), Branch(Leaf(4), Leaf(5)))
    let result = Branch(left, right) |> maximum
    test <@ result = 5 @>

// Ex3.27
[<Fact>]
let ``Ex 3.27``() =
    let left = Branch(Leaf(1), Leaf(2))
    let right = Branch(Leaf(3), Branch(Leaf(4), Leaf(5)))
    let result = Branch(left, right) |> depth
    test <@ result = 4 @>

// Ex3.28
[<Fact>]
let ``Ex 3.28``() =
    let left = Branch(Leaf(1), Leaf(2))
    let right = Branch(Leaf(3), Branch(Leaf(4), Leaf(5)))
    let result = Branch(left, right) |> map (fun x-> string (x+1))
    let expectLeft = Branch(Leaf("2"), Leaf("3"))
    let expectRight = Branch(Leaf("4"), Branch(Leaf("5"), Leaf("6")))
    test <@ result = Branch(expectLeft, expectRight) @>

// Ex3.29
[<Fact>]
let ``Ex 3.29 size``() =
    let left = Branch(Leaf(1), Leaf(2))
    let right = Branch(Leaf(3), Branch(Leaf(4), Leaf(5)))
    let result = Branch(left, right) |> size'
    test <@ result = 9 @>
[<Fact>]
let ``Ex 3.29 maximum``() =
    let left = Branch(Leaf(1), Leaf(2))
    let right = Branch(Leaf(3), Branch(Leaf(4), Leaf(5)))
    let result = Branch(left, right) |> maximum'
    test <@ result = 5 @>
[<Fact>]
let ``Ex 3.29 depth``() =
    let left = Branch(Leaf(1), Leaf(2))
    let right = Branch(Leaf(3), Branch(Leaf(4), Leaf(5)))
    let result = Branch(left, right) |> depth'
    test <@ result = 4 @>
[<Fact>]
let ``Ex 3.29 map``() =
    let left = Branch(Leaf(1), Leaf(2))
    let right = Branch(Leaf(3), Branch(Leaf(4), Leaf(5)))
    let result = Branch(left, right) |> map' (fun x-> string (x+1))
    let expectLeft = Branch(Leaf("2"), Leaf("3"))
    let expectRight = Branch(Leaf("4"), Branch(Leaf("5"), Leaf("6")))
    test <@ result = Branch(expectLeft, expectRight) @>

