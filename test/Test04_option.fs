module Test04_option

open Xunit
open Swensen.Unquote
open Chapter04_option

// Ex4.2
[<Fact>]
let ``Ex 4.02``() =
    let seq = seq { for i in 0.0..9.0 do yield i}
    let result = seq |> variance
    test <@ result = Some(8.25) @>
