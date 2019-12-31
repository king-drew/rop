// Learn more about F# at http://fsharp.org

open System

type Result<'TSuccess, 'TFailure> = 
    | Success of 'TSuccess
    | Failure of 'TFailure

let bind switchFn =
    function
    | Success s -> switchFn s
    | Failure f -> Failure f

let (>>=) twoTrackInput switchFunction =
    bind switchFunction twoTrackInput

let (>=>) switch1 switch2 x =
    match switch1 x with
    | Success s -> switch2 s
    | Failure f -> Failure f

type Request = {name: string; email: string}

let validate1 input =
    if input.name = "" then Failure "Name must not be blank"
    else Success input

let validate2 input =
    if input.name.Length > 50 then Failure "Name must not be more than 50 chars"
    else Success input

let validate3 input =
    if input.email = "" then Failure "Email must not be blank"
    else Success input

let combinedValidationDataOriented x =
    x
    |> validate1
    >>= validate2
    >>= validate3

let combinedValidation =
    validate1
    >=> validate2
    >=> validate3

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"

    // test 1
    let input1 = {name=""; email=""}
    combinedValidation input1 
    |> printfn "Result1=%A"

    // ==> Result1=Failure "Name must not be blank"

    // test 2
    let input2 = {name="Alice"; email=""}
    combinedValidation input2
    |> printfn "Result2=%A"

    // ==> Result2=Failure "Email must not be blank"

    // test 3
    let input3 = {name="Alice"; email="good"}
    combinedValidation input3
    |> printfn "Result3=%A"

    // ==> Result3=Success {name = "Alice"; email = "good";}

    0 // return an integer exit code
