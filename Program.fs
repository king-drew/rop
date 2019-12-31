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

let (>=>) switch1 switch2 =
    switch1 >> (bind switch2)

// convert normal function to a switch function
let switch f x =
    f x |> Success

let map oneTrackFunc twoTrackInput =
    match twoTrackInput with
    | Success s -> Success (oneTrackFunc s)
    | Failure f -> Failure f

let tee f x =
    f x |> ignore
    x

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

let canonicalizeEmail input =
    { input with email = input.email.Trim().ToLower() }

// dummy dead-end function
let updateDatabase input =
    // printfn "update DB"
    ()

let usecase =
    validate1
    >=> validate2
    >=> validate3
    >> map canonicalizeEmail
    // switch composition style
    >=> switch (tee updateDatabase)
    // or two-track style using normal composition
    >> map (tee updateDatabase)

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"

    let goodInput = {name="Alice"; email="UPPERCASE "}
    usecase goodInput
    |> printfn "Canonicalize Good Result = %A"

    //Canonicalize Good Result = Success {name = "Alice"; email = "uppercase";}

    let badInput = {name=""; email="UPPERCASE "}
    usecase badInput
    |> printfn "Canonicalize Bad Result = %A"

    //Canonicalize Bad Result = Failure "Name must not be blank"

    0 // return an integer exit code
