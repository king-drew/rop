// Learn more about F# at http://fsharp.org

open System

type Result<'TSuccess, 'TFailure> = 
    | Success of 'TSuccess
    | Failure of 'TFailure

let succeed x =
    Success x

let fail x =
    Failure x

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


let tee f x =
    f x |> ignore
    x

let tryCatch f x =
    try
        f x |> Success
    with
    | ex -> Failure ex.Message

let doubleMap successFunc failureFunc twoTrackInput =
    match twoTrackInput with
    | Success s -> Success (successFunc s)
    | Failure f -> Failure (failureFunc f)

// simpler map implementation using doubleMap
let map successFunc =
    doubleMap successFunc id

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

let log twoTrackInput =
    let success x = printfn "DEBUG. Success so far: %A" x; x
    let failure x = printfn "ERROR. %A" x; x
    doubleMap success failure twoTrackInput

let usecase =
    validate1
    >=> validate2
    >=> validate3
    >> map canonicalizeEmail
    >=> tryCatch (tee updateDatabase)
    >> log

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"

    let goodInput = {name="Alice"; email="good"}
    usecase goodInput
    |> printfn "Good Result = %A"

    // DEBUG. Success so far: {name = "Alice"; email = "good";}
    // Good Result = Success {name = "Alice"; email = "good";}

    let badInput = {name=""; email=""}
    usecase badInput 
    |> printfn "Bad Result = %A"

    // ERROR. "Name must not be blank"
    // Bad Result = Failure "Name must not be blank"

    0 // return an integer exit code
