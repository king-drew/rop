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

let plus addSuccess addFailure switch1 switch2 x =
    match (switch1 x), (switch2 x) with
    | Success s1, Success s2 -> Success (addSuccess s1 s2)
    | Failure f1, Success _ -> Failure f1
    | Success _, Failure f2 -> Failure f2
    | Failure f1, Failure f2 -> Failure (addFailure f1 f2)

let (&&&) v1 v2 =
    let addSuccess r1 r2 = r1 // return first
    let addFailure s1 s2 = s1 + "; " + s2 // concat errors
    plus addSuccess addFailure v1 v2

let thisOrThat addSuccess addFailure switch1 switch2 x =
    match (switch1 x), (switch2 x) with
    | Success s1, Success s2 -> succeed (addSuccess s1 s2)
    | Failure _, Success s2 -> succeed s2
    | Success s1, Failure _ -> succeed s1
    | Failure f1, Failure f2 -> Failure (addFailure f1 f2)

let (|||) v1 v2 =
    let addSuccess r1 r2 = r1 // return first
    let addFailure s1 s2 = s1 + "; " + s2 // concat errors
    thisOrThat addSuccess addFailure v1 v2

type Config = {debug:bool}

let debugLogger twoTrackInput =
    let success x = printfn "DEBUG. Success so far: %A" x; x
    let failure = id // don't log here
    doubleMap success failure twoTrackInput

let injectableLogger config =
    if config.debug then debugLogger else id


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
    &&& validate2
    &&& validate3

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

let usecase config =
    combinedValidation
    >> map canonicalizeEmail
    >> injectableLogger config

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"

    let input = {name="Alice"; email="good"}

    let releaseConfig = {debug=false}
    input 
    |> usecase releaseConfig 
    |> ignore

    // no output

    let debugConfig = {debug=true}
    input 
    |> usecase debugConfig 
    |> ignore

    // debug output
    // DEBUG. Success so far: {name = "Alice"; email = "good";}

    0 // return an integer exit code
