    // INSTANTIATE A MAP WITH MOCK DATA (of key-value pairs)
    let dataMap = Map.ofList ["a", true; "b", false; "c", true]

    // DECLARE EXPRESSIONS WITH STRONGLY TYPED TYPES (like an interface in c#)
    type Expression =
    | Value of bool
    | Not of Expression
    | And of Expression * Expression
    | Or of Expression * Expression
    | Equals of Expression * Expression
    | Implies of Expression * Expression
    | ExculsiveOr of Expression * Expression
    | Variable of string



    (* --- Evaluate --- *)



    // INSTANTIATE RECURSIVELY MATCHING FUNCTION (like a switch in c#)
    let rec Evaluate (map:Map<string,bool>) exp =
        match exp with
        | Value n -> n
        | Not (x) -> not (Evaluate map x)
        | And (x, y) -> Evaluate map x && Evaluate map y
        | Or (x, y) -> Evaluate map x || Evaluate map y
        | Equals (x, y) -> Evaluate map x = Evaluate map y
        | Implies (x, y) -> (not (Evaluate map x ) || Evaluate map y)
        | ExculsiveOr (x, y) -> Evaluate map x <> Evaluate map y
        | Variable id -> map.[id]

    // INSTANTIATE EVALUATION FUNCTIONS
    let NotEvaluate = Evaluate dataMap (Not(Variable "b"))
    let AndEvaluate = Evaluate dataMap (And(Variable "a", Variable "c"))
    let OrEvaluate = Evaluate dataMap (Or(Variable "a", Variable "b"))
    let EqualsEvaluate = Evaluate dataMap (Equals(Variable "a", Value true))
    let ImpliesEvaluate = Evaluate dataMap (Implies(Variable "b", Variable "b"))
    let ExclusiveOrEvaluate = Evaluate dataMap (ExculsiveOr(Variable "a", Variable "b"))
    
    // EXECUTE EVALUATION FUNCTIONS
    printfn "NotEvaluate -> %b" NotEvaluate
    printfn "AndEvaluate -> %b" AndEvaluate
    printfn "OrEvaluate -> %b" OrEvaluate
    printfn "EqualsEvaluate -> %b" EqualsEvaluate
    printfn "ImpliesEvaluate -> %b" ImpliesEvaluate
    printfn "ExclusiveOrEvaluate -> %b \n" ExclusiveOrEvaluate 



    (* --- Names used --- *)
    


    let NamesUsed = dataMap |> Map.toList |> List.map (fun (fst, _) -> fst)
    for name in NamesUsed do printfn "NamesUsed -> %A" name
    printf("\n")



   