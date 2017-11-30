    open System
    
    // CONSOLE START MESSAGE
    Console.BackgroundColor<-ConsoleColor.Yellow
    Console.ForegroundColor<-ConsoleColor.Black
    printfn "Compulsory Assignment : Propositions\n      by Rasmus, Tasin & Hardy      \n\n"
    Console.BackgroundColor<-ConsoleColor.Black
    Console.ForegroundColor<-ConsoleColor.White
    
    // INSTANTIATE A MAP WITH MOCK DATA (of key-value pairs)
    let dataMap = Map.ofList ["a", true; "b", false; "c", true]
    
    // DECLARE DISCRIMINATED UNIONS (like an interface in c# with strongly typed types)
    type Expression =
    | Value of bool
    | Not of Expression
    | And of Expression * Expression
    | Or of Expression * Expression
    | Equals of Expression * Expression
    | Implies of Expression * Expression
    | ExclusiveOr of Expression * Expression
    | Variable of string

    
    (*
        Evaluate:
        Computes the value of a proposition in a given state or environment.
        The environment is a binding of values (either true or false).
        The signature is:
        
        val Evaluate : map:Map<string,bool> -> exp:Expression -> bool
    *)
    
    // INSTANTIATE RECURSIVELY MATCHING FUNCTION (like a switch in c#)
    let rec Evaluate (map:Map<string,bool>) exp =
        match exp with
        | Value n -> n
        | Not (x) -> not (Evaluate map x)
        | And (x, y) -> Evaluate map x && Evaluate map y
        | Or (x, y) -> Evaluate map x || Evaluate map y
        | Equals (x, y) -> Evaluate map x = Evaluate map y
        | Implies (x, y) -> (not (Evaluate map x ) || Evaluate map y)
        | ExclusiveOr (x, y) -> Evaluate map x <> Evaluate map y
        | Variable id -> map.[id]   

    // INSTANTIATE EVALUATION FUNCTIONS
    let NotEvaluate = Evaluate dataMap (Not(Variable "b"))
    let AndEvaluate = Evaluate dataMap (And(Variable "a", Variable "c"))
    let OrEvaluate = Evaluate dataMap (Or(Variable "a", Variable "b"))
    let EqualsEvaluate = Evaluate dataMap (Equals(Variable "a", Variable "c"))
    let ImpliesEvaluate = Evaluate dataMap (Implies(Variable "b", Variable "b"))
    let ExclusiveOrEvaluate = Evaluate dataMap (ExclusiveOr(Variable "a", Variable "b"))
    
    // EXECUTE EVALUATION FUNCTIONS
    printfn "Evaluate exercise:\nNotEvaluate -> %b" NotEvaluate
    printfn "AndEvaluate -> %b" AndEvaluate
    printfn "OrEvaluate -> %b" OrEvaluate
    printfn "EqualsEvaluate -> %b" EqualsEvaluate
    printfn "ImpliesEvaluate -> %b" ImpliesEvaluate
    printfn "ExclusiveOrEvaluate -> %b \n" ExclusiveOrEvaluate 
    

    (*
        Verify:
        Check that all variables used in the proposition are defined in an environment.
        The signature is:
        
        val Verify : map:Map<string,bool> -> exp:Expression -> bool    
    *)

    // INSTANTIATE RECURSIVELY MATCHING FUNCTION
    let rec Verify (map:Map<string,bool>) exp =
        match exp with
        | Value n -> n
        | Not (x) -> Verify map x
        | And (x, y) -> Verify map x && Verify map y
        | Or (x, y) -> Verify map x && Verify map y
        | Equals (x, y) -> Verify map x && Verify map y
        | Implies (x, y) -> Verify map x && Verify map y
        | ExclusiveOr (x, y) -> Verify map x && Verify map y
        | Variable id -> map.ContainsKey(id)

    // INSTANTIATE VERIFY FUNCTIONS
    let NotVerify = Verify dataMap (Not(Variable "a"))
    let AndVerify = Verify dataMap (And(Variable "b", Variable "c"))
    let OrVerify = Verify dataMap (Or(Variable "d", Variable "e"))
    let EqualsVerify = Verify dataMap (Equals(Variable "f", Variable "g"))
    let ImpliesVerify = Verify dataMap (Implies(Variable "h", Variable "i"))
    let ExclusiveOrVerify = Verify dataMap (ExclusiveOr(Variable "j", Variable "k"))
    
    // EXECUTE VERIFY FUNCTIONS
    printfn "Verify exercise:\nNotVerify -> %A" NotVerify
    printfn "AndVerify -> %A" AndVerify
    printfn "OrVerify -> %A" OrVerify
    printfn "EqualsVerify -> %A" EqualsVerify
    printfn "ImpliesVerify -> %A" ImpliesVerify
    printfn "ExclusiveOrVerify -> %A \n" ExclusiveOrVerify


    (*
        Names Used:
        Return a list of strings containing all the names of the variables used in a proposition.
        The signature is:
        
        val NamesUsed : exp:Expression -> bool    
    *)

    // INSTANTIATE RECURSIVELY MATCHING FUNCTION
    let rec NamesUsed exp = 
        match exp with
        | Value n -> []
        | Not (x) -> NamesUsed x
        | And (x, y) -> List.append (NamesUsed x) (NamesUsed y)
        | Or (x, y) -> List.append (NamesUsed x) (NamesUsed y)
        | Equals (x, y) -> List.append (NamesUsed x) (NamesUsed y)
        | Implies (x, y) -> List.append (NamesUsed x) (NamesUsed y)
        | ExclusiveOr (x, y) -> List.append (NamesUsed x) (NamesUsed y)
        | Variable id -> [id]

    // INSTANTIATE NAMESUSED FUNCTIONS
    let NotNamesUsed = NamesUsed (Not(Variable "a"))
    let AndNamesUsed = NamesUsed (And(Variable "b", Variable "c"))
    let OrNamesUsed = NamesUsed (Or(Variable "d", Variable "e"))
    let EqualsNamesUsed = NamesUsed (Equals(Variable "f", Variable "g"))
    let ImpliesNamesUsed = NamesUsed (Implies(Variable "h", Variable "i"))
    let ExclusiveOrNamesUsed = NamesUsed (ExclusiveOr(Variable "j", Variable "k"))
    
    // EXECUTE NAMESUSED FUNCTIONS
    printfn "NamesUsed exercise:\nNotNamesUsed -> %A" NotNamesUsed
    printfn "AndNamesUsed -> %A" AndNamesUsed
    printfn "OrNamesUsed -> %A" OrNamesUsed
    printfn "EqualsNamesUsed -> %A" EqualsNamesUsed
    printfn "ImpliesNamesUsed -> %A" ImpliesNamesUsed
    printfn "ExclusiveOrNamesUsed -> %A \n" ExclusiveOrNamesUsed