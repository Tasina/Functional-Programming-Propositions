//
//Compulsory	Assignment:	Propositions
//Af Hardy, Rasmus og Tasin
//

// Evaluate
//  (Propositions) - Like an Interface C# Class
    type Expression =
    | Value of bool
    | Not of Expression
    | And of Expression * Expression
    | Or of Expression * Expression
    | Equals of Expression * Expression
    | Implies of Expression * Expression
    | ExculsiveOr of Expression * Expression
    | Variable of string

//  Evaluates the Propositions - Like a C# Class using an Abstract Class and implementing the methods.
    let rec Evaluate (env:Map<string,bool>) exp =
        match exp with
        | Value n -> n
        | Not (x) -> not (Evaluate env x)
        | And (x, y) -> Evaluate env x && Evaluate env y
        | Or (x, y) -> Evaluate env x || Evaluate env y
        | Equals (x, y) -> Evaluate env x = Evaluate env y
        | Implies (x, y) -> (not (Evaluate env x ) || Evaluate env y)
        | ExculsiveOr (x, y) -> Evaluate env x <> Evaluate env y
        | Variable id -> env.[id]

//  Map filled with string keys and boolean values.
    let environment = Map.ofList [ "a", true ;
                                   "b", false ;
                                   "c", true ]

//   Create an expression that represents
    let notExp = Not(Variable "a");
    let andExp = And(Variable "a", Variable "b")
    let orExp = Or(Variable "a", Variable "b")
    let equalsExp = Equals(Variable "a", Value true)
    let impliesExp = Implies(Variable "a", Variable "b")
    let exculsiveOrExp = ExculsiveOr(Variable "a", Variable "b") 
    
//  Evaluate the expressions
    let NotEvaluate = Evaluate environment notExp
    let AndEvaluate = Evaluate environment andExp
    let OrEvaluate = Evaluate environment orExp
    let EquealsEvaluate = Evaluate environment equalsExp
    let ImpliesEvaluate = Evaluate environment impliesExp
    let ExculsiveOrEvaluate = Evaluate environment exculsiveOrExp
    


// Verify

// Tautology

// Satisfiable

// Equality

// ToString

// Names used


   