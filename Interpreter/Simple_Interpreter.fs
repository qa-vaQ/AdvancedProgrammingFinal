// Simple Interpreter in F#
// Author: R.J. Lapeer 
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

// Code written by Justin Picksley 100393736- MComp


// Features implemented: 
// INT1:
// Remainder: Done
// Exponentiation: Done
// Floating point numbers: Done
// Exponential notation: Done
// Unary minus: Done

// INT2 Done

// Rational number arithmetic: Not done
// Complex number arithmetic: Not done
// Works with built-in functions such as cos, sin, tan, exp, log, etc. : Done

// INT3 Done

// INT4 Done

namespace Interpreter

open System

module Interpreter = 

    type num = Int of int | Float of float | Vector of float list | Matrix of float list list  | Rational of int * int | Complex of float * float

    type terminal = 
        Add | Sub | Mul | Div | Rem | Pow | Lpar | Rpar | Num of num | Cos | Sin | Tan | Exp | Log | Id of string | Assign
        | For | To | Semicolon | Root | Integral | Diff
    type assignmentTypes =
        | Number of num
        | Function of string * list<terminal>
    
    type symbolTable = Map<string, assignmentTypes>

    let InitialSymTab : symbolTable = Map.empty
    
    let str2lst s = [for c in s -> c]
    let isblank c = System.Char.IsWhiteSpace c
    let isdigit c = System.Char.IsDigit c
    let isletter c = System.Char.IsLetter c

    let lexError = System.Exception("Lexer error")
    let intVal (c:char) = (int)((int)c - (int)'0')

    let floatVal (c:char) = (float)((int)c - (int)'0')

    let parseError = System.Exception("Parser error")
 
    let rec scInt(iStr, iVal) = 
        match iStr with
        c :: tail when isdigit c -> scInt(tail, 10*iVal+(intVal c))
        | _ -> (iStr, iVal)
    
    let rec scFloat(iStr, fVal, div) = 
        match iStr with
        | c :: tail when isdigit c -> scFloat(tail, fVal + (float)(intVal c)/div, div*10.0)
        // Exponential notation
        | 'E' :: '-' :: tail
        | 'e' :: '-' :: tail ->
            let (powStr, powVal) = scInt(tail, 0)
            (powStr, fVal * (10.0 ** (float -powVal)))
        | 'E'::tail 
        | 'e'::tail ->
            let (powStr, powVal) = scInt(tail, 0)
            (powStr, fVal * (10.0 ** (float powVal)))
        | _ -> (iStr, fVal)
    
    let rec scLetter(iStr, sVal) = 
        match iStr with
        c :: tail when isletter c || isdigit c -> scLetter(tail, sVal + string c)
        | _ -> (iStr, sVal)

    // Vector and Matrix library

    let vectorAdd v1 v2 = List.map2 (+) v1 v2
    let vectorSub v1 v2 = List.map2 (-) v1 v2
    let vectorDot v1 v2 = List.map2 (*) v1 v2 |> List.sum
    let vectorNorm v = sqrt (vectorDot v v)
    let matrixAdd m1 m2 = List.map2 (List.map2 (+)) m1 m2
    let matrixSub m1 m2 = List.map2 (List.map2 (-)) m1 m2

    let vectorCrossProduct v1 v2 =
        if List.length v1 <> 3 || List.length v2 <> 3 then
            raise (System.Exception("Cross product only defined for 3D vectors"))
        else
            [
                (v1.[1] * v2.[2]) - (v1.[2] * v2.[1]);
                (v1.[2] * v2.[0]) - (v1.[0] * v2.[2]);
                (v1.[0] * v2.[1]) - (v1.[1] * v2.[0])
            ]

    let matrixDeterminant m =
        match m with
        | [[a; b]; [c; d]] -> (a * d) - (b * c)
        | _ -> raise (System.Exception("Determinant only implemented for 2x2 matrices"))
    let lexer input = 
        let rec scan input =
            match input with
            | [] -> []
            | '+'::tail -> Add :: scan tail
            | '-'::tail -> Sub :: scan tail
            | '*'::tail -> Mul :: scan tail
            | '/'::tail -> Div :: scan tail
            | '%'::tail -> Rem:: scan tail
            | '^'::tail -> Pow:: scan tail
            | '('::tail -> Lpar:: scan tail
            | ')'::tail -> Rpar:: scan tail
            | '='::tail -> Assign:: scan tail
            | ';'::tail -> Semicolon:: scan tail
            | '.' :: tail -> let (fStr, fVal) = scFloat(tail, 0.0, 10.0)
                             Num (Float fVal) :: scan fStr
                         
            | c :: tail when isblank c -> scan tail
            | c :: tail when isdigit c -> let (iStr, iVal) = scInt(tail, intVal c) 
                                          match iStr with
                                          | '.' :: fTail -> let fStr, fVal = scFloat(fTail, 0.0, 10.0)
                                                            Num (Float (float iVal + fVal)) :: scan fStr
                                          | _ -> Num (Int iVal) :: scan iStr
                                      
            | c :: tail when isletter c -> let lStr, lVal = scLetter(tail, string c)
                                           match lVal with
                                           | "for" -> For :: scan lStr
                                           | "to" -> To :: scan lStr
                                           | "cos" -> Cos :: scan lStr
                                           | "sin" -> Sin :: scan lStr
                                           | "tan" -> Tan :: scan lStr
                                           | "exp" -> Exp :: scan lStr
                                           | "log" -> Log :: scan lStr
                                           | "root" -> Root :: scan lStr    
                                           | "diff" -> Diff :: scan lStr
                                           | "integral" -> Integral :: scan lStr
                                           | _ -> Id lVal :: scan lStr
            | _ -> raise lexError
        scan (str2lst input)

    let getInputString() : string = 
        Console.Write("Enter an expression: ")
        Console.ReadLine()
    
    // T - Term ( *, /, % )
    // E - Expression ( +, - )
    // NR - Number ( Num, Float, (E))
    // P - Power ( ^ )

    // Opt - Optional Continuation

    // Grammar in BNF: 
    // <Statement> ::= <Id> "(" <Id> ")" "=" <E> ";" 
    //               | <Id> "=" <E> ";" 
    //               | "for" <Id> "=" <E> "to" <E> ";" 
    //               | "diff" <Id> <E> ";" 
    //               | "root" <Id> <E> <E> ";" 
    //               | "integral" <Id> <E> <E> <E> ";" 
    //               | <E> ";" 
    //               | <E>

    // <E>        ::= <T> <Eopt>
    // <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
    // <T>        ::= <P> <Topt>
    // <Topt>     ::= "*" <P> <Topt> | "/" <P> <Topt> | "%" <P> <Topt> | <empty>
    // <P>        ::= <NR> <Popt>
    // <Popt>     ::= "^" <P> | <empty>
    // <NR>       ::= "Num" <value> |  <Id> "(" <E> ")" | <Id> | "-" <NR> | "(" <E> ")" | "cos" "(" <E> ")" | "sin" "(" <E> ")" | "tan" "(" <E> ")" | "exp" "(" <E> ")" | "log" "(" <E> ")"

    // Parser
    let parser tList = 
        let rec E tList = (T >> Eopt) tList
        and Eopt tList = 
            match tList with
            | Add :: tail -> (T >> Eopt) tail
            | Sub :: tail -> (T >> Eopt) tail
            | _ -> tList
        
        and T tList = (P >> Topt) tList
        and Topt tList =
            match tList with
            | Mul :: tail -> (P >> Topt) tail
            | Div :: tail -> (P >> Topt) tail
            | Rem :: tail -> (P >> Topt) tail
            | _ -> tList
        
        and P tList = (NR >> Popt) tList
        and Popt tList =
            match tList with
            | Pow :: tail -> (NR >> Popt) tail 
            | _ -> tList
        
        and NR tList =
            match tList with 
            | Num value :: tail -> tail
            | Id _ :: Lpar :: tail -> 
                (match E tail with
                 | Rpar :: tail -> tail
                 | _ -> raise (System.Exception("Missing ')' after function argument")))
            | Id _ :: tail -> tail 
            | Sub :: tail -> P tail 
            | Lpar :: tail -> 
                (match E tail with
                 | Rpar :: tail -> tail
                 | _ -> raise parseError)
            | Cos :: Lpar :: tail ->
                (match E tail with
                 | Rpar :: tail -> tail
                 | _ -> raise (System.Exception("Missing ')' after cos()")))
            | Sin :: Lpar :: tail ->
                (match E tail with
                 | Rpar :: tail -> tail
                 | _ -> raise (System.Exception("Missing ')' after sin()")))
            | Tan :: Lpar :: tail ->
                (match E tail with
                 | Rpar :: tail  -> tail
                 | _ -> raise (System.Exception("Missing ')' after tan()")))
            | Exp :: Lpar :: tail ->
                (match E tail with
                 | Rpar :: tail -> tail
                 | _ -> raise (System.Exception("Missing ')' after exp()")))
            | Log :: Lpar :: tail ->
                (match E tail with
                 | Rpar :: tail -> tail
                 | _ -> raise (System.Exception("Missing ')' after log()")))
            | _ -> raise parseError
    
        and statement tList =
            match tList with
            // Function: f(x) = ... ;
            | Id _ :: Lpar :: Id _ :: Rpar :: Assign :: tail ->
                (match E tail with
                 | Semicolon :: tail -> tail
                 | _ -> raise (System.Exception("Missing ';' after assignment.")))
            
            // Variable: x = ... ;
            | Id _ :: Assign :: tail ->
                (match E tail with
                 | Semicolon :: tail -> tail
                 | _ -> raise (System.Exception("Missing ';' after assignment.")))
            
            // For loop: for i = ... to ... ;
            | For :: Id _ :: Assign :: tail ->
                (match E tail with
                 | To :: tail2 ->
                     (match E tail2 with
                      | Semicolon :: tail3 -> tail3
                      | _ -> raise (System.Exception("Missing ; after for loop")))
                 | _ -> raise (System.Exception("Missing 'to' in for loop")))
            
            // Differentiation
            | Diff :: Id _ :: tail ->
                (match E tail with
                 | Semicolon :: tail -> tail
                 | _ -> raise (System.Exception("Missing ';' after differentiation.")))
            
            // Root finding 
            | Root :: Id _ ::  tail ->
                (match E tail with
                 tail2 ->
                     (match E tail2 with
                      | Semicolon :: tail3 -> tail3
                      | _ -> raise (System.Exception("Missing ';' after root bounds"))))
            
            // Integration
            | Integral :: Id _ :: tail ->
                (match E tail with
                 | tail2 ->
                     (match E tail2 with
                      | tail3 ->
                          (match E tail3 with
                           | Semicolon :: tail4 -> tail4
                           | _ -> raise (System.Exception("Missing ';' after integral command")))))
            
            // Expression
            | _ -> E tList
        statement tList
   
    // Evaluator with parsing
    let rec parseNeval symTab tList   = 
        // Add, Subtract
        let rec E symTab tList = (T symTab >> Eopt symTab) tList
        and Eopt symTab (tList, value) =
            match tList with
            | Add :: tail -> let (tLst, tval) = T symTab tail
                             let result =
                                 match (value, tval) with
                                 | (Int i1, Int i2) -> Int (i1 + i2)
                                 | (Float f1, Float f2) -> Float (f1 + f2)
                                 | (Int i, Float f) | (Float f, Int i) -> Float (float i + f)
                                 | (Vector v1 , Vector v2) -> Vector (vectorAdd v1 v2)
                                 | (Matrix m1 , Matrix m2) -> Matrix (matrixAdd m1 m2)
                             Eopt symTab (tLst, result)
                         
            | Sub :: tail -> let (tLst, tval) = T symTab tail
                             let result =
                                    match (value, tval) with
                                    | (Int i1, Int i2) -> Int (i1 - i2)
                                    | (Float f1, Float f2) -> Float (f1 - f2)
                                    | (Int i, Float f) -> Float (float i - f)
                                    | (Float f, Int i) -> Float (f - float i)
                                    | (Vector v1 , Vector v2) -> Vector (vectorSub v1 v2)
                                    | (Matrix m1 , Matrix m2) -> Matrix (matrixSub m1 m2)
                             Eopt symTab (tLst, result)
            | _ -> (tList, value)
        
        // Multiply, Divide, Remainder
        and T symTab tList = (P symTab >> Topt symTab) tList
        and Topt symTab (tList, value) =
            match tList with
            | Mul :: tail -> let (tLst, tval) = P symTab tail
                             let result =
                                 match (value, tval) with
                                 | (Int i1, Int i2) -> Int (i1 * i2)
                                 | (Float f1, Float f2) -> Float (f1 * f2)
                                 | (Int i, Float f) | (Float f, Int i) -> Float (float i * f)
                                 | (Vector v1, Vector v2) -> Float (vectorDot v1 v2)
                             Topt symTab (tLst, result)
                         
            | Div :: tail -> let (tLst, tval) = P symTab tail
                             let result =
                                 match (value, tval) with
                                 | (Int i1, Int i2) -> 
                                     if i2 = 0 then raise (System.DivideByZeroException())
                                     Int (i1 / i2)
                                 | (Float f1, Float f2) -> 
                                     if f2 = 0.0 then raise (System.DivideByZeroException())
                                     Float (f1 / f2)
                                 | (Int i, Float f) -> 
                                     if f = 0.0 then raise (System.DivideByZeroException())
                                     Float (float i / f)
                                 | (Float f, Int i) -> 
                                     if i = 0 then raise (System.DivideByZeroException())
                                     Float (f / float i)
                             Topt symTab (tLst, result)
                         
            | Rem :: tail -> let (tLst, tval) = P symTab tail
                             let result =
                                    match (value, tval) with
                                    | (Int i1, Int i2) -> 
                                        if i2 = 0 then raise (System.DivideByZeroException())
                                        Int (i1 % i2)
                                    | (Float f1, Float f2) -> 
                                        if f2 = 0.0 then raise (System.DivideByZeroException())
                                        Float (f1 % f2)
                                    | (Int i, Float f) -> 
                                        if f = 0.0 then raise (System.DivideByZeroException())
                                        Float (float i % f)
                                    | (Float f, Int i) -> 
                                        if i = 0 then raise (System.DivideByZeroException())
                                        Float (f % float i)
                             Topt symTab (tLst, result)
                         
            | _ -> (tList, value)
        
        // Power
        and P symTab tList = (NR symTab >> Popt symTab) tList
        and Popt symTab (tList, value) = 
            match tList with
            | Pow :: tail -> let (tLst, tval) = P symTab tail
                             let result =
                                 match (value, tval) with
                                 | (Int i1, Int i2) -> Int (int (Math.Pow(float i1, float i2)))
                                 | (Float f1, Float f2) -> Float (Math.Pow(f1, f2))
                                 | (Int i, Float f) -> Float (Math.Pow(float i, f))
                                 | (Float f, Int i) -> Float (Math.Pow(f, float i))
                             Popt symTab (tLst, result)
                         
            | _ -> (tList, value)
        
        // Number
        and NR symTab tList =
            match tList with
            | Num (Int value) :: tail -> (tail, Int value)
            | Num (Float value) :: tail -> (tail, Float value)
        
            // Function calls 
            | Id name :: Lpar :: tail ->
                try
                    let storedVal = Map.find name symTab
                    match storedVal with
                    | Function (paramName, bodyTokens) ->
                        (match E symTab tail with // Match on result of (E)
                         | (Rpar :: finalTail, paramValue) ->
                            let tempSymTab = Map.add paramName (Number paramValue) symTab
                            let (remFromBody, result) = parseNeval tempSymTab bodyTokens
                            (finalTail, result) 
                         | _ -> raise (System.Exception("Missing ')' after function argument.")))
                    
                    | Number _ -> raise (System.Exception $"'%s{name}' is not a function")
                with
                | :? System.Collections.Generic.KeyNotFoundException -> 
                    raise (System.Exception $"Function '%s{name}' not defined")

            // Variable lookup
            | Id name :: tail ->
                try
                    let storedVal = Map.find name symTab
                    match storedVal with
                    | Number n -> (tail, n) 
                    | Function _ -> raise (System.Exception $"'%s{name}' is a function, missing ()")
                with
                | :? System.Collections.Generic.KeyNotFoundException -> 
                    raise (System.Exception $"Variable '%s{name}' not defined")
            
            | Sub :: tail -> let (tLst, tval) = P symTab tail
                             let result =
                                 match tval with
                                 | Int i -> Int (-i)
                                 | Float f -> Float (-f)
                             (tLst, result)
                         
            | Lpar :: tail -> 
                (match E symTab tail with 
                 | (Rpar :: tail, tval) -> (tail, tval)
                 | _ -> raise parseError)
            
            | Cos :: Lpar :: tail ->
                (match E symTab tail with
                 | Rpar :: tail, tval ->
                     let result =
                         match tval with
                         | Int i -> Float (Math.Cos(float i))
                         | Float f -> Float (Math.Cos(f))
                     (tail, result)
                 | _ -> raise (System.Exception("Missing ')' after cos()")))
            | Sin :: Lpar :: tail ->
                (match E symTab tail with
                    | Rpar :: tail, tval ->
                        let result =
                            match tval with
                            | Int i -> Float (Math.Sin(float i))
                            | Float f -> Float (Math.Sin(f))
                        (tail, result)
                    | _ -> raise (System.Exception("Missing ')' after sin()")))
            | Tan :: Lpar :: tail ->
                (match E symTab tail with
                    | Rpar :: tail, tval ->
                        let result =
                            match tval with
                            | Int i -> Float (Math.Tan(float i))
                            | Float f -> Float (Math.Tan(f))
                        (tail, result)
                    | _ -> raise (System.Exception("Missing ')' after tan()")))
            | Exp :: Lpar :: tail ->
                (match E symTab tail with
                    | Rpar :: tail, tval ->
                        let result =
                            match tval with
                            | Int i -> Float (Math.Exp(float i))
                            | Float f -> Float (Math.Exp(f))
                        (tail, result)
                    | _ -> raise (System.Exception("Missing ')' after exp()")))
            
            | Log :: Lpar :: tail ->
                (match E symTab tail with
                    | Rpar :: tail, tval ->
                        let result =
                            match tval with
                            | Int i ->
                                if i <= 0 then raise (System.Exception("Logarithm of non-positive number"))
                                Float (Math.Log(float i))
                            | Float f ->
                                if f <= 0.0 then raise (System.Exception("Logarithm of non-positive number"))
                                Float (Math.Log(f))
                        (tail, result)
                    | _ -> raise (System.Exception("Missing ')' after log()")))
            
            | _ -> raise parseError
       
        E symTab tList

    let rec printTList (lst:list<terminal>) : list<string> = 
        match lst with
        head::tail -> Console.Write("{0} ",head.ToString())
                      printTList tail
                  
        | [] -> Console.Write("EOL\n")
                []

    let differentiation funcName xVal symTab =
        match Map.tryFind funcName symTab with
        | Some (Function (paramName, tokens)) ->
            // Using formula (ax^n)' = a*n*x^(n-1)
            let calcPower baseVal power = Math.Pow(baseVal, power - 1.0)

            let rec derive tokens acc =
                match tokens with
                | [] -> acc 
            
                // Explicit Multiply (3 * x^2)
                | Num (Int a) :: Mul :: Id p :: Pow :: Num (Int n) :: tail when p = paramName ->
                    let derivativeTerm = (float a) * (float n) * (calcPower xVal (float n))
                    derive tail (acc + derivativeTerm)

                // No Coefficient (x^2)
                | Id p :: Pow :: Num (Int n) :: tail when p = paramName ->
                    let derivativeTerm = (float n) * (calcPower xVal (float n))
                    derive tail (acc + derivativeTerm)

                // Explicit Multiply (5 * x)
                | Num (Int a) :: Mul :: Id p :: tail when p = paramName ->
                    let derivativeTerm = float a
                    derive tail (acc + derivativeTerm)

                // Variable (x)
                | Id p :: tail when p = paramName ->
                    derive tail (acc + 1.0)

                // Constants
                | Num _ :: tail -> derive tail acc
            
                // Skip operators
                | _ :: tail -> derive tail acc

            derive tokens 0.0

        | _ -> raise (System.Exception($"Function '{funcName}' not found"))
    
    // Calculate f(x) ---
    let calculateY funcName xVal symTab =
        match Map.tryFind funcName symTab with
        | Some (Function (paramName, tokens)) ->
            let tempSymTab = Map.add paramName (Number (Float xVal)) symTab
            let (_, result) = parseNeval tempSymTab tokens
            match result with | Int i -> float i | Float f -> f
        | _ -> raise (System.Exception($"Function '{funcName}' not found"))

    // Newton-Raphson + Bisection ---
    let solveRootHybrid funcName low high symTab =
        let tolerance = 0.0001
        let maxIter = 100

        let yLow = calculateY funcName low symTab
        let yHigh = calculateY funcName high symTab
    
        // Check for root
        if (yLow * yHigh) > 0.0 then 
            raise (System.Exception($"Root not bracketed. f({low}) and f({high}) have the same sign."))
    
        // Shrinking interval
        let rec iterate a b iter =
            if iter > maxIter then 
                Console.WriteLine("Converged via max iterations.")
                (a + b) / 2.0
            else
                // If f(mid) is close enough to 0, return midpoint
                let mid = (a + b) / 2.0
                let yMid = calculateY funcName mid symTab
                if abs yMid < tolerance then mid
                else
                    let slope = differentiation funcName mid symTab
                    let newtonStep = if abs slope < 0.00001 then 100000.0 else mid - (yMid / slope)

                    // If Newton step stays strictly inside [a, b], use it
                    // else fail-safe to bisection/midpoint
                    let nextGuess = 
                        if newtonStep > a && newtonStep < b then 
                            newtonStep // Newton is safe
                        else 
                            (a + b) / 2.0 // Newton jumped out; fallback to Bisection

                    let yNext = calculateY funcName nextGuess symTab

                    // Update Brackets
                    if (yLow * yNext) < 0.0 then
                        iterate a nextGuess (iter + 1) // Root is in left half
                    else
                        iterate nextGuess b (iter + 1) // Root is in right half
    
        if low < high then iterate low high 0
        else iterate high low 0

    // Integration (Trapezoidal Rule) ---
    let solveIntegral funcName a b steps symTab =
        let h = (b - a) / steps
    
        // Recursive loop to sum the middle terms: 2 * f(x_i)
        let rec sumMiddle i acc =
            if i >= steps then acc
            else
                let x = a + (i * h)
                let y = calculateY funcName x symTab
                sumMiddle (i + 1.0) (acc + (2.0 * y))
            
        let fStart = calculateY funcName a symTab // f(a)
        let fEnd = calculateY funcName b symTab   // f(b)
        let middleSum = sumMiddle 1.0 0.0
    
        // Final Formula: (h/2) * (f(a) + 2*sum + f(b))
        (h / 2.0) * (fStart + middleSum + fEnd)
    let statement_or_assignment symTab tList =
        // Helper to get tokens for function body until semicolon
        let rec getExprTokens (tList: terminal list) (acc: terminal list) =
            match tList with
            | Semicolon :: tail -> (List.rev acc, tail) // Found end
            | token :: tail -> getExprTokens tail (token :: acc) // Add token
            | [] -> (List.rev acc, []) // End of line
    
        match tList with
        | For :: Id varName :: Assign :: tail -> 
            let (remAfterStart, startVal) = parseNeval symTab tail
            (match remAfterStart with
             | To :: tailAfterTo ->
                let (remAfterEnd, endVal) = parseNeval symTab tailAfterTo
            
                (match remAfterEnd with
                 | Semicolon :: tail ->
                    let toFloat (n: num) = match n with | Int i -> float i | Float f -> f
                    let start = toFloat startVal
                    let stop = toFloat endVal

                    let rec runFor i (currentSymTab: symbolTable) =
                        if i > stop then currentSymTab // Loop finished
                        else
                            Console.WriteLine(i) 
                        
                            let tempSymTab = Map.add varName (Number (Float i)) currentSymTab
                        
                            runFor (i + 1.0) tempSymTab
                
                    let finalSymTab = runFor start symTab
                    (finalSymTab, None) // Return final table, nothing to print

                 | _ -> raise (System.Exception("Missing ';' after 'for' loop.")))
             | _ -> raise (System.Exception("Missing 'to' in 'for' loop.")))
        
        | Diff :: Id funcName :: tail ->
            let (remTokens, xValWrapper) = parseNeval symTab tail
            match remTokens with
            | Semicolon :: tail ->
                // Extract the number (e.g., 2 from "diff f 2")
                let x = match xValWrapper with | Int i -> float i | Float f -> f
            
                // Calculate derivative using power rule
                let slope = differentiation funcName x symTab
            
                Console.WriteLine($"Derivative of {funcName} at {x} is: {slope}")
                (symTab, Some (Float slope))
            
            | _ -> raise (System.Exception("Invalid syntax for diff"))
        
        | Root :: Id funcName :: tail ->
                let (tail2, minValWrapper) = parseNeval symTab tail
                let (tail3, maxValWrapper) = parseNeval symTab tail2
            
                match tail3 with
                | Semicolon :: tail ->
                    let minVal = match minValWrapper with | Int i -> float i | Float f -> f
                    let maxVal = match maxValWrapper with | Int i -> float i | Float f -> f
                
                    try
                        let root = solveRootHybrid funcName minVal maxVal symTab
                        Console.WriteLine($"Root found at: {root}")
                        (symTab, Some (Float root))
                    with
                    | ex -> 
                        Console.WriteLine($"Root Error: {ex.Message}")
                        (symTab, None)

                | _ -> raise (System.Exception("Invalid syntax for root"))
            
        | Integral :: Id funcName :: tail ->
            let (tail2, startWrapper) = parseNeval symTab tail
            let (tail3, endWrapper) = parseNeval symTab tail2
            let (tail4, stepsWrapper) = parseNeval symTab tail3
        
            match tail4 with
            | Semicolon :: tail ->
                let start = match startWrapper with | Int i -> float i | Float f -> f
                let stop = match endWrapper with | Int i -> float i | Float f -> f
                let steps = match stepsWrapper with | Int i -> float i | Float f -> f
            
                if steps <= 0.0 then raise (System.Exception("Steps must be > 0"))
            
                // Run Trapezoidal Rule
                let area = solveIntegral funcName start stop steps symTab
            
                Console.WriteLine($"Area of {funcName} from {start} to {stop} is: {area}")
                (symTab, Some (Float area))
            
            | _ -> raise (System.Exception("Invalid syntax for integral"))
        
        // Function definition
        // e.g. y(x) = x^2 + 5;
        | Id funcName :: Lpar :: Id paramName :: Rpar :: Assign :: tail ->
        
            let (bodyTokens, remTokens) = getExprTokens tail []
            if List.isEmpty bodyTokens then
                raise (System.Exception("Missing function body after '='."))
        
            // store the parameter name ("x") and formula tokens
            let valueToStore = Function (paramName, bodyTokens)
            let newSymTab = Map.add funcName valueToStore symTab
            (newSymTab, None) // Return new table

        // Variable assignment
        // e.g. x = 10; or x = x + 3;
        | Id name :: Assign :: tail ->
        
            let (remTokens, value) = parseNeval symTab tail 
        
            (match remTokens with
             | Semicolon :: tail ->
                 let valueToStore = Number value
                 let newSymTab = Map.add name valueToStore symTab 
                 (newSymTab, None) 
             | _ -> raise (System.Exception("Invalid statement after assignment.")))
        
        // Expression
        | _ ->
            let (remTokens, value) = parseNeval symTab tList
            (match remTokens with
             | Semicolon :: tail -> (symTab, Some value)
             | [] -> (symTab, Some value) 
             | _ -> raise (System.Exception("Invalid expression")))

    // Loop
    let rec loop (symTab: symbolTable) =
        Console.Write("> ") 
        let input: string = getInputString()

        if input.ToLower() = "exit" then () 
        else if input.Trim() = "" then loop symTab 
        else
            try
                let oList = lexer input
            
                let (newSymTab, result) = statement_or_assignment symTab oList
            
                let sList = printTList oList
                let pList = printTList (parser oList)
                Console.WriteLine("Parser Success")
            
                match result with
                | Some (Int i) -> Console.WriteLine("Result = {0}", i)
                | Some (Float f) -> Console.WriteLine("Result = {0}", f)
                | None -> ()

                // Recurse with new symbol table
                loop newSymTab

            with
            | ex when ex = lexError -> 
                Console.WriteLine("LEXER ERROR - Invalid character in input")
                loop symTab 
            | ex when ex = parseError -> 
                Console.WriteLine("PARSER ERROR - Invalid syntax in input")
                loop symTab
            | :? System.DivideByZeroException -> 
                Console.WriteLine("Divide by zero error")
                loop symTab
            | :? System.Exception as ex -> 
                loop symTab

    let run_expression (symTab: symbolTable) (input: string) =
        if String.IsNullOrWhiteSpace(input.Trim()) then 
            (symTab, "") 
        else
            try
                let tList = lexer input 
                let (newSymTab, result) = statement_or_assignment symTab tList 
            
                let output = 
                    match result with
                    | Some (Int i) -> sprintf "%A" i  
                    | Some (Float f) -> sprintf "%A" f  
                    | None -> ""  
            
                (newSymTab, output)  
            with
            | ex when ex = lexError -> 
                (symTab, "LEXER ERROR - Invalid character in input")  
            | ex when ex = parseError -> 
                (symTab, "PARSER ERROR - Invalid syntax in input")  
            | :? System.DivideByZeroException -> 
                (symTab, "Divide by zero error")  
            | ex -> 
                (symTab, sprintf "RUNTIME ERROR - %A" ex.Message)  

    let CSharpInput (symTab: symbolTable) (input: string) =
        run_expression symTab input

    let CSharpPlot (symTab: symbolTable) (expression: string) (varName: string) (varValue: float) =
        if String.IsNullOrWhiteSpace(expression.Trim()) then 
            None  
        else
            try
                let tList = lexer expression  
                let item = Float varValue  
                let symTab2 = Map.add varName (Number item) symTab  
            
                let (remainingTokens, resultValue) = parseNeval symTab2 tList  
            
                match remainingTokens with
                | head :: _ when head <> Semicolon -> None  
                | _ -> 
                    match resultValue with
                    | Int i -> Some (float i)  
                    | Float f -> Some f  
            with
            | _ -> None  

    let CSharpPlotToNullable (symTab: symbolTable) (expression: string) (varName: string) (varValue: float) =
        match CSharpPlot symTab expression varName varValue with
        | Some value -> System.Nullable(value)  
        | None -> System.Nullable()  

    [<EntryPoint>]
    let main argv  =
        Console.WriteLine("Type 'exit' to quit")
    
        // Preloaded Data (Vectors and Matrices)
        // v1 = [1; 2; 3], v2 = [4; 5; 6] -> v1+v2 = [5; 7; 9], v1.v2 = 32
        // m1 = [[1; 2]; [3; 4]] -> det = 1*4 - 2*3 = -2
        let initialSymTab = 
            Map.empty 
            |> Map.add "v1" (Number (Vector [1.0; 2.0; 3.0]))
            |> Map.add "v2" (Number (Vector [4.0; 5.0; 6.0]))
            |> Map.add "m1" (Number (Matrix [[1.0; 2.0]; [3.0; 4.0]]))

        // (Input String, Description)
        let testCases = [
            // Arithmetic
            ("5*3+(2*3-2)/2+6", "Complex Arithmetic (Expect 23)");
            ("9-3-2", "Left Associativity (Expect 4)");
            ("10/3", "Integer Division (Expect 3)");
            ("10/3.0", "Float Division (Expect 3.333)");
            ("10%3", "Modulus (Expect 1)");
            ("10 - -2", "Double Negative (Expect 12)");
            ("-2 + 10", "Negative Start (Expect 8)");
            ("3*5^(-1+3)-2^2*-3", "Power Test (Expect 87)");
            ("-3^2", "Precedence Fix (Expect -9)"); 
            ("-7%3", "F# Mod Behavior (Expect -1)");
            ("2*3^2", "Precedence Pow > Mult (Expect 18)");
            ("(((3*2--2)))", "Nested Parens (Expect 8)");
            ("(((3*2--2))", "Syntax Error (Expect Parse Error)")
            ("1.2E3", "Sci Notation Positive (Expect 1200.0)");
            ("2.5e-2", "Sci Notation Negative (Expect 0.025)");
            ("1E5", "Sci Notation Integer Base (Expect 100000.0)");

            // Variables
            ("x = 10;", "Assign x");
            ("x * 2", "Use x (Expect 20)");
            ("y = 5;", "Assign y");
            ("z = y + 2;", "Assign z using y");
            ("z", "Read z (Expect 7)");
            ("unknown * 5", "Undefined Var (Expect Error)");

            // Functions
            ("f(x) = x^2;", "Define Function f");
            ("f(3)", "Call Function f (Expect 9)");
            ("sin(0)", "Built-in Sin (Expect 0.0)");
            ("cos(0)", "Built-in Cos (Expect 1.0)");
            ("tan(45)", "Built-in Tan");
            ("log(10)", "Built-in Log");
            ("log(-1)", "Log Error (Expect Error)");

            // Loops
            ("for i = 1 to 5;", "Loop 1 to 5 (Prints 1..5)");
            ("for i = 0 to 0;", "Loop 0 to 0 (Prints 0)");
            ("for i = 10 to 5;", "Loop 10 to 5 (Prints Nothing)");

            // Calculus: Differentiation
            ("f(x) = x^2;", "Redefine f for Diff");
            ("diff f 2;", "Diff Power Rule (Expect 4.0)");
            ("g(x) = 3*x^2 + 2*x;", "Define g");
            ("diff g 1;", "Diff Sum Rule (Expect 8.0)");
            ("h(x) = 5;", "Define h constant");
            ("diff h 10;", "Diff Constant (Expect 0.0)");

            // Calculus: Roots
            ("f(x) = x^2 - 4;", "Define f for Root");
            ("root f 0 5;", "Root Finding (Expect ~2.0)");
            ("root f 3 5;", "Bad Bracket (Expect Error/Fail)");

            // Calculus: Integration
            ("f(x) = 2*x;", "Define f for Integral");
            ("integral f 0 4 100;", "Triangle Area (Expect 16.0)");
            ("g(x) = 5;", "Define g constant");
            ("integral g 0 10 10;", "Rectangle Area (Expect 50.0)");

            // Linear Algebra (Using Preloaded v1, v2, m1)
            ("v1 + v2", "Vector Add (Expect [5; 7; 9])");
            ("vectorCrossProduct(v1, v2)", "Cross Product (3D only)"); // *Requires argument update if supported
            ("v1 * v2", "Vector Dot (Expect 32.0)");
            ("vectorNorm(v1)", "Vector Norm (Expect 3.741)");
            ("matrixDeterminant(m1)", "Matrix Det (Expect -2.0)");
        ]
  
        let rec runTests tests symTab =
            match tests with
            | [] -> 
                Console.WriteLine("\n--- ALL TESTS COMPLETED ---")
                // loop symTab 
            | (input, desc) :: tail ->
                printfn "\nTest: %s" desc   // %s for string
                printfn "Input: %s" input
            
                try
                    let tokens = lexer input
                
                    let (newSymTab, result) = statement_or_assignment symTab tokens
                
                    // Print Result
                    match result with
                    | Some (Int i) -> Console.WriteLine("  => Result: {0}", i)
                    | Some (Float f) -> Console.WriteLine("  => Result: {0}", f)
                    | Some (Vector v) -> Console.WriteLine("  => Result: Vector {0}", v)
                    | Some (Matrix m) -> Console.WriteLine("  => Result: Matrix {0}", m)
                    | None -> Console.WriteLine("  => (Statement Executed)")
                
                    // Continue with updated symbol table
                    runTests tail newSymTab

                with
                | ex -> 
                    Console.WriteLine("  => [HANDLED EXCEPTION]: {0}", ex.Message)
                    // Continue with OLD symbol table
                    runTests tail symTab

        // Start the runner
        runTests testCases initialSymTab
    
        let initialSymTab = Map.empty
        loop initialSymTab
        0