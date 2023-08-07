type Exp =
  | Num of int
  | Id of string
  | Mul of Exp * Exp
  | Add of Exp * Exp
  | Fun of string * Exp
  | Ap of Exp * Exp
  | Capture of Exp
  | Escape of string * Exp
  
type Env = Map<string, Value>
and Value =
  | NumV of int
  | ClosureV of (string * Exp) * Env
  | ContV of (Value -> Value)

let rec eval (e: Exp) (env: Env) (k: Value -> Value) : Value =
  match e with
  | Num n -> k(NumV(n))
  | Id x -> k(env[x])
  | Add (l,r) -> eval l env (fun lv -> 
      eval r env (fun rv -> 
        match lv, rv with
        | NumV v1, NumV v2 -> 
            let total = v1 + v2
            printfn "%A" (v1, v2, total)
            k(NumV(total))
        | _ -> raise (System.ArgumentException("can only add numbers"))))
  | Mul (l,r) -> eval l env (fun lv -> 
      eval r env (fun rv -> 
        match lv, rv with
        | NumV v1, NumV v2 -> 
            let total = v1 * v2
            printfn "%A" (v1, v2, total)
            k(NumV(total))
        | _ -> raise (System.ArgumentException("can only multiply numbers"))))
  | Fun (param, body) -> k(ClosureV((param, body), env))
  | Ap (f, a) -> eval f env (fun cl -> 
    match cl with
    | ClosureV ((param, body), closureEnv) -> 
        eval a env (fun av -> eval body (closureEnv.Add(param,av)) k)
    | ContV k2 -> eval a env (fun av -> k(k2(av)))
    | _ -> raise (System.ArgumentException("can only apply functions")))
  // reset the continuation to the identity function
  | Capture e -> k(eval e env (fun x -> x))
  // wrap current continuation and reset continuation
  | Escape (param, body) -> 
      eval body (env.Add(param, ContV(k))) (fun x -> x)

let env = Map.empty

// capture (2 * escape (\_ -> 42))
let exp = 
  Capture(Mul(Num(2),
    Escape("_", Num(42))))

// capture (2 * escape (\continue -> 17 + continue 4))
let exp1 = 
  Capture(Mul(Num(2),
    Escape("continue", Add(Num(17), (Ap(Id("continue"), Num(4)))))))
    
let result = eval exp1 env (fun x -> x)

// For more information see https://aka.ms/fsharp-console-apps
printfn "%A" result
