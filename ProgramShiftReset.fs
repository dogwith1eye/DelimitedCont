type Exp =
  | Num of int
  | Id of string
  | Add of Exp * Exp
  | Fun of string * Exp
  | Ap of Exp * Exp
  | Reset of Exp
  | Shift of string * Exp
  
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
  | Fun (param, body) -> k(ClosureV((param, body), env))
  | Ap (f, a) -> eval f env (fun cl -> 
    match cl with
    | ClosureV ((param, body), closureEnv) -> 
        eval a env (fun av -> eval body (closureEnv.Add(param,av)) k)
    | ContV k2 -> eval a env (fun av -> k(k2(av)))
    | _ -> raise (System.ArgumentException("can only apply functions")))
  // reset the continuation to the identity function
  | Reset e -> k(eval e env (fun x -> x))
  // wrap current continuation and reset continuation
  | Shift (param, body) -> 
      eval body (env.Add(param, ContV(k))) (fun x -> x)

let env = Map.empty

let exp = 
  Add(Num(1),
    Reset(Add(Num(2),
      Shift("x", Add(Num(4), Ap(Id("x"), Num(8)))))))

let exp1 = 
  Add(Num(1),
    Reset(Add(Num(2),
      Shift("x", Add(Ap(Id("x"), Num(4)), Ap(Id("x"), Num(8)))))))

let exp2 = 
  Add(Num(1),
    Reset(Add(Num(2),
      Shift("x", Add(Ap(Id("x"), Num(4)), Shift("y", Add(Num(8), Ap(Id("y"), Num(16)))))))))

let exp3 = 
  Add(Num(1),
    Reset(Add(Num(2),
      Shift("x", Add(Ap(Id("x"), Num(4)), Add(Num(8),
    Reset(Add(Num(16),
      Shift("x", Add(Ap(Id("x"), Num(32)), Ap(Id("x"), Num(64))))))))))))

let exp4 = 
  Add(Num(1),
    Reset(Add(Num(2),
      Shift("x", Add(Ap(Id("x"), Num(4)), Add(Num(8),
    Reset(Add(Num(16),
      Shift("y", Add(Ap(Id("y"), Num(32)), Ap(Id("y"), Num(64))))))))))))

let exp5 = 
  Add(Num(1),
    Reset(Add(Num(2),
      Shift("x", Add(Ap(Id("x"), Num(4)), Add(Num(8),
    Reset(Add(Num(16),
      Shift("y", Add(Ap(Id("y"), Num(32)), Ap(Id("x"), Num(64))))))))))))

let result = eval exp5 env (fun x -> x)

// For more information see https://aka.ms/fsharp-console-apps
printfn "%A" result
