open Closure
open Common

type world = { code : string; nlam : int }

let new_world = { code = ""; nlam = 0 }

let (>>=) l r =
  fun w ->
  match l w with
    None -> None
  | Some (v, w) -> r v w

let (let*) = (>>=)

let (>>) l r =
  fun w ->
  match l w with
    None -> None
  | Some w -> r w

let return v =
  fun w -> Some (v, w)

let fail =
  fun _ -> None

let add_code v { code; nlam } =
  Some { code = code ^ v; nlam }

let new_lam () { code; nlam } =
  Some ("lam" ^ string_of_int nlam, {code; nlam = nlam + 1})

let compile_lit = function
    Syntax.Int n -> return $ string_of_int n
  | Syntax.Bool true -> return "1"
  | Syntax.Bool false -> return "0"

let rec compile_expr = function
    Lit c -> compile_lit c
  | Arg -> return "arg"
  | GVar v -> return $ Printf.sprintf "__%s" v
  | Dup (_, e) -> compile_expr e (* TODO - manage drop and dup *)
  | Drop (_, e) -> compile_expr e
  | Env n -> return $ Printf.sprintf "(env[%d])" n
