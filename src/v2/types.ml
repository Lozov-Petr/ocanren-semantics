open State
open Goal

type stream = Nil | Cons of state * stream | Thunk of computation

and computation =
  | Goal of goal * state
  | Mplus of computation * computation
  | Bind of computation * goal
  | Force of computation
  | Stream of stream

type step_trace = computation * state option
type trace = step_trace list

let nil = Nil
let cons state stream = Cons (state, stream)
let single s = cons s nil
let thunk c = Thunk c
let goal g s = Goal (g, s)
let mplus c1 c2 = Mplus (c1, c2)
let bind c g = Bind (c, g)
let force c = Force c
let stream s = Stream s
let init_comp g = goal g init_state

module Printing = struct
  open Format

  let rec print_stream fmt = function
    | Nil -> fprintf fmt "%s" "Nil"
    | Cons (hd, tl) ->
        fprintf fmt "Cons@\n  @[%a@\n%a@]" print_state hd print_stream tl
    | Thunk c -> fprintf fmt "Thunk@\n  @[%a@]" print_computation c

  and print_computation fmt = function
    | Goal (g, st) ->
        fprintf fmt "< @[@\n%a,@\n%a@]@\n>" print_goal g print_state st
    | Mplus (c1, c2) ->
        fprintf fmt "mplus@\n  @[%a@\n%a@]" print_computation c1
          print_computation c2
    | Bind (c, g) ->
        fprintf fmt "bind@\n  @[%a@\n%a@]" print_computation c print_goal g
    | Force c -> fprintf fmt "force@\n  @[%a@]" print_computation c
    | Stream s -> print_stream fmt s

  let print_step_trace : formatter -> step_trace -> unit =
   fun fmt -> function
    | c, None -> fprintf fmt "%a@\n@\nNo answer@\n" print_computation c
    | c, Some a ->
        fprintf fmt "%a@\n@\nAnswer:@\n  @[%a@]@\n" print_computation c
          print_state a

  let print_step_trace_with_index fmt (index, trace) =
    fprintf fmt
      "----------------------------------------------@\n@\nStep %d:@\n  @[%a@]"
      index print_step_trace trace

  let print_trace : formatter -> trace -> unit =
   fun fmt trace ->
    pp_print_list
      ~pp_sep:(fun fmt () -> fprintf fmt "@\n")
      print_step_trace_with_index fmt
    @@ List.mapi (fun i t -> (i, t)) trace
end

let print_trace :
    (environment -> int -> goal -> trace) -> environment -> int -> goal -> unit
    =
 fun eval env n goal ->
  let res = eval env n goal in
  Format.printf "%a\n%!" Printing.print_trace res
