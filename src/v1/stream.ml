open State
open Goal

(*****************************************************************************************)

type stream =
  | Nil
  | Leaf of goal * state
  | Mplus of stream * stream
  | Bind of stream * goal

type step_trace = state option * stream
type trace = step_trace list

(*****************************************************************************************)
module Printing = struct
  open Format

  let rec print_stream fmt = function
    | Nil -> fprintf fmt "%s" "nil"
    | Leaf (g, st) ->
        fprintf fmt "< @[@\n%a,@\n%a@]@\n>" print_goal g print_state st
    | Mplus (s1, s2) ->
        fprintf fmt "mplus@\n  @[%a@\n%a@]" print_stream s1 print_stream s2
    | Bind (s, g) ->
        fprintf fmt "bind@\n  @[%a@\n%a@]" print_stream s print_goal g

  let print_step_trace fmt = function
    | None, s -> fprintf fmt "No answer, @\n%a@\n" print_stream s
    | Some a, s ->
        fprintf fmt "Answer:@\n  @[%a,@] @\n%a@\n" print_state a print_stream s

  let print_step_trace_with_index fmt (index, trace) =
    fprintf fmt
      "----------------------------------------------@\n@\nStep %d:@\n  @[%a@]"
      index print_step_trace trace

  let print_trace fmt trace =
    pp_print_list
      ~pp_sep:(fun fmt () -> fprintf fmt "@\n")
      print_step_trace_with_index fmt
    @@ List.mapi (fun i t -> (i, t)) trace
end
(*****************************************************************************************)

let init_state : state = ([], 0)
let init_stream : goal -> stream = fun g -> Leaf (g, init_state)

(*****************************************************************************************)
let print_trace :
    (environment -> int -> stream -> trace) ->
    environment ->
    int ->
    goal ->
    unit =
 fun eval env n goal ->
  let res = eval env n @@ init_stream goal in
  Format.printf "%a\n%!" Printing.print_trace res
