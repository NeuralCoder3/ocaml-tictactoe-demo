open Bogue
module W = Widget
module L = Layout

type btn_state = X | O | Empty
type state = btn_state list list

let zip2d l1 l2 : ('a * 'b) list list =
  List.combine l1 l2
  |> List.map (fun (xs,ys) -> List.combine xs ys)

let playO state : int*int = 
  List.fold_left (fun (p,y) row ->
    fst (List.fold_left (fun (p,x) s ->
      (match p,s with
      | (None,Empty) -> Some (x,y)
      | _ -> p),
      x+1
    ) (p,0) row),
    y+1
  ) (None,0) state
  |> fst
  |> Option.get

let rec transpose = function
  | [] | []:: _ -> []
  | xs -> List.map List.hd xs :: transpose (List.map List.tl xs)

let check_win state : btn_state option =
  let check_row row =
    match row with
    | [X;X;X] -> Some X
    | [O;O;O] -> Some O
    | _ -> None
  in
  let check_diag1 state =
    match state with
    | [[X;_;_];[_;X;_];[_;_;X]] -> Some X
    | [[O;_;_];[_;O;_];[_;_;O]] -> Some O
    | _ -> None
  in
  let check_diag2 state =
    match state with
    | [[_;_;X];[_;X;_];[X;_;_]] -> Some X
    | [[_;_;O];[_;O;_];[O;_;_]] -> Some O
    | _ -> None
  in
  let rows = List.map check_row state in
  let cols = List.map check_row (transpose state) in
  let diags = [check_diag1 state; check_diag2 state] in
  let all = rows @ cols @ diags in
  List.fold_left (fun acc x -> match acc with
    | Some _ -> acc
    | None -> x
  ) None all




let update_buttons (state:state) btns =
  zip2d state btns
  |> List.iter (fun row ->
      List.iter (fun (s,b) ->
        W.set_text b (match s with
          | X -> " X "
          | O -> " O "
          | Empty -> "   ")
      ) row)

let update xxs (x,y) f =
  List.mapi (fun yi row -> List.mapi (fun xi s -> if xi = x && yi = y then f s else s) row) xxs

let handle_win state pane =
  match check_win state with
  | Some X -> (print_endline "X wins!"; L.set_background pane (Some (L.color_bg (Draw.opaque (0,120,255)))))
  | Some O -> (print_endline "O wins!"; L.set_background pane (Some (L.color_bg (Draw.opaque (255,180,0)))))
  | _ -> ()

let handler pane state btns (x,y) _evt = 
  let field = List.nth (List.nth (!state) y) x in
  if field <> Empty then () else
  if check_win (!state) <> None then () else
    (state := update (!state) (x,y) (fun _ -> X);
    handle_win (!state) pane;
    if check_win (!state) = None then (
    state := update (!state) (playO (!state)) (fun _ -> O);
    handle_win (!state) pane;
    ) else ();
    update_buttons (!state) btns;
    ())

let main () =
  let state = 
    (* an array would be more appropriate, but we want to stay true to the original exercise *)
    ref (List.init 3 (fun _ -> List.init 3 (fun _ -> Empty)))
  in
  let buttons = List.map (fun row -> List.map (fun _ -> W.button "") row) (!state) in
  update_buttons (!state) buttons;
  let layout = L.tower (List.map L.flat_of_w buttons) in
  List.iteri (fun y row ->
    List.iteri (fun x b ->
      W.on_click b ~click:(handler layout state buttons (x,y))
    ) row)
  buttons;

  let board = Bogue.of_layout layout in
  Bogue.run board;;

let () = main ();
  Bogue.quit ()
