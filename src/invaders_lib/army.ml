type invader = {
  x: int;
  y: int;
}
type direction = Left | Right

type invaders = {
  army: invader list;
  direction: direction;
}

let init_invaders max_rows max_cols =
  let rec setup_cols acc count curr_x =
    if count >= max_cols
      then acc
    else if count mod 2 <> 0 || count < 2 || count > max_cols - 3 
      then setup_cols acc (count + 1) curr_x
    else setup_cols ({ x = count; y = curr_x } :: acc) (count + 1) curr_x
  in
  let rec setup_rows acc count =
    if count > max_rows - 10
      then acc
    else if count mod 2 = 0
      then
        let row = setup_cols [] 0 count in
        setup_rows (row :: acc) (count + 1)
    else setup_rows acc (count + 1)
  in
  {
    army = (setup_rows [] 3) |> List.concat;
    direction = Right;
  }

type cmd = Dead of invader | Move

let max_y invaders = List.fold_left (fun acc i -> if acc > i.y then acc else i.y) (-1) invaders.army

let move_invaders invaders =
  let max_x = List.fold_left (fun acc i -> if acc > i.x then acc else i.x) (-1) invaders.army in
  let min_x = List.fold_left (fun acc i -> if acc < i.x then acc else i.x) Const.max_col invaders.army in
  if max_x >= (Const.max_col - 1) && invaders.direction = Right then
    {
      army = List.map (fun i -> {i with y = i.y + 1}) invaders.army;
      direction = Left
    }
  else if min_x <= 0 && invaders.direction = Left then
    {
      army = List.map (fun i -> {i with y = i.y + 1}) invaders.army;
      direction = Right
    }
  else
    {
      invaders with
        army = List.map (fun i -> {i with x = i.x + (if invaders.direction = Right then 1 else (-1))}) invaders.army
    }

let update invaders = function
| Move -> move_invaders invaders
| Dead dead -> { invaders with army = (List.filter (fun i -> (i.x <> dead.x) || (i.y <> dead.y)) invaders.army) }

let nottyfy army x y =
  let symbol = Notty.I.string Notty.A.empty (if (x + y) mod 2 = 0 then "x" else "+") in
  army
  |> List.find_opt (fun i -> i.x = x && i.y = y)
  |> Option.map (fun _ -> symbol)
  |> Option.value ~default:(Notty.I.void 1 1)

let view (model: invaders) =
  Notty.I.tabulate Const.max_col Const.max_row (fun x y -> nottyfy model.army x y )
