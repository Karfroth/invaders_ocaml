type shot_status = Forwarding | Exploding | Exploded

type shot = {
  x: int;
  y: int;
  status: shot_status;
}

let max_shots = 5

type cmd = Forward | Explode

let update shot = function
| Forward -> if shot.status = Forwarding then { shot with y = shot.y - 1 } else { shot with status = Exploded }
| Explode -> { shot with y = shot.y - 1; status = Exploding }

let view shot =
  let symbol = Notty.I.string Notty.A.empty (if shot.status = Exploding then "*" else "|") in
  Notty.I.tabulate Const.max_col Const.max_row (fun x y -> if x = shot.x && y = shot.y then symbol else Notty.I.void 1 1 )
