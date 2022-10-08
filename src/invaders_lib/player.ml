type player = {
  x: int;
  y: int
}

type cmd = Left | Right

let update player = function
| Left -> { player with x = max 0 (player.x - 1) }
| Right -> { player with x = min (Const.max_col - 1) (player.x + 1) }

let view (player: player) =
  Notty.I.tabulate Const.max_col (Const.max_row - 1) (fun x y ->
    if x = player.x && y = player.y then (Notty.I.string Notty.A.empty "A")
    else Notty.I.void 1 1 )