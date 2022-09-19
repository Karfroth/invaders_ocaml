type player = {
  x: int;
  y: int
}

type cmd = Left | Right

let update player = function
| Left -> { player with x = player.x - 1 }
| Right -> { player with x = player.x + 1 }