(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2022 Woosang Lee <recollect12@gmail.com>                *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)


(* If you delete or rename this file, you should add
   'src/invaders_lib/main.ml' to the 'skip' field in "drom.toml" *)

let frame term =
   let (size_x, size_y) = Notty_lwt.Term.size term in
   Notty.I.tabulate size_x size_y (fun x y ->
      if x > Const.max_col || y > Const.max_row
         then Notty.I.string (Notty.A.bg Notty.A.white) " "
      else Notty.I.void 1 1
      )

type model = {
   player: Player.player;
   shots: Shot.shot list;
   invaders: Army.invaders
}

type exit_type = Quit | Win | Loss

let calculate_hits (m: model) =
   let find_dead_invader (invaders: Army.invaders) (shot: Shot.shot) =
      List.find_opt (fun (i: Army.invader) -> i.x = shot.x && i.y = shot.y) invaders.army in
   let new_invaders, new_shots = List.fold_left (
      fun (curr_invaders, acc_shots) (shot: Shot.shot) ->
            shot
            |> find_dead_invader curr_invaders
            |> Option.map (fun dead -> (
                  Army.update m.invaders (Army.Dead dead),
                  Shot.update shot (Shot.Explode) :: acc_shots
               ))
            |> Option.value ~default: (curr_invaders, shot :: acc_shots)
      ) (m.invaders, []) m.shots in
   { m with invaders = new_invaders; shots = new_shots }

let render t (m: model) =
   let open Notty.I in
   let img_x = Notty.I.string Notty.A.empty "x: " <|> Notty.I.string Notty.A.empty (string_of_int m.player.x)
   and shots_img = m.shots |> List.map Shot.view |> List.fold_left (fun acc x -> acc </> x) (Notty.I.void 0 0)
   and player_img = Player.view m.player
   and enermy_img = Army.view m.invaders in
   let shot_count = Notty.I.string Notty.A.empty "shots: " <|> Notty.I.string Notty.A.empty (string_of_int (List.length m.shots)) in
   Notty_lwt.Term.image t ((img_x <|> shot_count) <-> shots_img </> player_img </> enermy_img </> frame t)

open Lwt.Infix
open Notty
open Notty_lwt

let timer () = Lwt_unix.sleep 999999. >|= fun () -> `Timer
let key_event term = Lwt_stream.get (Term.events term) >|= function
   | Some (`Resize _ | #Unescape.event as x) -> x
   | None -> `End

let schedule_shots () =
   Lwt_unix.sleep 0.1 >|= fun () -> `UpdateShots

let schedule_army () =
   Lwt_unix.sleep 1. >|= fun () -> `UpdateArmy

let rec loop term (e, t, s, a, m) =
   Lwt.choose [e; t; s; a] >>= function
   | `End | `Key (`Escape, []) | `Key (`ASCII 'C', [`Ctrl]) ->
      Lwt.return_unit
   | `Key ( `Arrow `Left, []) ->
      let new_m = { m with player = Player.update m.player Player.Left } in
      render term new_m >>= fun () ->
      loop term (key_event term, t, s, a, new_m)
   | `Key ( `Arrow `Right, []) ->
      let new_m = { m with player = Player.update m.player Player.Right } in
      render term new_m >>= fun () ->
      loop term (key_event term, t, s, a, new_m)
   | `Key ( `ASCII ' ', [] ) ->
      let new_shots: Shot.shot list = if (List.length m.shots) < 2 
         then { x = m.player.x; y = m.player.y - 2; status = Shot.Forwarding } :: m.shots
         else m.shots in
      let new_m = { m with shots = new_shots } in
      render term new_m >>= fun () ->
      loop term (key_event term, t, s, a, new_m)
   | `Timer ->
      loop term (e, timer (), s, a, m)
   | `UpdateShots ->
      let new_shots: Shot.shot list =
         m.shots
         |> List.map (fun x -> Shot.update x Shot.Forward)
         |> List.filter (fun (x: Shot.shot) -> x.y >= 0 && x.status <> Shot.Exploded) in
      let new_m = { m with shots = new_shots } |> calculate_hits in
      render term new_m >>= fun () ->
      loop term (e, t, schedule_shots (), a, new_m)
   | `UpdateArmy ->
      let new_invaders = Army.update m.invaders Army.Move in
      let new_m = { m with invaders = new_invaders } |> calculate_hits in
      if Army.max_y new_m.invaders = new_m.player.y then
         Lwt.return_unit
      else if List.nth_opt new_m.invaders.army 0 |> Option.is_none then
         Lwt.return_unit
      else
         render term new_m >>= fun () ->
         loop term (e, t, s, schedule_army (), new_m)
   | _ ->
      render term m >>= fun () ->
      loop term (key_event term, t, s, a, m)

let main () =
   let t = Term.create ()
   and m: model = {
      player = {x = Const.max_col / 2; y = Const.max_row - 2};
      shots = [];
      invaders = Army.init_invaders Const.max_row Const.max_col;
   } in
   render t m >>= fun () ->
   loop t (key_event t, timer (), schedule_shots(), schedule_army (), m)
