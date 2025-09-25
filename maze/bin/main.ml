module D = Maze.Draw
module G = Maze.Game

let () =
  let board, state = "maze.txt" |> G.of_file in
  D.init_window board ;
  let rec loop s =
    D.render board s ;
    match D.read_key () with
    | 'h' ->
        G.move_left ~board ~state:s |> loop
    | 'j' ->
        G.move_down ~board ~state:s |> loop
    | 'k' ->
        G.move_up ~board ~state:s |> loop
    | 'l' ->
        G.move_right ~board ~state:s |> loop
    | 'q' ->
        D.close_window ()
    | c ->
        Printf.printf "%c pressed\n%!" c ;
        loop s
  in
  loop state
