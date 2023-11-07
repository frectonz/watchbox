open Nottui
module W = Nottui_widgets

let dirs dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.filter (fun entry -> entry <> "." && entry <> "..")
  |> List.filter (fun entry -> Sys.is_directory (dir ^ "/" ^ entry))
;;

let ui_of_dirs dirs =
  dirs
  |> List.map (fun dir -> Lwd.var (W.printf "[%s]" dir))
  |> List.map Lwd.get
  |> W.vlist
;;

let () =
  if Array.length Sys.argv <> 2
  then print_endline "Usage: <path_to_directory>"
  else (
    let path = Sys.argv.(1) in
    let () = Printf.printf "You provided the directory path: %s\n" path in
    let ui = dirs path |> ui_of_dirs in
    Ui_loop.run ui ~quit_on_escape:true)
;;
