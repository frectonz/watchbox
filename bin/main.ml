open Nottui
module W = Nottui_widgets
module A = Notty.A

type app = { show_idx : int }

let title =
  {|
                               ___                ,---,                                    
                             ,--.'|_            ,--.' |      ,---,                         
           .---.             |  | :,'           |  |  :    ,---.'|      ,---.              
          /. ./|             :  : ' :           :  :  :    |   | :     '   ,'\ ,--,  ,--,  
       .-'-. ' |  ,--.--.  .;__,'  /     ,---.  :  |  |,--.:   : :    /   /   ||'. \/ .`|  
      /___/ \: | /       \ |  |   |     /     \ |  :  '   |:     |,-..   ; ,. :'  \/  / ;  
   .-'.. '   ' ..--.  .-. |:__,'| :    /    / ' |  |   /' :|   : '  |'   | |: : \  \.' /   
  /___/ \:     ' \__\/: . .  '  : |__ .    ' /  '  :  | | ||   |  / :'   | .; :  \  ;  ;   
  .   \  ' .\    ," .--.; |  |  | '.'|'   ; :__ |  |  ' | :'   : |: ||   :    | / \  \  \  
   \   \   ' \ |/  /  ,.  |  ;  :    ;'   | '.'||  :  :_:,'|   | '/ : \   \  /./__;   ;  \ 
    \   \  |--";  :   .'   \ |  ,   / |   :    :|  | ,'    |   :    |  `----' |   :/\  \ ; 
     \   \ |   |  ,     .-./  ---`-'   \   \  / `--''      /    \  /          `---'  `--`  
      '---"     `--`---'                `----'             `-'----'                        
|}
;;

let dirs path =
  path
  |> Sys.readdir
  |> Array.to_list
  |> List.filter (fun entry -> entry <> "." && entry <> "..")
  |> List.filter (fun entry -> Sys.is_directory (path ^ "/" ^ entry))
;;

let attr_of_dir i app = if app.show_idx = i then Some A.(fg blue ++ st bold) else None

let ui_of_dir app i dir =
  let attr = app |> Lwd.map ~f:(attr_of_dir i) in
  attr |> Lwd.map ~f:(fun attr -> W.printf ?attr "[%s]" dir)
;;

let ui_of_dirs app dirs =
  let app = Lwd.get app in
  dirs
  |> Lwd.map ~f:(fun lst -> lst |> List.mapi (ui_of_dir app) |> W.vlist ~bullet:" * ")
;;

let open_random_vid path = Feather.process "mpv" [ path; "--shuffle" ]
let open_from_start_vid path = Feather.process "mpv" [ path ]
let second (_, y) = y

let list_get (i : int) (lst : 'a list) =
  lst |> List.mapi (fun i d -> i, d) |> List.find (fun (idx, _) -> i = idx) |> second
;;

let remove_char str =
  if String.length str > 0 then String.sub str 0 (String.length str - 1) else str
;;

let add_char ch str = str ^ String.make 1 ch

let () =
  if Array.length Sys.argv <> 2
  then print_endline "Usage: <path_to_directory>"
  else (
    let search_input = Lwd.var None in
    let search =
      search_input
      |> Lwd.get
      |> Lwd.map ~f:(function
        | Some s -> W.printf "search: %s" s
        | None -> W.printf "")
    in
    let app = Lwd.var { show_idx = 0 } in
    let path = Sys.argv.(1) in
    let () = Printf.printf "You provided the directory path: %s\n" path in
    let dirs_lst = dirs path |> Lwd.return in
    let dirs_ui =
      ui_of_dirs
        app
        (dirs_lst
         |> Lwd.map ~f:(fun lst ->
           lst |> List.filter (fun dir -> Some dir = (search_input |> Lwd.peek))))
    in
    let title_ui = W.string ~attr:A.(fg blue ++ st bold) title |> Lwd.return in
    let help =
      W.vbox
        [ W.string ~attr:A.(fg blue) "(q) quit" |> Lwd.return
        ; W.string ~attr:A.(fg blue) "(s) open episode from the start" |> Lwd.return
        ; W.string ~attr:A.(fg blue) "(enter) open random episode" |> Lwd.return
        ; W.string ~attr:A.(fg blue) "(/) to search" |> Lwd.return
        ]
    in
    let ui =
      W.vbox
        [ title_ui
        ; Ui.space 0 5 |> Lwd.return
        ; dirs_ui |> Lwd.join
        ; Ui.space 0 5 |> Lwd.return
        ; help
        ; Ui.space 0 2 |> Lwd.return
        ; search
        ]
    in
    let quit_with_q = Lwd.var false in
    let ui =
      ui
      |> Lwd.map ~f:(fun ui ->
        Ui.keyboard_area
          (function
            | `Arrow `Up, _ ->
              let { show_idx } = Lwd.peek app in
              let new_idx = max 0 (show_idx - 1) in
              let new_app = { show_idx = new_idx } in
              Lwd.set app new_app;
              `Handled
            | `Arrow `Down, _ ->
              let { show_idx } = Lwd.peek app in
              let new_idx =
                Lwd.map dirs_lst ~f:(fun dirs_lst ->
                  min (List.length dirs_lst - 1) (show_idx + 1))
              in
              let _ =
                Lwd.map new_idx ~f:(fun new_idx -> Lwd.set app { show_idx = new_idx })
              in
              `Handled
            | `Enter, _ ->
              let { show_idx } = Lwd.peek app in
              let cmd =
                Lwd.map dirs_lst ~f:(fun dirs_lst ->
                  open_random_vid (dirs_lst |> list_get show_idx |> ( ^ ) path))
              in
              let _ = Lwd.map cmd ~f:Feather.run in
              `Handled
            | `ASCII 'q', _ ->
              Lwd.set quit_with_q true;
              `Handled
            | `ASCII 's', _ ->
              let { show_idx } = Lwd.peek app in
              let cmd =
                dirs_lst
                |> Lwd.map ~f:(fun dirs_lst ->
                  open_from_start_vid (dirs_lst |> list_get show_idx |> ( ^ ) path))
              in
              let _ = Lwd.map cmd ~f:Feather.run in
              `Handled
            | `ASCII '/', _ ->
              Lwd.set search_input (Some "");
              `Handled
            | `Escape, _ ->
              Lwd.set search_input None;
              `Handled
            | `Backspace, _ ->
              search_input |> Lwd.peek |> Option.map remove_char |> Lwd.set search_input;
              `Handled
            | `ASCII ch, _ ->
              search_input |> Lwd.peek |> Option.map (add_char ch) |> Lwd.set search_input;
              `Handled
            | _ -> `Unhandled)
          ui)
    in
    Ui_loop.run ui ~quit:quit_with_q ~quit_on_escape:false)
;;
