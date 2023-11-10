open Nottui
module W = Nottui_widgets
module A = Notty.A

type app = { show_idx : int }
type dir = string
type attrs = Notty.A.t option

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

let dirs (path : dir) : dir list =
  path
  |> Sys.readdir
  |> Array.to_list
  |> List.filter (fun entry -> entry <> "." && entry <> "..")
  |> List.filter (fun entry -> Sys.is_directory (path ^ "/" ^ entry))
;;

let attr_of_dir (i : int) ({ show_idx } : app) : attrs =
  if show_idx = i then Some A.(fg blue ++ st bold) else None
;;

let ui_of_dir (app : app Lwd.t) (i : int) (dir : dir) : Nottui.ui Lwd.t =
  let attr = Lwd.map app ~f:(attr_of_dir i) in
  Lwd.map attr ~f:(fun attr -> W.printf ?attr "[%s]" dir)
;;

let ui_of_dirs (app : app Lwd.var) (dirs : dir list) : Nottui.ui Lwd.t =
  let app = Lwd.get app in
  dirs |> List.mapi (ui_of_dir app) |> W.vlist ~bullet:"* "
;;

let open_random_vid path = Feather.process "mpv" [ path; "--shuffle" ]
let second (_, y) = y

let list_get (i : int) (lst : 'a list) =
  lst |> List.mapi (fun i d -> i, d) |> List.find (fun (idx, _) -> i = idx) |> second
;;

let () =
  if Array.length Sys.argv <> 2
  then print_endline "Usage: <path_to_directory>"
  else (
    let app = Lwd.var { show_idx = 0 } in
    let path = Sys.argv.(1) in
    let () = Printf.printf "You provided the directory path: %s\n" path in
    let dirs_lst = dirs path in
    let dirs_ui = ui_of_dirs app dirs_lst in
    let title_ui = W.string ~attr:A.(fg blue ++ st bold) title |> Lwd.var |> Lwd.get in
    let ui = W.vbox [ title_ui; dirs_ui ] in
    let quit_with_q = Lwd.var false in
    let ui =
      Lwd.map ui ~f:(fun ui ->
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
              let new_idx = min (List.length dirs_lst - 1) (show_idx + 1) in
              let new_app = { show_idx = new_idx } in
              Lwd.set app new_app;
              `Handled
            | `ASCII 'q', _ ->
              Lwd.set quit_with_q true;
              `Handled
            | `Enter, _ ->
              let { show_idx } = Lwd.peek app in
              let cmd = open_random_vid (dirs_lst |> list_get show_idx |> ( ^ ) path) in
              Feather.run cmd;
              `Handled
            | _ -> `Unhandled)
          ui)
    in
    Ui_loop.run ui ~quit:quit_with_q ~quit_on_escape:true)
;;
