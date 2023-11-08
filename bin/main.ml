open Nottui
module W = Nottui_widgets
module A = Notty.A

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

type app = { show_idx : int }

let dirs dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.filter (fun entry -> entry <> "." && entry <> "..")
  |> List.filter (fun entry -> Sys.is_directory (dir ^ "/" ^ entry))
;;

let ui_of_dirs app dirs =
  dirs
  |> List.mapi (fun i dir ->
    let app = Lwd.get app in
    let attr =
      Lwd.map app ~f:(fun a ->
        if a.show_idx = i then Some A.(fg blue ++ st bold) else None)
    in
    Lwd.map attr ~f:(fun attr -> W.printf ?attr "[%s]" dir))
  |> W.vlist ~bullet:"* "
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
              let got = Lwd.peek app in
              let new_idx = if got.show_idx = 0 then 0 else got.show_idx - 1 in
              let new_app = { show_idx = new_idx } in
              let () = Lwd.set app new_app in
              `Handled
            | `Arrow `Down, _ ->
              let got = Lwd.peek app in
              let new_idx =
                if got.show_idx + 1 = List.length dirs_lst
                then List.length dirs_lst - 1
                else got.show_idx + 1
              in
              let new_app = { show_idx = new_idx } in
              let () = Lwd.set app new_app in
              `Handled
            | `ASCII 'q', _ ->
              let () = Lwd.set quit_with_q true in
              `Handled
            | _ -> `Unhandled)
          ui)
    in
    Ui_loop.run ui ~quit:quit_with_q ~quit_on_escape:true)
;;
