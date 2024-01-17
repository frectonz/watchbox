open Nottui
module W = Nottui_widgets
module A = Notty.A

module List = struct
  include List

  let first = function
    | [] -> None
    | x :: _ -> Some x
  ;;

  let rec last = function
    | [] -> None
    | [ x ] -> Some x
    | _ :: rest -> last rest
  ;;
end

module Helpers = struct
  let dirs path =
    path
    |> Sys.readdir
    |> Array.to_list
    |> List.filter (fun entry -> entry <> "." && entry <> "..")
    |> List.filter (fun entry -> Sys.is_directory (path ^ "/" ^ entry))
  ;;

  let contains term =
    let contains' s1 s2 =
      let s1 = String.lowercase_ascii s1 in
      let s2 = String.lowercase_ascii s2 in
      let re = Str.regexp_string s1 in
      try
        ignore (Str.search_forward re s2 0);
        true
      with
      | Not_found -> false
    in
    List.filter (contains' term)
  ;;

  let equal s1 s2 =
    String.equal (s1 |> String.lowercase_ascii) (s2 |> String.lowercase_ascii)
  ;;

  let add_char ch str = str ^ String.make 1 ch

  let remove_last_char str =
    if String.length str > 0 then String.sub str 0 (String.length str - 1) else str
  ;;

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
end

module App = struct
  type app =
    { path : string
    ; dirs : string list
    ; selected : string option
    ; search_term : string option
    }

  let make path =
    let dirs = Helpers.dirs path in
    match dirs with
    | [] -> Error "directory is empty"
    | hd :: _ -> Ok ({ path; dirs; selected = Some hd; search_term = None } |> Lwd.var)
  ;;

  let search app =
    app
    |> Lwd.get
    |> Lwd.map ~f:(fun { search_term; _ } -> search_term)
    |> Lwd.map ~f:(function
      | Some s -> W.printf "search: %s" s
      | None -> W.printf "")
  ;;

  let dirs app =
    app
    |> Lwd.get
    |> Lwd.map ~f:(fun { dirs; search_term; selected; _ } -> dirs, search_term, selected)
    |> Lwd.map ~f:(fun (dirs, search_term, selected) ->
      match search_term with
      | Some search -> Helpers.contains search dirs, selected
      | None -> dirs, selected)
    |> Lwd.map ~f:(fun (dirs, selected) ->
      List.map
        (fun dir ->
          if selected |> Option.map (Helpers.equal dir) |> Option.value ~default:false
          then Some A.(fg blue ++ st bold), dir
          else None, dir)
        dirs)
    |> Lwd.map ~f:(List.map (fun (attr, dir) -> W.printf ?attr "[%s]" dir))
    |> Lwd.map ~f:(List.map Lwd.return)
    |> Lwd.map ~f:(W.vlist ~bullet:" * ")
    |> Lwd.join
  ;;

  let title = Helpers.title |> W.string ~attr:A.(fg blue ++ st bold) |> Lwd.return

  let help =
    W.vbox
      [ W.string ~attr:A.(fg blue) "(q) quit" |> Lwd.return
      ; W.string ~attr:A.(fg blue) "(s) open episode from the start" |> Lwd.return
      ; W.string ~attr:A.(fg blue) "(enter) open random episode" |> Lwd.return
      ; W.string ~attr:A.(fg blue) "(/) to search" |> Lwd.return
      ]
  ;;

  let gap app =
    app
    |> Lwd.get
    |> Lwd.map ~f:(fun { dirs; search_term; _ } ->
      ( List.length dirs
      , match search_term with
        | Some search -> Helpers.contains search dirs |> List.length
        | None -> List.length dirs ))
    |> Lwd.map ~f:(fun (all, filtered) -> all - filtered)
    |> Lwd.map ~f:(Ui.space 0)
  ;;

  let render app =
    W.vbox
      [ title
      ; Ui.space 0 1 |> Lwd.return
      ; dirs app
      ; gap app
      ; Ui.space 0 1 |> Lwd.return
      ; help
      ; Ui.space 0 1 |> Lwd.return
      ; search app
      ]
  ;;

  let move_up app =
    let a = Lwd.peek app in
    let a =
      { a with
        dirs =
          a.search_term
          |> Option.map (fun search -> Helpers.contains search a.dirs)
          |> Option.value ~default:a.dirs
      }
    in
    let pos =
      Option.bind a.selected (fun s -> List.find_index (Helpers.equal s) a.dirs)
    in
    let new_app =
      { a with
        selected =
          (match pos with
           | Some 0 -> List.last a.dirs
           | Some x -> List.nth_opt a.dirs (x - 1)
           | None -> List.first a.dirs)
      }
    in
    Lwd.set app new_app
  ;;

  let move_down app =
    let a = Lwd.peek app in
    let a =
      { a with
        dirs =
          a.search_term
          |> Option.map (fun search -> Helpers.contains search a.dirs)
          |> Option.value ~default:a.dirs
      }
    in
    let pos =
      Option.bind a.selected (fun s -> List.find_index (Helpers.equal s) a.dirs)
    in
    let new_app =
      { a with
        selected =
          (match pos with
           | Some x when x = List.length a.dirs - 1 -> List.first a.dirs
           | Some x -> List.nth_opt a.dirs (x + 1)
           | None -> List.first a.dirs)
      }
    in
    Lwd.set app new_app
  ;;

  let open_random app =
    let app = Lwd.peek app in
    let path = app.selected |> Option.map (fun selected -> app.path ^ selected) in
    match path with
    | Some path -> Feather.process "mpv" [ path; "--shuffle" ] |> Feather.run
    | None -> ()
  ;;

  let open_from_start app =
    let app = Lwd.peek app in
    let path = app.selected |> Option.map (fun selected -> app.path ^ selected) in
    match path with
    | Some path -> Feather.process "mpv" [ path ] |> Feather.run
    | None -> ()
  ;;

  let enable_search app =
    let a = Lwd.peek app in
    let new_app = { a with search_term = Some "" } in
    Lwd.set app new_app
  ;;

  let disable_search app =
    let a = Lwd.peek app in
    let new_app = { a with search_term = None } in
    Lwd.set app new_app
  ;;

  let add_char_to_search_term app char =
    let a = Lwd.peek app in
    let new_app =
      { a with search_term = a.search_term |> Option.map (Helpers.add_char char) }
    in
    let new_app =
      match new_app.search_term with
      | Some search ->
        { new_app with selected = Helpers.contains search new_app.dirs |> List.first }
      | None -> new_app
    in
    Lwd.set app new_app
  ;;

  let remove_char_from_search_term app =
    let a = Lwd.peek app in
    let new_app =
      { a with search_term = a.search_term |> Option.map Helpers.remove_last_char }
    in
    let new_app =
      match new_app.search_term with
      | Some search ->
        { new_app with selected = Helpers.contains search new_app.dirs |> List.first }
      | None -> new_app
    in
    Lwd.set app new_app
  ;;

  let with_actions app =
    let quit_with_q = Lwd.var false in
    let action_handler = function
      | `Arrow `Up, _ ->
        move_up app;
        `Handled
      | `Arrow `Down, _ ->
        move_down app;
        `Handled
      | `Enter, _ ->
        open_random app;
        `Handled
      | `ASCII 's', _ ->
        open_from_start app;
        `Handled
      | `ASCII '/', _ ->
        enable_search app;
        `Handled
      | `Escape, _ ->
        disable_search app;
        `Handled
      | `ASCII 'q', _ ->
        Lwd.set quit_with_q true;
        `Handled
      | `Backspace, _ ->
        remove_char_from_search_term app;
        `Handled
      | `ASCII ch, _ ->
        add_char_to_search_term app ch;
        `Handled
      | _ -> `Unhandled
    in
    let ui = render app |> Lwd.map ~f:(fun ui -> Ui.keyboard_area action_handler ui) in
    Ui_loop.run ui ~quit:quit_with_q ~quit_on_escape:false
  ;;
end

let () =
  if Array.length Sys.argv <> 2
  then print_endline "Usage: <path_to_directory>"
  else (
    let path = Sys.argv.(1) in
    match App.make path with
    | Ok app -> App.with_actions app
    | Error err -> print_endline err)
;;
