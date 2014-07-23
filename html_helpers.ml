let create_object name params =
  Js.Unsafe.new_obj (Js.Unsafe.variable name) params;;

let get_variable name =
  Js.Unsafe.variable name;;

let by_id_coerce s f  =
  Js.Opt.get
    (f (Dom_html.getElementById s))
    (fun () -> raise Not_found);;

let get_input id =
  by_id_coerce id Dom_html.CoerceTo.input;;

let get_button id =
  by_id_coerce id Dom_html.CoerceTo.button;;

let create_color r g b =
  CSS.Color.string_of_t (CSS.Color.rgb r g b);;

let get_context canvas =
  canvas##getContext (Dom_html._2d_);;

let get_button id =
  by_id_coerce id Dom_html.CoerceTo.button;;

let set_button_click_event button f =
  button##onclick <-
    Dom_html.handler
      (fun ev ->
       begin
         f ();
         Js._true;
       end);;
