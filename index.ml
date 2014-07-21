let pi = 4.0 *. atan 1.0;;

let deg_of_rad teta = teta *. (200. /. pi);;
let rad_of_deg teta = teta *. (pi /. 200.);;

let error f = Printf.ksprintf (fun s -> Firebug.console##error (Js.string s); failwith s) f
let debug f = Printf.ksprintf (fun s -> Firebug.console##log(Js.string s)) f
let alert f = Printf.ksprintf (fun s -> Dom_html.window##alert(Js.string s); failwith s) f

let by_id_coerce s f  =
  Js.Opt.get
    (f (Dom_html.getElementById s))
    (fun () -> raise Not_found);;

let get_input id =
  by_id_coerce id Dom_html.CoerceTo.input;;

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

let create_canvas w h =
  let d = Dom_html.window##document in
  let c = Dom_html.createCanvas d in
  c##width <- w;
  c##height <- h;
  Dom.appendChild Dom_html.window##document##body c;
  c;;

let create_color r g b =
  CSS.Color.string_of_t (CSS.Color.rgb r g b);;

let get_context canvas =
  canvas##getContext (Dom_html._2d_);;


let rec loop renderer scene camera mesh cloud_mesh () =
  begin
    renderer##render(scene, camera);
    mesh##rotation##y <- mesh##rotation##y +. (1. /. 64.);
    cloud_mesh##rotation##y <- cloud_mesh##rotation##y +. (1. /. 63.);
    Dom_html._requestAnimationFrame (Js.wrap_callback (loop renderer scene camera mesh cloud_mesh));
  end;;

let create_scene () =
  Js.Unsafe.new_obj (Js.Unsafe.variable "THREE.Scene")
                    [||];;

let create_renderer () =
  let h = Js.Unsafe.variable "window.innerWidth"
  and w = Js.Unsafe.variable "window.innerHeight" in
  let renderer  = Js.Unsafe.new_obj (Js.Unsafe.variable "THREE.WebGLRenderer")
                                    [||] in
  begin
    renderer##setSize(h, w);
    let body = Js.Unsafe.variable "document.body" in
    body##appendChild(renderer##domElement);
    renderer
  end;;

let create_object name params =
    Js.Unsafe.new_obj (Js.Unsafe.variable name) params;;

let get_variable name =
  Js.Unsafe.variable name;;

let create_camera () =
  let w = float (Js.Unsafe.variable "window.innerWidth")
  and h = float (Js.Unsafe.variable "window.innerHeight") in
  let camera =
    Js.Unsafe.new_obj (Js.Unsafe.variable "THREE.PerspectiveCamera")
                      [|Js.Unsafe.inject 0.2;
                        Js.Unsafe.inject (w /. h);
                        Js.Unsafe.inject 1.;
                        Js.Unsafe.inject 1000.|] in
  begin
    camera##position##z <- 500;
    camera
  end;;

(** [create_sphere r h w] create a sphere geometry of radius [r], with [h,w] polygones *)
let create_sphere r h w  =
  Js.Unsafe.new_obj (Js.Unsafe.variable "THREE.SphereGeometry")
                    [|Js.Unsafe.inject r; Js.Unsafe.inject h; Js.Unsafe.inject w|];;

let create_light color =
  Js.Unsafe.new_obj (Js.Unsafe.variable "THREE.SpotLight") [|color|];;

let move_light light x y z =
  light##position##set(x, y, z);;

let create_material () =
  Js.Unsafe.new_obj (Js.Unsafe.variable "THREE.MeshPhongMaterial") [||];;

let load_texture path =
  Js.Unsafe.fun_call (Js.Unsafe.variable "THREE.ImageUtils.loadTexture") [|Js.Unsafe.inject path|];;

let create_cloud_material () =
  let material = create_object "THREE.MeshPhongMaterial" [||] in
  begin
    material##map <- load_texture "textures/earthcloudmap.jpg";
    material##side <- get_variable "THREE.DoubleSide";
    material##opacity <- 0.4;
    material##transparent <- Js._true;
    material##depthWrite <- Js._false;
    material
  end;;

let create_mesh geometry meterial =
  Js.Unsafe.new_obj (Js.Unsafe.variable "THREE.Mesh")
                    [|Js.Unsafe.inject geometry;Js.Unsafe.inject meterial|];;

let create_color color =
  create_object "THREE.Color" [|Js.Unsafe.inject color|];;

Dom_html.window##onload <- Dom.handler (fun _ ->
                                        begin
                                          debug "ocaml START";
                                          let renderer = create_renderer () in
                                          let camera = create_camera () in
                                          let scene = create_scene () in
                                          let geometry = create_sphere 0.5 32 32 in
                                          let material = create_material () in
                                          let mesh = create_mesh geometry material in
                                          let light = create_light (Js.Unsafe.inject 0x404040) in
                                          let cloud_geometry = create_sphere 0.51 32 32 in
                                          let cloud_material = create_cloud_material () in
                                          let cloud_mesh = create_mesh cloud_geometry cloud_material in
                                          mesh##add(cloud_mesh);
                                          move_light light 10 10 15;
                                          light##intensity <- 4;
                                          material##map <- load_texture "textures/earthmap1k.jpg";
                                          material##bumpMap <- load_texture "textures/earthbump1k.jpg";
                                          material##bumpScale <- 0.65;
                                          material##specularMap <- load_texture "textures/earthspec1k.jpg";
                                          material##specular <- create_color "gray";
                                          renderer##alpha <- Js._true;
                                          renderer##setClearColor(0xffffff, 1);
                                          scene##add(mesh);
                                          scene##add(light);
                                          debug "ocaml END";
                                          Dom_html._requestAnimationFrame
                                            (Js.wrap_callback
                                               (loop renderer scene camera mesh cloud_mesh));
                                          Js._true
                                        end);;
