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

let create_color r g b =
  CSS.Color.string_of_t (CSS.Color.rgb r g b);;

let get_context canvas =
  canvas##getContext (Dom_html._2d_);;


let rec loop renderer scene camera mesh cloud_mesh stars_mesh () =
  begin
    renderer##render(scene, camera);
    mesh##rotation##y <- mesh##rotation##y +. (1. /. 100.);
    cloud_mesh##rotation##y <- cloud_mesh##rotation##y +. (1. /. 100.);
    stars_mesh##rotation##y <- stars_mesh##rotation##y +. (1. /. 15000.);
    Dom_html._requestAnimationFrame (Js.wrap_callback (loop renderer scene camera mesh cloud_mesh stars_mesh));
  end;;

let create_scene () =
  Js.Unsafe.new_obj (Js.Unsafe.variable "THREE.Scene")
                    [||];;

let create_renderer () =
  let h = Js.Unsafe.variable "screen.width"
  and w = Js.Unsafe.variable "screen.height" in
  let renderer  = Js.Unsafe.new_obj (Js.Unsafe.variable "THREE.WebGLRenderer")
                                    [||] in
  begin
    renderer##setSize(h, w);
    renderer##antialias <- Js._true;
    let body = Js.Unsafe.variable "document.body" in
    body##appendChild(renderer##domElement);
    renderer
  end;;

let create_object name params =
  Js.Unsafe.new_obj (Js.Unsafe.variable name) params;;

let get_variable name =
  Js.Unsafe.variable name;;

let create_camera () =
  let w = float (Js.Unsafe.variable "screen.width")
  and h = float (Js.Unsafe.variable "screen.height") in
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

let create_ambiant_light color =
  create_object "THREE.AmbientLight" [|color|];;

let create_directional_light color direction =
  let light = create_object "THREE.DirectionalLight" [|color|] in
  begin
    light##castShadow <- Js._true;
    light##shadowCameraNear <- 0.01;
    light##shadowCameraFar <- 15;
    light##shadowCameraFov <- 45;
    light##shadowCameraLeft <- -1;
    light##shadowCameraRight <- 1;
    light##shadowCameraTop <- 1;
    light##shadowCameraBottom <- -1;
   (* light.shadowCameraVisible <- true; *)
    light##shadowBias <- 0.001;
    light##shadowDarkness <- 0.2;
    light##shadowMapWidth <- 1024;
    light##shadowMapHeight <- 1024;
    light
  end;;

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

let create_stars_material () =
  let material = create_object "THREE.MeshBasicMaterial" [||] in
  begin
    material##map <- load_texture "textures/galaxy_starfield.png";
    material##side <- get_variable "THREE.BackSide";
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
                                            let earth_geometry = create_sphere 0.5 32 32 in
                                            let earth_material = create_material () in
                                            let earth_mesh = create_mesh earth_geometry earth_material in
                                            let cloud_geometry = create_sphere 0.51 32 32 in
                                            let cloud_material = create_cloud_material () in
                                            let cloud_mesh = create_mesh cloud_geometry cloud_material in
                                            let stars_geometry = create_sphere 2 32 32 in
                                            let stars_material = create_stars_material () in
                                            let stars_mesh = create_mesh stars_geometry stars_material in
                                            let ambiant_light = create_ambiant_light
                                                                  (Js.Unsafe.inject 0x222222) in
                                            let directional_light = create_directional_light
                                                                      (Js.Unsafe.inject 0xffffff)
                                                                      (Js.Unsafe.inject 1) in
                                            move_light directional_light 5 5 5;
                                            earth_mesh##add(cloud_mesh);
                                            earth_material##map <- load_texture
                                                                     "textures/earthmap1k.jpg";
                                            earth_material##bumpMap <- load_texture
                                                                         "textures/earthbump1k.jpg";
                                            earth_material##bumpScale <- 0.65;
                                            earth_material##specularMap <- load_texture
                                                                             "textures/earthspec1k.jpg";
                                            earth_material##specular <- create_color "gray";
                                            renderer##alpha <- Js._true;
                                            renderer##setClearColor(0xffffff, 1);
                                            scene##add(earth_mesh);
                                            scene##add(stars_mesh);
                                            scene##add(ambiant_light);
                                            scene##add(directional_light);
                                            debug "ocaml END";
                                            Dom_html._requestAnimationFrame
                                              (Js.wrap_callback
                                                 (loop renderer scene camera earth_mesh cloud_mesh stars_mesh));
                                            Js._true
                                          end);;
