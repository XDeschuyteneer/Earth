open Trigonometry
open Logs
open Html_helpers

let rec loop renderer scene camera current_mesh cloud_mesh stars_mesh () =
  begin
    debug "render loop";
    renderer##render(scene, camera);
    !(current_mesh)##rotation##y <- !(current_mesh)##rotation##y +. (1. /. 100.);
    cloud_mesh##rotation##y <- cloud_mesh##rotation##y +. (1. /. 100.);
    stars_mesh##rotation##y <- stars_mesh##rotation##y +. (1. /. 15000.);
    Dom_html._requestAnimationFrame (Js.wrap_callback (loop renderer scene camera current_mesh cloud_mesh stars_mesh));
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
                                            let earth_button = get_button "earth_button" in
                                            let jupiter_button = get_button "jupiter_button" in
                                            let renderer = create_renderer () in
                                            let camera = create_camera () in
                                            let scene = create_scene () in
                                            let jupiter_geometry = create_sphere 0.8 32 32 in
                                            let jupiter_material = create_material () in
                                            jupiter_material##map <-
                                              load_texture
                                                "textures/jupiter2_1k.jpg";
                                            let jupiter_mesh = create_mesh jupiter_geometry jupiter_material in
                                            let earth_geometry = create_sphere 0.08 32 32 in
                                            let earth_material = create_material () in
                                            let earth_mesh = create_mesh earth_geometry earth_material in
                                            let cloud_geometry = create_sphere 0.081 32 32 in
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
                                            (* drawing parameters *)
                                            move_light directional_light 5 5 5;
                                            (* earth_mesh##add(cloud_mesh); *)
                                            earth_material##map <- load_texture
                                                                     "textures/earthmap1k.jpg";
                                            earth_material##bumpMap <- load_texture
                                                                         "textures/earthbump1k.jpg";
                                            earth_material##bumpScale <- 0.65;
                                            earth_material##specularMap <- load_texture
                                                                             "textures/earthspec1k.jpg";
                                            earth_material##specular <- create_color "gray";
                                            let current_mesh = ref jupiter_mesh in
                                            renderer##alpha <- Js._true;
                                            renderer##setClearColor(0xffffff, 1);
                                            scene##add(!current_mesh);
                                            scene##add(stars_mesh);
                                            scene##add(ambiant_light);
                                            scene##add(directional_light);
                                            (* button handling *)
                                            set_button_click_event earth_button (fun () ->
                                                                                 begin
                                                                                   scene##remove(jupiter_mesh);
                                                                                   scene##add(earth_mesh);
                                                                                   current_mesh := earth_mesh;
                                                                                   Js._true;
                                                                                 end);
                                            set_button_click_event jupiter_button (fun () ->
                                                                                 begin
                                                                                   scene##remove(earth_mesh);
                                                                                   scene##add(jupiter_mesh);
                                                                                   current_mesh := jupiter_mesh;
                                                                                   Js._true;
                                                                                 end);
                                            (* loop running *)
                                            Dom_html._requestAnimationFrame
                                              (Js.wrap_callback
                                                 (loop renderer scene camera current_mesh cloud_mesh stars_mesh));
                                            Js._true
                                          end);;
