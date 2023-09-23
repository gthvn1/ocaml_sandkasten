type t = { origin : Vec3.t; direction : Vec3.t }

let create ~(o : Vec3.t) ~(d : Vec3.t) : t = { origin = o; direction = d }
let direction (r : t) = r.direction
let origin (r : t) = r.origin

let hit_sphere (center : Vec3.t) (radius : float) (ray : t) : bool =
  let oc = Vec3.(orig --- center) in
  let dr = direction ray in
  let a : float = Vec3.(dot dr dr) in
  let b : float = Vec3.(2. *. dot oc dr) in
  let c : float = Vec3.dot oc oc -. (radius *. radius) in
  let discriminant = (b *. b) -. (4. *. a *. c) in
  discriminant >= 0.

let ray_color (r : t) =
  if hit_sphere (0., 0., -1.) 0.5 r then Pixel.create ~r:1.0 ~g:0.0 ~b:1.0
  else
    let d = Vec3.unit_vector r.direction in
    let a = (0.5 *. Vec3.y d) +. 1.0 in

    Pixel.create ~r:(1.0 -. a +. (a *. 0.5)) ~g:(1.0 -. a +. (a *. 0.7)) ~b:1.0

let raytrace () =
  (*
   * let's use a 16:9 aspect ratio image
   * It allows to see errors when manipulating x and y axis. Otherwise with 1:1
   * ratio we can mixed value and it seems ok.
   *)
  let aspect_ratio = 16. /. 9. in
  let image_width = 800 in
  let image_height = float_of_int image_width /. aspect_ratio |> int_of_float in
  (* setup camera *)
  let focal_length = 1.0 in
  let viewport_height = 2.0 in
  let viewport_width = viewport_height *. aspect_ratio in
  let camera_center = Vec3.orig in
  (* viewport setup *)
  let viewport_u : Vec3.t = (viewport_width, 0., 0.) in
  let viewport_v : Vec3.t = (0., -.viewport_height, 0.) in
  let pixel_delta_u = Vec3.(viewport_u /// float_of_int image_width) in
  let pixel_delta_v = Vec3.(viewport_v /// float_of_int image_height) in
  (* locate the upper left pixel *)
  let viewport_upper_left =
    Vec3.(
      camera_center +++ (0., 0., focal_length) --- (viewport_u /// 2.)
      --- (viewport_v /// 2.))
  in
  let pixel00_loc =
    Vec3.(viewport_upper_left +++ (0.5 *** (pixel_delta_u +++ pixel_delta_v)))
  in
  "aspect_ratio:        " ^ string_of_float aspect_ratio |> print_endline;
  "image_width:         " ^ string_of_int image_width |> print_endline;
  "image_height:        " ^ string_of_int image_height |> print_endline;
  "viewport_width:      " ^ string_of_float viewport_width |> print_endline;
  "viewport_height:     " ^ string_of_float viewport_height |> print_endline;
  "focal length:        " ^ string_of_float focal_length |> print_endline;
  "camera_center:       " ^ Vec3.string_of_vec3 camera_center |> print_endline;
  "viewport_u:          " ^ Vec3.string_of_vec3 viewport_u |> print_endline;
  "viewport_v:          " ^ Vec3.string_of_vec3 viewport_v |> print_endline;
  "pixel_delta_u:       " ^ Vec3.string_of_vec3 pixel_delta_u |> print_endline;
  "pixel_delta_v:       " ^ Vec3.string_of_vec3 pixel_delta_v |> print_endline;
  "viewport_upper_left: " ^ Vec3.string_of_vec3 viewport_upper_left
  |> print_endline;
  "pixel00_loc:         " ^ Vec3.string_of_vec3 pixel00_loc |> print_endline;
  "==== Start raytracing ====" |> print_endline;
  let image = Image.create ~columns:image_width ~rows:image_height in
  for y = 0 to image_height - 1 do
    for x = 0 to image_width - 1 do
      let fx = float_of_int x in
      let fy = float_of_int y in
      let pixel_center =
        Vec3.(pixel00_loc +++ (fx *** pixel_delta_u) +++ (fy *** pixel_delta_v))
      in
      let ray_direction = Vec3.(pixel_center --- camera_center) in
      let r = create ~o:camera_center ~d:ray_direction in
      let color_pixel = ray_color r in
      image.(y).(x) <- color_pixel
    done
  done;
  image
