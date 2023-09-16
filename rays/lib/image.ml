module P = Pixel

type t = { width : int; height : int; pixels : P.t array }

let create ~width ~height =
  if width <= 0 then failwith "Width must be strictly positive";
  if height <= 0 then failwith "Height must be strictly positive";
  { width; height; pixels = Array.make (width * height) P.white }

let fold_pixels s p = s ^ P.string_of_pixel p ^ "\n"

(*
    for (int j = 0; j < image_height; ++j) {
        for (int i = 0; i < image_width; ++i) {
            auto r = double(i) / (image_width-1);
            auto g = double(j) / (image_height-1);
            auto b = 0;

            int ir = static_cast<int>(255.999 * r);
            int ig = static_cast<int>(255.999 * g);
            int ib = static_cast<int>(255.999 * b);

            std::cout << ir << ' ' << ig << ' ' << ib << '\n';
        }
    }
*)
let transform (image : t) =
  let pixels = image.pixels in
  let boundx = image.width - 1 in
  let boundy = image.height - 1 in
  for y = 0 to boundy do
    for x = 0 to boundx do
      let vr = float_of_int x /. float_of_int boundx in
      let vg = float_of_int y /. float_of_int boundy in
      let index = x + (y * image.width) in
      let p =
        P.create
          ~r:(int_of_float (vr *. 255.999))
          ~g:(int_of_float (vg *. 255.999))
          ~b:0
      in
      pixels.(index) <- p
    done
  done

let string_of_image (i : t) : string =
  let header =
    "P3\n" ^ string_of_int i.width ^ " " ^ string_of_int i.height ^ "\n255\n"
  in
  let body = Array.fold_left fold_pixels "" i.pixels in
  header ^ body
