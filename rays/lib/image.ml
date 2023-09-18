module P = Pixel

(*
 * PPM image is a matrix of pixels.
 * Image 3 x 2
 *   => 3 columns / 2 rows
 *   =>
 *      +--------+--------+--------+
 *      | (0, 0) | (0, 1) | (0, 2) | => row 0 (x == 0)
 *      +--------+--------+--------+
 *      | (1, 0) | (1, 1) | (1, 2) | => row 1 (x == 1)
 *      +--------+--------+--------+
 *        col 0    col 1    col 2
 *        y = 0    y = 1    y = 2
 *
 *)
type t = P.t array array

let create ~(columns : int) ~(rows : int) : t =
  assert (columns > 0 && rows > 0);
  Array.make_matrix rows columns P.white

let get_rows image = Array.length image

let get_columns image =
  assert (Array.length image > 0);
  Array.length image.(0)

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
  let rows = get_rows image in
  let cols = get_columns image in
  for y = 0 to cols - 1 do
    for x = 0 to rows - 1 do
      let p =
        P.create
          ~r:(float_of_int y /. float_of_int (cols - 1))
          ~g:(float_of_int x /. float_of_int (rows - 1))
          ~b:0.
      in
      image.(y).(x) <- p
    done
  done

let string_of_array arr =
  let rec str_of_a i a =
    if i = Array.length a - 1 then P.string_of_pixel a.(i)
    else P.string_of_pixel a.(i) ^ "  " ^ str_of_a (i + 1) a
  in
  str_of_a 0 arr

let string_of_matrix matrix =
  let rows = Array.length matrix in
  let rec str_of_m i m =
    if i = rows - 1 then string_of_array m.(i)
    else string_of_array m.(i) ^ "\n" ^ str_of_m (i + 1) m
  in
  str_of_m 0 matrix

let string_of_ppm (i : t) : string =
  let rows = Array.length i in
  let cols = Array.length i.(0) in
  let header =
    "P3\n" ^ string_of_int cols ^ " " ^ string_of_int rows ^ "\n255\n"
  in
  let body = string_of_matrix i in
  header ^ body
