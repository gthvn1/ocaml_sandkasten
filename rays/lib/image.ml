(*

    int image_width = 256;
    int image_height = 256;

    // Render

    std::cout << "P3\n" << image_width << ' ' << image_height << "\n255\n";

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
}
*)
module P = Pixel

type t = {
  width: int;
  height: int;
  pixels: P.t array array;
}

let create ~width ~height = 
  if width <= 0 then failwith "Width must be strictly positive";
  if height <= 0 then failwith "Height must be strictly positive";
  {
    width = width;
    height = height; 
    pixels = Array.make_matrix width height P.white;
  }

let string_of_image (i:t) : string =
  let header = "P3\n"
              ^ (string_of_int i.width)
              ^ " "
              ^ (string_of_int i.height)
              ^ "\n255\n" in
  let body = "0 0 0" in
  header ^ body
