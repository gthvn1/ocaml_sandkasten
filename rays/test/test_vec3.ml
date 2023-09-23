open OUnit2
open Rayslib

let tests =
  let v1 : Vec3.t = (3., 4., 0.) in
  let v2 : Vec3.t = (2., 6., 4.) in
  "unittest for vec3"
  >::: [
         ("origin" >:: fun _ -> assert_equal (0., 0., 0.) Vec3.orig);
         ("length" >:: fun _ -> assert_equal (Vec3.length v1) 5.);
         ("get x" >:: fun _ -> assert_equal (Vec3.x v1) 3.);
         ("get y" >:: fun _ -> assert_equal (Vec3.y v1) 4.);
         ("get z" >:: fun _ -> assert_equal (Vec3.z v1) 0.);
         ("addition" >:: fun _ -> assert_equal Vec3.(v1 +++ v2) (5., 10., 4.));
         ( "substraction" >:: fun _ ->
           assert_equal Vec3.(v1 --- v2) (1., -2., -4.) );
         ( "multiplication" >:: fun _ ->
           assert_equal Vec3.(2. *** v1) (6., 8., 0.) );
         ("division" >:: fun _ -> assert_equal Vec3.(v2 /// 2.) (1., 3., 2.));
       ]

let _ = run_test_tt_main tests
