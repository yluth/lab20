
(* Lab 20: Filtering the Mona Lisa *)

open Graphics ;;
open Monalisa ;;

(* images are lists of lists of floats between 0. (white) and 1. (black) *)
type image = float list list ;;

type size = int * int ;;

(* threshold threshold image -- image where pixels above the threshold
value are black *)
let threshold img threshold =
  List.map (fun row -> List.map (fun pix -> if pix <= threshold then 0. else 1.)
                                 row) img
;;

(* dither max image: makes each pixel black or white based on its rgb value *)
let dither img =
  List.map(fun row -> List.map (fun pix -> if pix > Random.float 1.
                                           then 1. else 0.) row) img
;;

(* show the image *)
let depict img =
  open_graph "";
  clear_graph ();
  let x, y = List.length (List.hd img), List.length img in
  resize_window x y;
  let depict_pix v r c =
    let lvl = int_of_float (255. *. (1. -. v)) in
    set_color (rgb lvl lvl lvl);
    plot c (y - r) in
  (* c and y-r are coordinate points; y-r term ensures that image is oriented
     correctly *)
  List.iteri (fun r row ->
    List.iteri (fun c pix -> depict_pix pix r c) row) img;
  Unix.sleep 2;
  close_graph ()
;;

(* print images *)
let _ =
  depict image;
  depict (dither image);
  depict (threshold image 0.75)
;;
