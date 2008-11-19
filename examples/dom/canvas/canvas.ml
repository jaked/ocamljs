module D = Dom

let onload () =
  let canvas = (D.document#getElementById "canvas" : D.canvas) in
  let ctx = canvas#getContext "2d" in

  ctx#_set_fillStyle "rgb(200,0,0)";
  ctx#fillRect 10. 10. 55. 50.;

  ctx#_set_fillStyle "rgba(0,0,200,0.5)";
  ctx#fillRect 30. 30. 55. 50.;

  ctx#beginPath;
  ctx#moveTo 75. 25.;
  ctx#quadraticCurveTo 25. 25. 25. 62.5;
  ctx#quadraticCurveTo 25. 100. 50. 100.;
  ctx#quadraticCurveTo 50. 120. 30. 125.;
  ctx#quadraticCurveTo 60. 120. 65. 100.;
  ctx#quadraticCurveTo 125. 100. 125. 62.5;
  ctx#quadraticCurveTo 125. 25. 75. 25.;
  ctx#stroke

;;

D.window#_set_onload (Ocamljs.jsfun onload)
