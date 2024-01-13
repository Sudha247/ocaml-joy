open Base

(* borrowed from
   https://gist.githubusercontent.com/tjammer/509981fed4d50683cdb800da5bf16ab1/raw/da2b8cc86718ef7e93e2e2c707dcfe443809d7cc/simplex.ml *)
let permutation =
  [|
    151; 160; 137; 91; 90; 15; 131; 13; 201; 95; 96; 53; 194; 233; 7; 225; 140;
    36; 103; 30; 69; 142; 8; 99; 37; 240; 21; 10; 23; 190; 6; 148; 247; 120;
    234; 75; 0; 26; 197; 62; 94; 252; 219; 203; 117; 35; 11; 32; 57; 177; 33;
    88; 237; 149; 56; 87; 174; 20; 125; 136; 171; 168; 68; 175; 74; 165; 71;
    134; 139; 48; 27; 166; 77; 146; 158; 231; 83; 111; 229; 122; 60; 211; 133;
    230; 220; 105; 92; 41; 55; 46; 245; 40; 244; 102; 143; 54; 65; 25; 63; 161;
    1; 216; 80; 73; 209; 76; 132; 187; 208; 89; 18; 169; 200; 196; 135; 130;
    116; 188; 159; 86; 164; 100; 109; 198; 173; 186; 3; 64; 52; 217; 226; 250;
    124; 123; 5; 202; 38; 147; 118; 126; 255; 82; 85; 212; 207; 206; 59; 227;
    47; 16; 58; 17; 182; 189; 28; 42; 223; 183; 170; 213; 119; 248; 152; 2; 44;
    154; 163; 70; 221; 153; 101; 155; 167; 43; 172; 9; 129; 22; 39; 253; 19; 98;
    108; 110; 79; 113; 224; 232; 178; 185; 112; 104; 218; 246; 97; 228; 251; 34;
    242; 193; 238; 210; 144; 12; 191; 179; 162; 241; 81; 51; 145; 235; 249; 14;
    239; 107; 49; 192; 214; 31; 181; 199; 106; 157; 184; 84; 204; 176; 115; 121;
    50; 45; 127; 4; 150; 254; 138; 236; 205; 93; 222; 114; 67; 29; 24; 72; 243;
    141; 128; 195; 78; 66; 215; 61; 156; 180;
  |]
[@@ocamlformat "break-collection-expressions=wrap"]

let hash n = permutation.(Int.of_float n land 255)

let grad1 hash x =
  let h = hash land 0x0F in
  (* gradient value 1.0, 2.0 .. 8.0 *)
  let grad = 1.0 +. Float.of_int (h land 7) in
  let grad = if h land 8 <> 0 then -.grad else grad in
  grad *. x

let grad2 hash x y =
  let h = hash land 0x3F in
  let u, v = if h < 4 then (x, y) else (y, x) in
  (if h land 1 <> 0 then -.u else u)
  +. if h land 2 <> 0 then -2.0 *. v else 2.0 *. v

let snoise1 x =
  let i0 = Float.round_down x in
  let i1 = i0 +. 1.0 in
  let x0 = x -. i0 in
  let x1 = x0 -. 1.0 in
  let t0 = 1.0 -. (x0 *. x0) in
  let t0 = t0 *. t0 in
  let n0 = t0 *. t0 *. grad1 (hash i0) x0 in
  let t1 = 1.0 -. (x1 *. x1) in
  let t1 = t1 *. t1 in
  let n1 = t1 *. t1 *. grad1 (hash i1) x1 in
  (* The maximum value of this noise is 8*(3/4)^4 = 2.53125 *)
  (* A factor of 0.395 scales to fit exactly within [-1,1] *)
  0.395 *. (n0 +. n1)

let snoise2 x y =
  let _F2 = 0.366025403 in
  (* F2 = (sqrt(3) - 1) / 2*)
  let _G2 = 0.211324865 in
  (*G2 = (3 - sqrt(3)) / 6 = F2 / (1 + 2 * K)*)
  (* skew the input space to determine which simplex cell we're in *)
  let s = (x +. y) *. _F2 in
  let xs, ys = (x +. s, y +. s) in
  let i, j = Float.(round_down xs, round_down ys) in
  (* unskew the cell origin back to (x, y) space *)
  let t = (i +. j) *. _G2 in
  let _X0 = i -. t in
  let _Y0 = j -. t in
  let x0, y0 = (x -. _X0, y -. _Y0) in
  (* determine which simplex we're in *)
  let i1, j1 = if Poly.(x0 > y0) then (1., 0.) else (0., 1.) in
  (* A step of (1,0) in (i,j) means a step of (1-c,-c) in (x,y), and *)
  (* a step of (0,1) in (i,j) means a step of (-c,1-c) in (x,y), where *)
  (* c = (3-sqrt(3))/6 *)
  let x1, y1 = Float.(x0 - i1 + _G2, y0 - j1 + _G2) in
  let x2, y2 = Float.(x0 - 1.0 + (2.0 * _G2), y0 - 1.0 + (2.0 * _G2)) in
  (* Work out the hashed gradient indices of the three simplex corners *)
  let gi0 = (j |> hash |> Float.of_int) +. i |> hash in
  let gi1 = (j +. j1 |> hash |> Float.of_int) +. i +. i1 |> hash in
  let gi2 = (j +. 1. |> hash |> Float.of_int) +. i +. 1. |> hash in
  let contrib x y gi =
    let t = 0.5 -. (x *. x) -. (y *. y) in
    if Float.(t < 0.0) then 0.0
    else
      let t = t *. t in
      t *. t *. grad2 gi x y
  in
  (* Calculate the contribution from the first corner *)
  let n0 = contrib x0 y0 gi0 in
  (* Calculate the contribution from the second corner *)
  let n1 = contrib x1 y1 gi1 in
  (* Calculate the contribution from the third corner *)
  let n2 = contrib x2 y2 gi2 in
  45.23065 *. (n0 +. n1 +. n2)

(* constants *)
let frequency = ref 1.0
let amplitude = ref 1.0
let lacunarity = ref 2.0
let persistence = ref 0.5

let fractal1 octaves x =
  let rec loop noise amp i =
    if i = 0 then noise /. amp
    else
      let frequency = !frequency *. Float.int_pow !lacunarity (i - 1) in
      let amplitude = !amplitude *. Float.int_pow !persistence (i - 1) in
      loop
        (noise +. (amplitude *. snoise1 (x *. frequency)))
        (amp +. amplitude) (i - 1)
  in
  loop 0.0 0.0 octaves

let fractal2 octaves x y =
  let rec loop noise amp i =
    if i = 0 then noise /. amp
    else
      let frequency = !frequency *. Float.int_pow !lacunarity (i - 1) in
      let amplitude = !amplitude *. Float.int_pow !persistence (i - 1) in
      loop
        (noise +. (amplitude *. snoise2 (x *. frequency) (y *. frequency)))
        (amp +. amplitude) (i - 1)
  in
  loop 0.0 0.0 octaves
