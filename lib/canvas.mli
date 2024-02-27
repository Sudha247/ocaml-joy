module Html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Js = Js_of_ocaml.Js

val str : string -> Js.js_string Js.t
val bl : bool -> bool Js.t
val doc : Html.document Js_of_ocaml.Js.t
val _window : Html.window Js_of_ocaml.Js.t

module C : Modules.Impl

module Backend : Modules.Backend