open Ocamlbuild_plugin;;
open Ocamlbuild_pack;;

dispatch begin
  function
  | After_rules ->
     ocaml_lib "lib/asp";
     flag ["ocaml"; "compile"; "open_print_code"] &
     if Sys.ocaml_version = "4.04.0" then S[A"-open"; A"Print_code"]
     else if Sys.ocaml_version = "4.07.1" then S[A"-open"; A"Codelib"]
     else Printf.kprintf failwith "Unsupported OCaml version %s" Sys.ocaml_version
  | _ -> ()
end;;

