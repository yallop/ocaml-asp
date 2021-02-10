open Ocamlbuild_plugin;;
open Ocamlbuild_pack;;

dispatch begin
  function
  | After_rules ->
     ocaml_lib "lib/asp";
     begin match Sys.ocaml_version with
     | "4.04.0" ->
        flag ["ocaml"; "compile"; "open_print_code"] & S[A"-open"; A"Print_code"]
     | "4.07.1" | "4.11.1" ->
        flag ["ocaml"; "compile"; "open_print_code"] & S[A"-open"; A"Codelib"]
     | version ->
        Printf.kprintf failwith "Unsupported OCaml version %s" version
     end
  | _ -> ()
end;;
