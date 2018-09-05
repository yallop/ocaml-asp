open Ocamlbuild_plugin

let () =
  dispatch begin function
  | After_rules ->
     ocaml_lib "lib/asp";
  | _ -> ()
  end
