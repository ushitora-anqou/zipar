(*open Zipar*)

let run _file = exit 1

let () =
  Cmdliner.(
    Cmd.(
      group (info "zipar")
        [
          v (info "run")
            Term.(
              const run $ Arg.(value & pos 0 string "" & info ~docv:"FILE" []));
        ]
      |> eval))
  |> exit
