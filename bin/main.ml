open Zipar

let run = Cmd.run

let () =
  Cmdliner.(
    Cmd.(
      group (info "zipar")
        [
          v (info "run")
            Term.(
              const run
              $ Arg.(value & pos 0 string "" & info ~docv:"ZIPFILE" [])
              $ Arg.(value & pos_right 0 string [] & info ~docv:"FILE" []));
        ]
      |> eval))
  |> exit
