[@@@warning "+A"]

(** Thrown when a printing error happens. Using the standard entry_point
    [print_table] it shouldn't be possible. However, it is when using other
    functions without care. Please note that this is not the only exception that
    can be raised. For instance Invalid_argument is also possible.
*)
exception PrintError of string list

let () =
  let string_of_exception (e: exn) : string option =
    match e with
    | PrintError l ->
      let buf = Buffer.create 128 in
      let fmt = Format.formatter_of_buffer buf in
      let () = Format.pp_print_string fmt "PrintError(" in
      let () =
        match l with
        | [] -> ()
        | [s] -> Format.pp_print_string fmt s
        | h::t ->
          let () = Format.pp_print_string fmt h in
          let () = List.iter (Format.fprintf fmt ", %s") t in
          ()
      in
      let () = Format.fprintf fmt ")@?" in
      Some (Buffer.contents buf)
    | _ -> None
  in
  let () = Printexc.register_printer string_of_exception in
  ()
