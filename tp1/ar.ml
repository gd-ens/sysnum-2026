let read_archive (ic: In_channel.t) =
  let ic = Scanf.Scanning.from_channel ic in
  Scanf.kscanf ic (fun _ _ -> failwith "No magic number") "!<arch>\n" ();
  let rec aux () =
    Scanf.kscanf ic (fun _ e -> raise e) "%16s%12s%6s%6s%8s%10s `\n" (fun name _ _ _ _ length ->
        let length = int_of_string (String.trim length) in
        let content = Bytes.init length (fun _ -> Scanf.bscanf ic "%c" Fun.id) in
        if length mod 2 = 1 then
          Scanf.kscanf ic (fun _ _ -> failwith "Incorrect terminator") "\n" ();
        (name, content)::aux ()
      ) in
  aux ()