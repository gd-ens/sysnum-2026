let read_archive (ic: In_channel.t) =
  let mn = "!<arch>\n" in
  if In_channel.really_input_string ic (String.length mn) <> Some mn then
    failwith "No magic number";
  let rec aux () =
    try
      let name = Option.get (In_channel.really_input_string ic 16) in
      let _ = In_channel.really_input_string ic 32 in
      let length = int_of_string @@ String.trim @@ Option.get (In_channel.really_input_string ic 10) in
      let em = In_channel.really_input_string ic 2 in
      assert(em = Some "`\n");
      let content = Option.get (In_channel.really_input_string ic length) in
      if length mod 2 = 1 then ignore (In_channel.input_byte ic);
      (name, content)::aux ()
    with
    | End_of_file | Invalid_argument _ -> [] in
  aux ()