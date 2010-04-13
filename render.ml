
let lines_from_chan chan =
    let rec loop accum =
        try
            let line = input_line chan in
            loop (line :: accum)
        with End_of_file -> accum
    in
    List.rev (loop []);;

let lines = lines_from_chan stdin in
let instrs = Parser.parse_lines lines in
let _ = Parser.validate_instructions instrs in
let scene = Scene.create_scene instrs in
begin
  Graphics.open_graph " 512x512";
  for i = 0 to 512 do
    for j = 0 to 512 do
      let r, g, b = Scene.cast_ray_into_scene scene i j in
      begin
        Graphics.set_color (Graphics.rgb r g b);
        Graphics.plot i j;
      end
    done
  done;
  let rec loop () =
    let status = Graphics.wait_next_event [Graphics.Key_pressed] in
    if status.Graphics.key = '\027' then () else loop ()
  in loop ();
end


(*try
    while true do
        let input = input_line stdin in
        let instr = Parser.line_to_instr input in
        let str = Parser.instr_to_string instr in
        print_endline str
    done
with End_of_file -> ();; *)
