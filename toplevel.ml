
let () =
    let usage_msg = "usage: ./toplevel.native [file.swl]" in
    let channel = ref stdin in
    Arg.parse [] (fun file -> channel := open_in file) usage_msg;
    let lexbuf = Lexing.from_channel !channel in
    let ast = Parser.program Scanner.token lexbuf in
    let sast = Semant.check ast in
    let m = Codegen.translate sast in
    (*print_string("Through compilation\n")*)
    Llvm_analysis.assert_valid_module m;
    print_string (Llvm.string_of_llmodule m)
    (*(match Llvm_analysis.verify_module m with
      Some(reason) -> print_string reason
    | None -> ());*)


    (*print_string (Sast.string_of_sprogram sast)*)
    
    
    
    
    
    
    
    (*print_string (Ast.string_of_program ast)*)