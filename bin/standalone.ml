open Core.Std

let () = 
  Interpreter.repl ~prompt:"> " In_channel.stdin Out_channel.stdout


