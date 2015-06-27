datatype command = Right
                 | Left
                 | Inc
                 | Dec
                 | Output
                 | Input
                 | JumpForward
                 | JumpBackward

val tokens = [#"<", #">", #"+", #"-", #".", #",", #"[", #"]"];

val parseTokens  =
    let val filter =
            List.filter (fn c => List.exists (fn c' => c' = c) tokens)
            o explode;
        fun charToCommand #">" = Right
          | charToCommand #"<" = Left
          | charToCommand #"+" = Inc
          | charToCommand #"-" = Dec
          | charToCommand #"." = Output
          | charToCommand #"," = Input
          | charToCommand #"[" = JumpForward
          | charToCommand #"]" = JumpBackward
          | charToCommand _ = raise Fail "impossible"
    in map charToCommand o filter end

fun main () = print "Hello world\n";

val _ = main ();
