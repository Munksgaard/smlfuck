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

fun interpret xs =
  let val arr = Array.array (30000, 0);
      fun interpret' [] pos = ()
        | interpret' (Right :: xs) pos = interpret' xs (pos + 1)
        | interpret' (Left :: xs) pos = interpret' xs (pos - 1)
        | interpret' (Inc :: xs) pos =
          (Array.update (arr, pos, (Array.sub (arr, pos) + 1));
           interpret' xs pos)
        | interpret' (Dec :: xs) pos =
          (Array.update (arr, pos, (Array.sub (arr, pos) - 1));
           interpret' xs pos)
        | interpret' (Output :: xs) pos =
          (print (Char.toString (chr (Array.sub (arr, pos))));
           interpret' xs pos)
        | interpret' _ _ = raise Fail "Bla"
  in interpret' xs 0 end;

fun main () = print "Hello world\n";

val _ = main ();
