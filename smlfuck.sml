datatype ast = Right
             | Left
             | Inc
             | Dec
             | Output
             | Input
             | Loop of ast list

val tokens = [#"<", #">", #"+", #"-", #".", #",", #"[", #"]"];

val parse  =
    let infix >>=;
        fun x >>= (xs, rest) = (x :: xs, rest);
        fun ast [] = ([], [])
          | ast (#">" :: xs) = Right >>= ast xs
          | ast (#"<" :: xs) = Left >>= ast xs
          | ast (#"+" :: xs) = Inc >>= ast xs
          | ast (#"-" :: xs) = Dec >>=  ast xs
          | ast (#"." :: xs) = Output >>= ast xs
          | ast (#"," :: xs) = Input >>= ast xs
          | ast (#"[" :: xs) =
            let val (loop, rest) = ast xs
            in Loop loop >>= ast rest end
          | ast (#"]" :: xs) = ([], xs)
          | ast (_ :: xs) = ast xs
    in #1 o ast o explode end

fun interpret commands =
  let val arr = Array.array (30000, 0);
      fun interpret' [] dp = dp
        | interpret' (Right :: xs) dp = interpret' xs (dp + 1)
        | interpret' (Left :: xs) dp = interpret' xs (dp - 1)
        | interpret' (Inc :: xs) dp =
          (Array.update (arr, dp, (Array.sub (arr, dp) + 1));
           interpret' xs dp)
        | interpret' (Dec :: xs) dp =
          (Array.update (arr, dp, (Array.sub (arr, dp) - 1));
           interpret' xs dp)
        | interpret' (Output :: xs) dp =
          (print (str (chr (Array.sub (arr, dp))));
           interpret' xs dp)
        | interpret' (Loop xs' :: xs) dp =
          if Array.sub (arr, dp) = 0 then
              interpret' xs dp
          else
              let val dp = interpret' xs' dp;
              in if Array.sub (arr, dp) <> 0 then
                     interpret' (Loop xs' :: xs) dp
                 else
                     interpret' xs dp
              end
        | interpret' (Input :: xs) dp =
          (case TextIO.input1 TextIO.stdIn of
              SOME c => Array.update (arr, dp, ord c)
            | NONE => ();
           interpret' xs dp)
  in
      interpret' commands 0
  end

fun main () =
  case CommandLine.arguments () of
      [fileName] => let val is = TextIO.openIn fileName
                        val src = TextIO.inputAll is
                    in (interpret (parse src); ()) end
    | _ => print ("usage: " ^ CommandLine.name () ^ " FILE\n")

val _ = main ();
