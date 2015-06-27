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

fun main () = ();

val _ = main ();
