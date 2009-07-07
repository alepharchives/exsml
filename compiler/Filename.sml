(* filename.sml, various filename operations used in the compiler *)

fun check_suffix name suff = String.isSuffix suff name
fun chop_suffix name suff =
  String.extract(name, 0, SOME (size name - size suff))

val current_dir_name = "."

fun concat dirname filename =
  let val len = size dirname
      val x   = if len = 0 then "/" else String.extract(dirname, len-1, SOME 1)
  in
    case x of
        "/"   => dirname ^ filename
      | _     => dirname ^ "/" ^ filename
  end

fun is_absolute n =
  let val len = size n in
     (len >= 1 andalso String.extract(n, 0, SOME 1) = "/")    orelse
     (len >= 2 andalso String.extract(n, 0, SOME 2) = "./")   orelse
     (len >= 3 andalso String.extract(n, 0, SOME 3) = "../")
  end

fun slash_pos s =
  let val ss = Substring.full s
      fun pos i =
	  if i < 0 then NONE else
	  case Substring.sub (ss, i) of
            #"/"  => SOME i
	  | _    => pos (i - 1)
  in pos (size s - 1) end

fun basename name =
  case slash_pos name of
      SOME p =>
        String.extract(name, p+1, NONE)
    | NONE   => name

fun dirname name =
  if name = "/" then name else
  case slash_pos name of
      SOME p  => String.extract(name, 0, SOME p)
    | NONE    => "."

