(* FileSys -- 1995-06-16, 1998-05-07 *)

local

    (* The type of directory structures, as handled by the OS: *)
    prim_type dirstruct_;

    (* Primitives from runtime/sys.c -- raise Io on error *)
    prim_val chdir_  : string -> unit            = 1 "sys_chdir";
    prim_val remove_ : string -> unit            = 1 "sys_remove";
    prim_val rename_ : string -> string -> unit  = 2 "sys_rename";

    (* Primitives from runtime/mosml.c -- raise Fail on error *)
    prim_val access_    : string -> int -> bool  = 2 "sml_access";
    prim_val getdir_    : unit -> string         = 1 "sml_getdir";
    prim_val isdir_     : string -> bool         = 1 "sml_isdir";
    prim_val mkdir_     : string -> unit         = 1 "sml_mkdir";
    prim_val tmpnam_    : unit -> string         = 1 "sml_tmpnam";
    prim_val modtime_   : string -> real         = 1 "sml_modtime";
    prim_val rmdir_     : string -> unit         = 1 "sml_rmdir";
    prim_val settime_   : string -> real -> unit = 2 "sml_settime";
    prim_val filesize_  : string -> int          = 1 "sml_filesize";

    prim_val opendir_   : string -> dirstruct_   = 1 "sml_opendir";
    prim_val readdir_   : dirstruct_ -> string   = 1 "sml_readdir";
    prim_val rewinddir_ : dirstruct_ -> unit     = 1 "sml_rewinddir";
    prim_val closedir_  : dirstruct_ -> unit     = 1 "sml_closedir";

    fun formatErr mlOp (SOME operand) reason =
	mlOp ^ " failed on `" ^ operand ^ "': " ^ reason
      | formatErr mlOp NONE reason =
	mlOp ^ " failed: " ^ reason

    (* Raise SysErr from ML function *)
    fun raiseSysML mlOp operand reason =
	raise SysErr (formatErr mlOp operand reason, NONE)

    (* Raise SysErr with OS specific explanation if errno <> 0 *)
    fun raiseSys mlOp operand reason =
	let prim_val errno_    : unit -> int        = 1 "sml_errno";
	    prim_val errormsg_ : int -> string      = 1 "sml_errormsg";
	    prim_val mkerrno_  : int -> syserror    = 1 "identity";
            val errno = errno_ ()
	in
	    if errno = 0 then raiseSysML mlOp operand reason
	    else raise SysErr
		(formatErr mlOp operand (errormsg_ errno),
		 SOME (mkerrno_ errno))
	end
in

    type dirstream  = dirstruct_ option ref;
    datatype access_mode = A_READ | A_WRITE | A_EXEC;

    fun access (path, perm) =
	let fun mem p = if List.exists (fn q => p=q) perm then 1 else 0
	    val permcode = mem A_READ + 2 * mem A_WRITE + 4 * mem A_EXEC
	in
	    (access_ path permcode)
	    handle Fail s => raiseSys "access" (SOME path) s
	end;

    fun getDir () =
	(getdir_ ())
	handle Fail s => raiseSys "getDir" NONE s;

    fun isDir p =
	(isdir_ p) handle Fail s => raiseSys "isDir" (SOME p) s;

    fun mkDir p =
	(mkdir_ p) handle Fail s => raiseSys "mkDir" (SOME p) s;

    fun chDir p =
	(chdir_ p)
	handle SysErr _ => raiseSys "chDir" (SOME p) "chdir";

    fun mosmlFullPath p =
	let prim_val islink_   : string -> bool   = 1 "sml_islink"
	    prim_val readlink_ : string -> string = 1 "sml_readlink"
            val links = ref 0
	    fun incrlink () =
		if !links < 30 then links := !links + 1
		else raise Fail "Too many symbolic links encountered"
	    open Path
	    fun expand p =
		let val {vol, arcs, isAbs} = Path.fromString p
		    val root = if isAbs then vol ^ "/" else vol
		in mkCanonical (List.foldl followlink root arcs) end
	    and followlink (a, p) =
		let val file = concat(p, a)
		in
		    if islink_ file then
			(incrlink();
			 expand(mkAbsolute{path=readlink_ file, relativeTo=p}))
		    else
			file
		end
	in
	    (expand(mkAbsolute{path=p, relativeTo=getDir()}))
	    handle Fail s => raiseSys "fullPath" (SOME p) s
	end;

    fun fullPath p =
	let prim_val realpath_ : string -> string = 1 "sml_realpath"
	in
	    (realpath_ p)
	    handle Fail "realpath not supported" => mosmlFullPath p
		 | Fail s => raiseSys "fullPath" (SOME p) s
	end;

    fun isLink p =
	let prim_val islink_ : string -> bool = 1 "sml_islink"
        in (islink_ p) handle Fail s => raiseSys "isLink" (SOME p) s end;

    fun readLink p =
	let prim_val readlink_ : string -> string = 1 "sml_readlink"
	in (readlink_ p) handle Fail s => raiseSys "readLink" (SOME p) s end;

    type file_id = real;  (* Namely, 2^17 * device id  + inode number *)

    fun fileId p : file_id =
	let prim_val devinode_ : string -> real = 1 "sml_devinode"
	in (devinode_ p) handle Fail s => raiseSys "fileId" (SOME p) s end;

    fun hash (fid : file_id) =
	let prim_val hash_param : int -> int -> 'a -> word
						= 3 "hash_univ_param";
	in hash_param 50 500 fid end;

    fun compare (fid1 : file_id, fid2) =
	if fid1 < fid2 then LESS
	else if fid1 > fid2 then GREATER
	else EQUAL


    fun realPath p =
	if Path.isAbsolute p then fullPath p
	else Path.mkRelative{path=fullPath p, relativeTo=getDir()};

    fun rmDir p =
	(rmdir_ p) handle Fail s => raiseSys "rmDir" (SOME p) s;
    fun tmpName () =
	(tmpnam_ ())
	handle Fail s => raiseSys "tmpName" NONE s

    fun modTime p =
	(Time.fromReal (modtime_ p))
	handle Fail s => raiseSys "modTime" (SOME p) s;

    fun fileSize p =
	(filesize_ p)
	handle Fail s => raiseSys "fileSize" (SOME p) s;

    fun remove p =
	(remove_ p)
	handle SysErr _ => raiseSys "remove" (SOME p) "unlink";

    fun rename {old, new} =
	(rename_ old new)
	handle SysErr _ => raiseSys "rename" (SOME old) "rename";

    fun setTime (path, time) =
	let val tsec =
	    Time.toReal (case time of NONE => Time.now() | SOME t => t)
	in
	    (settime_ path tsec)
	    handle Fail s => raiseSys "setTime" (SOME path) s
	end;

    fun openDir path =
	(ref (SOME (opendir_ path)))
	handle Fail s => raiseSys "openDir" (SOME path) s;

    fun mkOpt "" = NONE
      | mkOpt s  = SOME s

    fun readDir (ref NONE) =
	raiseSysML "readDir" NONE "Directory stream is closed"
      | readDir (arg as ref (SOME dstr)) =
        let val entry = readdir_ dstr
        in
            if entry <> Path.parentArc andalso entry <> Path.currentArc then
                mkOpt entry
            else
                readDir arg
        end

    fun rewindDir (ref NONE) =
	raiseSysML "rewindDir" NONE "Directory stream is closed"
      | rewindDir (ref (SOME dstr)) = rewinddir_ dstr;

    fun closeDir (ref NONE) = ()
      | closeDir (r as ref (SOME dstr)) =
	(r := NONE; closedir_ dstr);
end;
