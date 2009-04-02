(* Mosmlcookie.sml v 2.0 2000-01-12

 (c) Hans Molin, Computing Science Dept., Uppsala University, 1999.
 http://www.csd.uu.se/~d97ham/                     d97ham@csd.uu.se

 Documentation, cleanup and efficiency improvements by sestoft@dina.kvl.dk

 Anyone is granted the right to copy and/or use this code, provided
 that this note is retained, also in modified versions.  The code is
 provided as is with no guarantee about any functionality.  I take no
 responsibility for its proper function.
*)

exception CookieError of string

type cookiedata =
    { name : string, value : string, expiry : Date.date option,
      domain : string option, path : string option, secure : bool }

val http_cookie = Process.getEnv("HTTP_COOKIE");

local
    fun is c1 c2 = c1 = c2

    (* Create list of all name=value pairs *)

    val allCookies : string list =
	let open Substring
	    fun removeBWS sus = concat (fields (is #" ") sus)
	in
	    List.map removeBWS (tokens (is #";")
			       (full (Option.getOpt(http_cookie, ""))))
	end

    (* Return (name, value) for the desired cookie name *)

    fun lookupcookie cookie : (string * substring) option =
	let open Substring
	    fun matchCookie (x::xr)=
	        let val (pref, suff) = position "=" (full x)
		in
		    if string pref = cookie then SOME (x, suff)
		    else matchCookie xr
		end
	      | matchCookie [] = NONE
	in matchCookie allCookies end
in
    fun getCookieValue cookie : string option =
	let fun (f o g) x = f (g x)
	    open Substring Option
	in map (string o triml 1 o #2) (lookupcookie cookie) end

    fun getCookie cookie : string option =
	let open Substring Option
	in map #1 (lookupcookie cookie) end

    val allCookies = allCookies
end

(* Note that Date is a record.  The ctime(3) format Wdy Mon DD HH:MM:SS YYYY
   (as generated by Date.toString) is not acceptable because it is not a
   valid RFC-822 date.  No whitespaces are allowed in any string.
*)

fun concatOpt s NONE     = ""
  | concatOpt s (SOME t) = s ^ t

fun setCookie { name : string, value : string, expiry : Date.date option,
	        domain : string option, path : string option, secure : bool } =
    let fun datefmt date = Date.fmt "%A, %d-%b-%Y %H:%M:%S %Z" date
    in
	if name = "" orelse value= ""
	then raise CookieError "Name or value empty in call to setCookie"
	else String.concat
    	       ["Set-cookie: ", name, "=", value,
		concatOpt "; expires=" (Option.map datefmt expiry),
		concatOpt "; domain=" domain,
		concatOpt "; path=" path,
		"; secure", Bool.toString secure]
    end

(* To set multiple cookies *)

fun setCookies cookies = String.concat (List.map setCookie cookies)

(* To set one cookie *)

fun setCookie cookie = setCookies [cookie]

(* A CGI script can delete a cookie by returning (setting) a cookie
   with the same name and an expiry time which is in the past.  The
   path and name must match exactly in order for the expiring cookie
   to replace the valid cookie.  This requirement makes it difficult
   for anyone but the originator of a cookie to delete a cookie.  *)

fun deleteCookie { name : string, path : string option } : string =
    String.concat["Set-cookie: ", name, "=deleted;",
		  "expires=Friday, 11-Feb-77 12:00:00 GMT",
		  concatOpt "; path=" path]
