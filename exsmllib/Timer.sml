(* Timer -- new basis 1995-03-20, 1995-09-14, 1995-11-06, 1997-03-07
       kfl: 2003-05-26, 2008-05-05
*)

(* Under DOS, real time and cpu time are the same *)
local
    type tusage = { gcSec : int,  gcUsec : int,
                   sysSec : int, sysUsec : int,
                   usrSec : int, usrUsec : int  }
    prim_val getrutime_ : unit -> tusage = 1 "sml_getrutime"
    open Time
in
    type cpu_timer  = {usr : time, sys : time, gc : time};
    type real_timer = time;

    fun startCPUTimer () =
	let val {gcSec, gcUsec, sysSec, sysUsec, usrSec, usrUsec}
	        = getrutime_ ()
	in {usr = fromSeconds usrSec + fromMicroseconds usrUsec,
	    sys = fromSeconds sysSec + fromMicroseconds sysUsec,
	    gc  = fromSeconds gcSec  + fromMicroseconds gcUsec}
	end

    fun checkCPUTimes {usr, sys, gc} =
	let val {gcSec, gcUsec, sysSec, sysUsec, usrSec, usrUsec}
	        = getrutime_ ()
            val gcUsr = fromSeconds gcSec  + fromMicroseconds gcUsec - gc
	in {nongc = {usr = fromSeconds usrSec + fromMicroseconds usrUsec
                           - usr - gcUsr,
	             sys = fromSeconds sysSec + fromMicroseconds sysUsec - sys}
           ,gc = {usr = gcUsr,
                  sys = fromSeconds 0}
           }
	end

    fun checkCPUTimer {usr, sys, gc} =
        let val {gcSec, gcUsec, sysSec, sysUsec, usrSec, usrUsec}
	        = getrutime_ ()
	in {usr = fromSeconds usrSec + fromMicroseconds usrUsec - usr,
	    sys = fromSeconds sysSec + fromMicroseconds sysUsec - sys}
	end

    fun checkGCTime timer = #usr(#gc(checkCPUTimes timer))

    fun startRealTimer () = now ();

    fun checkRealTimer time1 = now () - time1;

(* Removed 1995-11-03, added again 1997-03-07 *)

    val totalCPUTime  = startCPUTimer ();
    val totalRealTime = startRealTimer ();

    fun totalCPUTimer _  = totalCPUTime;
    fun totalRealTimer _ = totalRealTime;

end
