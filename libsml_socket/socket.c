/* File mosml/src/dynlibs/msocket/msocket.c
   Ken Larsen 1998: design and implementation
   The initial implementation was financed by the PROSPER project
   Peter Sestoft 1999: beautification and robustness
   Doug Currie and Sergei Romanenko: adaptation to MacOS and MS Win32
   Last update: 1999-08-30
 */

/* General includes */
#include <errno.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <signal.h>

/* Moscow ML includes */
#include "attributes.h"
#include "mlvalues.h"
#include "alloc.h"
#include "memory.h"
#include "fail.h"
#include "str.h"
#include "signals.h"

/* ML representations of values used in this interface:

Type sock_        = Socket.sock_ = ML abstract object representing a socket
                    Corresponds to the C type int used to represent a socket
     0: socket     = the socket as a C int

Type addr         = Socket.addr = ML 3-tuple representing a socket address:
     0: size       = ML int specifying the size of the data C representation
     1: nspace     = ML int specifying the name space AF_UNIX or AF_INET
     2: data       = ML abstract object, either
                     for AF_UNIX: an ML string giving the file name
                     for AF_INET: an ML object of `type' sinaddrport

Type sinaddrport = ML 2-tuple representing INET socket address and port number
                   Corresponds to the C type struct sockaddr_in.
     0: ml_s_addr  = ML abstract object of `type' ml_s_addr
     1: port       = ML int specifying the port number

Type ml_s_addr    = ML abstract object containing an INET socket's address
                    Corresponds to the C type s_addr_t = unsigned long int
     0: s_addr     = the socket's s_addr, of C type unsigned long int
*/

/* Decomposition of addr values: */
#define Size_addrval(a)   Field(a, 0)
#define Nspace_addrval(a) Field(a, 1)
#define Data_addrval(a)   Field(a, 2)

/* Decomposition of sinaddrport values: */
#define Mlsaddr_sapval(sap) Field(sap, 0)
#define Port_sapval(sap)    Field(sap, 1)

/* Decomposition of ml_s_addr values: */
#define Saddr_mlsaddrval(mlsa) Field(mlsa, 0)

#define VAL_TO_SOCK(x) ((int) Field(x,0))

#define NILval Atom(0)
#define CONStag 1

union saddr {
	struct sockaddr sockaddr_gen;
	struct sockaddr_un sockaddr_unix;
	struct sockaddr_in sockaddr_inet;
};

typedef unsigned long s_addr_t;

value msocket_constants(value);
value newsinaddrport(s_addr_t, value);
value msocket_desccmp(value, value);
value msocket_newfileaddr(value);
int my_aton(const char *, struct in_addr *);
value msocket_newinetaddr(value, value);
value msocket_socket(value, value);
value msocket_getinetaddr(value);
value msocket_accep(value);
value msocket_accept(value);
value msocket_bind(value, value);
value msocket_connect(value, value);
value msocket_listen(value, value);
value msocket_close(value);
value msocket_shutdown(value, value);
value msocket_send(value, value, value, value, value);
value msocket_sendto(value, value, value, value, value);
value msocket_recv(value, value, value, value, value);
value msocket_recvfrom(value, value, value, value, value);
value msocket_select(value rsocks, value wsocks, value esocks,
		     int tsec, int tusec);

/* ML type: unit -> int * int * int * int * int * int * int * int * int *int */
value msocket_constants(value UNUSED(dummy))
{
	value res = alloc_tuple(10);
	Field(res, 0) = LONG_TO_VAL(SOCK_STREAM);
	Field(res, 1) = LONG_TO_VAL(SOCK_DGRAM);
	Field(res, 2) = LONG_TO_VAL(PF_UNIX);
	Field(res, 3) = LONG_TO_VAL(PF_INET);
	Field(res, 4) = LONG_TO_VAL(0); /* NO_RECVS */
	Field(res, 5) = LONG_TO_VAL(1); /* NO_SENDS */
	Field(res, 6) = LONG_TO_VAL(2); /* NO_RECVS_OR_SENDS */
	Field(res, 7) = LONG_TO_VAL(MSG_OOB);
	Field(res, 8) = LONG_TO_VAL(MSG_PEEK);
	Field(res, 9) = LONG_TO_VAL(MSG_DONTROUTE);
	return res;
}

/* Warning: allocates in the heap, may cause a GC */
/* ML return type: sock_ */
static value newsocket(int sock) {
  value result = alloc(1, Abstract_tag);
  Field(result, 0) = sock;
  return result;
}



/* Warning: allocates in the heap, may cause a GC */
static value newinaddr(s_addr_t sa) {
  int bsize = sizeof(s_addr_t);
  int wsize = (bsize + sizeof(value) - 1) / sizeof(value); /* rounding up */
  value result = alloc(wsize, Abstract_tag);
  *((s_addr_t*) result) = sa;
  return result;
}

/* Maps a : addr to s : union saddr */
static void make_saddr(union saddr *s, value a) {
  int size = VAL_TO_INT(Size_addrval(a));

  switch(VAL_TO_INT(Nspace_addrval(a))) {
  case AF_UNIX:
    s->sockaddr_unix.sun_family = AF_UNIX;
    bcopy(String_val(Data_addrval(a)), s->sockaddr_unix.sun_path, size + 1);
    break;
  case AF_INET: {
    value sinaddrport = Data_addrval(a);
    s->sockaddr_inet.sin_family = AF_INET;
    s->sockaddr_inet.sin_addr.s_addr =
      *((s_addr_t*) Mlsaddr_sapval(sinaddrport));
    /* Maybe this should be htonl? / sestoft */
    s->sockaddr_inet.sin_port = htons(VAL_TO_INT(Port_sapval(sinaddrport)));
    break;
  }
  default:
	  assert(0); /* Never reached */
  }
}

/* Warning: allocates in the heap, may cause a GC */
/* ML result type: addr */
static value newaddr(int len, int namespace, value addrdata) {
  value res;
  PUSH_ROOTS(r,1)
  r[0] = addrdata;
  res = alloc_tuple(3);
  Data_addrval(res) = r[0];
  Size_addrval(res) = INT_TO_VAL(len);
  Nspace_addrval(res) = INT_TO_VAL(namespace);
  POP_ROOTS();
  return (value) res;
}

/* Warning: allocates in the heap, may cause a GC */
/* Return type: sinaddrport = int * ml_s_addr */
value newsinaddrport(s_addr_t sa, value port) {
  value res;
  PUSH_ROOTS(r, 1);
  r[0] = alloc_tuple(2);

  Field(r[0], 0) = 0; /* to please the gc */
  Field(r[0], 1) = 0;

  modify(&Mlsaddr_sapval(r[0]), newinaddr(sa));
  modify(&Port_sapval(r[0]), port);
  res = r[0];
  POP_ROOTS();
  return res;
}

/* Warning: allocates in the heap, may cause a GC */
/* ML result type: addr */
static value from_saddr(union saddr *s, int len) {
	switch(s->sockaddr_gen.sa_family) {
	case AF_UNIX: {
		value name = copy_string(s->sockaddr_unix.sun_path);
		return newaddr(len, AF_UNIX, name);
	}
	case AF_INET: {
		value sinaddrport =
			newsinaddrport(s->sockaddr_inet.sin_addr.s_addr,
				       INT_TO_VAL(ntohs(s->sockaddr_inet.sin_port)));
		/* The native representation of a sinaddrport is struct sockaddr_in */
		return newaddr(sizeof(struct sockaddr_in), AF_INET, sinaddrport);
	}
	default:
		assert(0); /* Never reached */
	}
}

/* ML type: sock_ -> sock_ -> int */
value msocket_desccmp(value sockval1, value sockval2) {
	int sock1 = VAL_TO_SOCK(sockval1);
	int sock2 = VAL_TO_SOCK(sockval2);

	if (sock1 < sock2) {
		return LONG_TO_VAL(-1);
	} else if (sock1 > sock2) {
		return LONG_TO_VAL(1);
	} else {
		return LONG_TO_VAL(0);
	}
}

/* ML type: string -> addr */
value msocket_newfileaddr(value name) {
  struct sockaddr_un dummy;

  mlsize_t len = string_length(name);
  int addr_len = offsetof (struct sockaddr_un, sun_path) + len + 1;

  if (len >= sizeof(dummy.sun_path)) {
    failwith("ENAMETOOLONG");
  }

  return newaddr(addr_len, AF_UNIX, name);
}

/* Solaris 2.5, MacOS and MS Win32 lack inet_aton: */

int my_aton(const char* name, struct in_addr *inp) {
	return inet_aton(name, inp);
}

/* ML type: string -> int -> addr */
value msocket_newinetaddr(value name, value port) {
	struct sockaddr_in addr;
	value res = Val_unit;

	if (my_aton(String_val(name), &addr.sin_addr)) {
		value sinaddrport = newsinaddrport(addr.sin_addr.s_addr, port);
		res = newaddr(sizeof(struct sockaddr_in), AF_INET, sinaddrport);
	} else {
		failwith("Invalid address");
	}

	return res;
}

/* ML type: int -> int -> sock_ */
value msocket_socket(value namespace, value style) {
  int result = socket(VAL_TO_INT(namespace), VAL_TO_INT(style), 0);
  if (result < 0)
    failwith(strerror(errno));
  return newsocket(result);
}

/* ML type: addr -> string */
value msocket_getinetaddr(value addr) {
  /* Assumes that Nspace_addrval(addr) = AF_INET */
  value sinaddrport = Data_addrval(addr);
  value ml_s_addr = Mlsaddr_sapval(sinaddrport);
  struct in_addr in;
  in.s_addr = (s_addr_t)(Saddr_mlsaddrval(ml_s_addr));
  return copy_string(inet_ntoa(in));
}

/* ML type: sock_ -> sock_ * addr */
value msocket_accept(value sock) {
	int ret;
	union saddr addr;
	value res = Val_unit;

	size_t len = sizeof(addr);
	enter_blocking_section();
	ret = accept(VAL_TO_SOCK(sock), &addr.sockaddr_gen, &len);
	leave_blocking_section();
	if (ret == -1) {
		failwith(strerror(errno));
	} else {
		PUSH_ROOTS(roots, 2);
		roots[0] = from_saddr(&addr, len);
		roots[1] = newsocket(ret);
		res = alloc_tuple(2);
		modify(&Field(res, 0), roots[1]);
		modify(&Field(res, 1), roots[0]);
		POP_ROOTS();
	}

	return res;
}

/* ML type: sock_ -> addr -> unit */
value msocket_bind(value sock, value address)
{
	int ret, size;
	union saddr addr;
	make_saddr(&addr, address);
	size  = VAL_TO_INT(Size_addrval(address));
	ret = bind(VAL_TO_SOCK(sock), &addr.sockaddr_gen, size);
	if (ret == -1) {
		failwith(strerror(errno));
	}
	return Val_unit;
}

/* ML type: sock_ -> addr -> unit */
value msocket_connect(value sock, value address)
{
	int ret, size;
	union saddr addr;

	make_saddr(&addr, address);
	size  = VAL_TO_INT(Size_addrval(address));

	/* should enter_blocking_section() be inserted? */
	ret = connect(VAL_TO_SOCK(sock), &addr.sockaddr_gen, size);
	if (ret == -1) {
		failwith(strerror(errno));
	}
	return Val_unit;
}

/* ML type: sock_ -> int -> unit */
value msocket_listen(value sock, value queuelength) {
  int ret;
  ret =listen(VAL_TO_SOCK(sock), VAL_TO_INT(queuelength));
  if (ret == -1)
    failwith(strerror(errno));
  return Val_unit;
}

/* ML type: sock_ -> unit */
value msocket_close(value sock) {
	if (close(VAL_TO_SOCK(sock)) == -1) {
		failwith("msocket: error closing socket");
	}
	return Val_unit;
}

/* ML type: sock_ -> int -> unit */
value msocket_shutdown(value sock, value how) {
	int ret;
	ret = shutdown(VAL_TO_SOCK(sock), VAL_TO_INT(how));
	if (ret == -1) {
		failwith(strerror(errno));
	}
	return Val_unit;
}

/* ML type: sock_ -> string -> int -> int -> int -> int */
value msocket_send(value sock, value buff, value offset, value size,
                            value flags) {
	int ret;

	/* Ignore SIGPIPE signals; instead send will return -1: */
	signal(SIGPIPE, SIG_IGN);

	enter_blocking_section();
	ret = send(VAL_TO_SOCK(sock), &Byte(buff, VAL_TO_LONG(offset)), VAL_TO_INT(size),
		   VAL_TO_INT(flags));
	leave_blocking_section();
	if (ret == -1) {
		failwith(strerror(errno));
	}
	return INT_TO_VAL(ret);
}

/* ML type: sock_ -> Word8Vector.vector -> int * int -> int -> addr -> int */
value msocket_sendto(value sock, value buff, value tup, value flags,
		     value address) {
	int ret;
	union saddr addr;

	/* Ignore SIGPIPE signals; instead sendto will return -1: */
	signal(SIGPIPE, SIG_IGN);

	make_saddr(&addr, address);
	enter_blocking_section();
	ret = sendto(VAL_TO_SOCK(sock), &Byte(buff, VAL_TO_LONG(Field(tup,0))),
		     VAL_TO_INT(Field(tup, 1)), VAL_TO_INT(flags),
		     &addr.sockaddr_gen, VAL_TO_INT(Size_addrval(address)));
	leave_blocking_section();
	if (ret == -1) {
		failwith(strerror(errno));
	}
	return INT_TO_VAL(ret);
}

/* ML type: sock_ -> Word8Vector.vector -> int -> int -> int -> int */
value msocket_recv(value sock, value buff, value offset,
                            value len, value flags) {
	int ret;

	enter_blocking_section();
	ret = recv(VAL_TO_SOCK(sock), &Byte(buff, VAL_TO_LONG(offset)), VAL_TO_INT(len),
		   VAL_TO_INT(flags));
	leave_blocking_section();
	if (ret == -1) {
		failwith(strerror(errno));
	}
	return INT_TO_VAL(ret);
}

/* ML type: sock_ -> Word8Vector.vector -> int -> int -> int -> int * addr */
value msocket_recvfrom(value sock, value buff, value offset,
                                value size, value flags) {
	int ret;
	value res = Val_unit;
	union saddr addr;

	size_t len = sizeof(addr);

	enter_blocking_section();
	ret = recvfrom(VAL_TO_SOCK(sock), &Byte(buff, VAL_TO_LONG(offset)),
		       VAL_TO_INT(size),
		       VAL_TO_INT(flags), &addr.sockaddr_gen, &len);
	leave_blocking_section();

	if (ret == -1) {
		failwith(strerror(errno));
	} else {
		PUSH_ROOTS(roots, 1);
		roots[0] = from_saddr(&addr, len);
		res = alloc_tuple(2);
		modify(&Field(res, 0), INT_TO_VAL(len));
		modify(&Field(res, 1), roots[0]);
		POP_ROOTS();
	}

	return res;
}

/* This makes fd_set a set of the sockets in vector sockv */

static void vec_to_fdset(value sockv, fd_set *fds) {
	int i, vlen = Wosize_val(sockv);

	FD_ZERO(fds);
	for (i = 0; i < vlen; i++) {
		FD_SET(VAL_TO_SOCK(Field(sockv, i)), fds);
	}
}

/* This returns a list of those elements of vector sockv which are
   also in fd_set fds, in the order in which they appear in sockv.
   The list is built from the end of sockv towards its head.       */

/* Warning: allocates in the heap, may cause a GC */
/* ML return type: sock list */
static value fdset_to_list(value sockv, fd_set *fds) {
	int i, fd, vlen = Wosize_val(sockv);
	value res;

#define xs ls[0]
#define ys ls[1]
#define sockv_ ls[2]
#define sock_  ls[3]
	PUSH_ROOTS(ls, 4);
	sockv_ = sockv;
	xs = NILval;
	for (i = vlen-1; i >= 0; i--) {
		sock_ = Field(sockv_, i);
		fd = VAL_TO_SOCK(sock_);
		if (FD_ISSET(fd, fds)) {
			ys = alloc(2, CONStag);
			modify(&Field(ys, 0), sock_);
			modify(&Field(ys, 1), xs);
			xs = ys;
		}
	}
	res = xs;
	POP_ROOTS();
#undef xs
#undef ys

	return res;
}


/* Warning: allocates in the heap, may cause a GC */
/* ML return type: sock list * sock list * sock list */
value msocket_select(value rsocks, value wsocks, value esocks,
                     int tsec, int tusec) {
  int ret;
  fd_set rfd, wfd, efd;
  struct timeval timeout, *top;
  value res;

  vec_to_fdset(rsocks, &rfd);
  vec_to_fdset(wsocks, &wfd);
  vec_to_fdset(esocks, &efd);

  if (VAL_TO_INT(tsec) < 0) {
    top = NULL;
  }
  else {
    timeout.tv_sec = VAL_TO_INT(tsec);
    timeout.tv_usec = VAL_TO_INT(tusec);
    top = &timeout;
  }
  ret = select(FD_SETSIZE, &rfd, &wfd, &efd, top);

  if (ret == -1)
    failwith(strerror(errno));

  {
    PUSH_ROOTS(ls, 6);
    ls[3] = rsocks;
    ls[4] = rsocks;
    ls[5] = rsocks;
    ls[0] = fdset_to_list(ls[3], &rfd);
    ls[1] = fdset_to_list(ls[4], &wfd);
    ls[2] = fdset_to_list(ls[5], &efd);
    res = alloc_tuple(3);
    modify(&Field(res, 0), ls[0]);
    modify(&Field(res, 1), ls[1]);
    modify(&Field(res, 2), ls[2]);
    POP_ROOTS();
  }
  return res;
}
