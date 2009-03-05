// Change all uses of bcopy to memmove

@ bcopy_to_memmove @
expression SRC;
expression DST;
expression LEN;
@@

- bcopy(SRC, DST, LEN)
+ memmove(SRC, DST, LEN)
