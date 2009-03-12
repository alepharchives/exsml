/* exec.h : format of executable bytecode files */

/*  offset 0 --->  initial junk
                   code block
                   data block
                   symbol table
                   debug infos
                   trailer
 end of file --->
*/

/* Structure of the trailer: five 32-bit, unsigned integers, big endian */

#define TRAILER_SIZE 20

struct exec_trailer {
	size_t code_size;      /* Size of the code block (in bytes) */
	size_t data_size;      /* Size of the global data table (bytes) */
	size_t symbol_size;    /* Size of the symbol table (bytes) */
	size_t debug_size;     /* Size of the debug infos (bytes) */
	size_t magic;          /* A magic number */
};

/* Magic number for this release */

#define EXEC_MAGIC 0x4d4c3038   /* "ML08" */
