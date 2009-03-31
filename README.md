# Project Ex-SML

## What is this?

This is a fork of Moscow ML 2.01 onto which it is the plan to add a
backend for the LLVM system and then extend Standard ML with some new
ideas.

Jesper Louis Andersen - 2008,2009.

### Current status

Currently, we are pretty far from a working system. We have chosen to
implement a representation of the LLVM ASCII assembler format for
SML. This is the most ML-idiomatic way to work with LLVM I think,
though it a different path than the OCaml bindings for LLVM.

We still need a compilation from the internal Lambda IR to LLVM and
probably a closure conversion phase as well. After that we need to
build glue.

### Ideology

Use UNIX tools all the way. We want a pipeline of commands, not some
big buildsystem built into the REPL. Use Autotools. Prefer sh(1) for
scripting and Python if it is more expensive.

   * Handle the win32 implementation separately.
   * Kill old Macintosh code.
   * Kill old MSDOS code.
   * Kill the Threaded/Direct Jump compiler.

### Original readme:

This is file README for Moscow ML 2.00 for Linux/Unix (June 2000)

#### EXTENT OF THE IMPLEMENTATION

The current version 2.00 of Moscow ML

   * implements the full Standard ML language, as revised 1997,
     including Modules and some extensions
   * yet is backwards compatible with versions prior to 2.00
   * implements large parts of the new SML Basis Library
   * implements separate compilation
   * can produce compact stand-alone executables (a la Caml Light)
   * supports quotations and antiquotations, useful for metaprogramming
   * supports dynamic linking of external functions under Linux (x86
      and Alpha), FreeBSD, NetBSD, Solaris, Digital Unix, HP-UX,
      MacOS, and MS Windows'95/98/NT

New in version 2.00 of Moscow ML

   * The full SML Modules language (structures, signatures, and functors)
     is now supported, thanks to Claudio Russo.  Also, several extensions
     to the SML Modules language are provided:
      - higher-order functors: functors may be defined within structures
        and functors
      - first-class modules: structures and functors may be packed and
        then handled as Core language values, which may then be unpacked
        as structures or functors again
      - recursive modules: signatures and structures may be recursively
        defined
   * Value polymorphism has become friendlier: non-generalizable free
     type variables are left free, and become instantiated (once only)
     when the bound variable is used
   * Added facilities for creating and communicating with subprocesses
     (structure Unix and Signal from SML Basis Library).
   * Added facilities for efficient functional generation of HTML code
     (structure Msp); also supports the writing of ML Server Page scripts.
   * Added facilities setting and accessing `cookies' in CGI scripts
     (structure Mosmlcookie), thanks to Hans Molin, Uppsala, Sweden.
   * The Gdimage structure now produces PNG images (using Thomas
     Boutell's gd library).

#### SYSTEM REQUIREMENTS

Compilation under Unix is best done using GNU make, gcc, and Perl.  A
binary installation requires 5 MB disk space; a source installation
requires 25 MB disk space.

#### LIST OF FILES

Documentation files:

   * install.txt : Installation instructions for Linux
   * README : This file
   * doc/manual.pdf : User manual (PDF format)
   * doc/mosmlref.pdf : A compact guide to Moscow ML syntax and primitives
   * doc/mosmllib.pdf : Moscow ML library documentation with an index
   * doc/mosmllib/*.html : Moscow ML library documentation in HTML format

Executables:

   * bootstrap : Bytecode to bootstrap the compiler

Source files:

   * compiler : The compiler
   * runtime : The C-based caml-light runtime
   * mosmlyac : The ML yacc compiler
   * mosmllib : The standard library
   * smllib_* : Dynamically loadable libraries
   * regression : The regression framework, imported from MLton
   * scripts : Shell scripts working as compiler drivers

