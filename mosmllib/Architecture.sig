(*
    Achitecture - Machine specific definitions
    Copyright (C) 2008,2009 - Jesper Louis Andersen

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
*)

(* The precision in bits of an integer *)
val integer_precision : int

(* The number of bits in a word *)
val word_size : int

(* The maximal word that can be converted safely to an integer *)
val word_maxpos : word

(* The maximal length of an arrays and strings *)
val max_array_len : int
val max_string_len : int

(* Maximal and minimal values of the native int *)
val int_max : int
val int_min : int
