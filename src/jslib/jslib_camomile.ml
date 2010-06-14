(*
 * This file is part of ocamljs, OCaml to Javascript compiler
 * Copyright (C) 2007-9 Skydeck, Inc
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA
 *)

(* UChar and UTF8 taken from camomile 0.7.2 *)

module UChar =
struct
  (* $Id: uChar.ml,v 1.4 2004/09/04 16:07:38 yori Exp $ *)
  (* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)
  
  type t = int

  external uint_code : t -> int = "%identity"

  let chr_of_uint n = 
    if n lsr 31 = 0 then n else 
    invalid_arg "UChar.char_of_uint"
    
  type uchar = t

  let of_int n = chr_of_uint n
end

module UTF8 =
struct
  (* $Id: uTF8.ml,v 1.11 2004/09/04 16:07:38 yori Exp $ *)
  (* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)

  type t = string

  let look s i =
    let n' =
      let n = Char.code s.[i] in
      if n < 0x80 then n else
      if n <= 0xdf then
        (n - 0xc0) lsl 6 lor (0x7f land (Char.code s.[i + 1]))
      else if n <= 0xef then
        let n' = n - 0xe0 in
        let m0 = Char.code s.[i + 2] in
        let m = Char.code (String.unsafe_get s (i + 1)) in
        let n' = n' lsl 6 lor (0x7f land m) in
        n' lsl 6 lor (0x7f land m0)
      else if n <= 0xf7 then
        let n' = n - 0xf0 in
        let m0 = Char.code s.[i + 3] in
        let m = Char.code (String.unsafe_get s (i + 1)) in
        let n' = n' lsl 6 lor (0x7f land m) in
        let m = Char.code (String.unsafe_get s (i + 2)) in
        let n' = n' lsl 6 lor (0x7f land m) in
        n' lsl 6 lor (0x7f land m0)     
      else if n <= 0xfb then
        let n' = n - 0xf8 in
        let m0 = Char.code s.[i + 4] in
        let m = Char.code (String.unsafe_get s (i + 1)) in
        let n' = n' lsl 6 lor (0x7f land m) in
        let m = Char.code (String.unsafe_get s (i + 2)) in
        let n' = n' lsl 6 lor (0x7f land m) in
        let m = Char.code (String.unsafe_get s (i + 3)) in
        let n' = n' lsl 6 lor (0x7f land m) in
        n' lsl 6 lor (0x7f land m0)     
      else if n <= 0xfd then
        let n' = n - 0xfc in
        let m0 = Char.code s.[i + 5] in
        let m = Char.code (String.unsafe_get s (i + 1)) in
        let n' = n' lsl 6 lor (0x7f land m) in
        let m = Char.code (String.unsafe_get s (i + 2)) in
        let n' = n' lsl 6 lor (0x7f land m) in
        let m = Char.code (String.unsafe_get s (i + 3)) in
        let n' = n' lsl 6 lor (0x7f land m) in
        let m = Char.code (String.unsafe_get s (i + 4)) in
        let n' = n' lsl 6 lor (0x7f land m) in
        n' lsl 6 lor (0x7f land m0)
      else invalid_arg "UTF8.look"
    in
    UChar.of_int n'
  
  let rec search_head s i =
    if i >= String.length s then i else
    let n = Char.code (String.unsafe_get s i) in
    if n < 0x80 || n >= 0xc2 then i else
    search_head s (i + 1)

  let next s i = 
    let n = Char.code s.[i] in
    if n < 0x80 then i + 1 else
    if n < 0xc0 then search_head s (i + 1) else
    if n <= 0xdf then i + 2
    else if n <= 0xef then i + 3
    else if n <= 0xf7 then i + 4
    else if n <= 0xfb then i + 5
    else if n <= 0xfd then i + 6
    else invalid_arg "UTF8.next"
  
  let rec iter_aux proc s i =
    if i >= String.length s then () else
    let u = look s i in
    proc u;
    iter_aux proc s (next s i)
  
  let iter proc s = iter_aux proc s 0
end
