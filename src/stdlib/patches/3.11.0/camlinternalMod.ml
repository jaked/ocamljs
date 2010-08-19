(*
 * This file is part of ocamljs, OCaml to Javascript compiler
 * Copyright (C) 2007-9 Skydeck, Inc
 * Copyright (C) 2010 Jake Donham
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

(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*         Xavier Leroy, projet Cristal, INRIA Rocquencourt            *)
(*                                                                     *)
(*  Copyright 2004 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: camlinternalMod.ml,v 1.6 2008/01/11 16:13:16 doligez Exp $ *)

type shape =
  | Function
  | Lazy
  | Class
  | Module of shape array

let rec init_mod loc shape =
  match shape with
  | Function ->
      Obj.repr (fun _ -> raise (Undefined_recursive_module loc))
  | Lazy ->
      Obj.repr (lazy (raise (Undefined_recursive_module loc)))
  | Class ->
      Obj.repr (CamlinternalOO.dummy_class loc)
  | Module comps ->
      Obj.repr (Array.map (init_mod loc) comps)

let rec update_mod shape o n =
  match shape with
    | Module comps ->
        assert (Obj.tag n = 0 && Obj.size n >= Array.length comps);
        for i = 0 to Array.length comps - 1 do
          match comps.(i) with
            | Module _ -> update_mod comps.(i) (Obj.field o i) (Obj.field n i)
            | _ -> Obj.set_field o i (Obj.field n i)
        done
    | _ -> assert false
