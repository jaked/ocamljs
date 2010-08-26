// compiled by ocamlc 3.12.0, ocamljs 0.3
var ocamljs$caml_named_value = (function (){
var Match_failure$16g = "Match_failure";
var Out_of_memory$17g = "Out_of_memory";
var Stack_overflow$24g = "Stack_overflow";
var Invalid_argument$18g = "Invalid_argument";
var Failure$19g = "Failure";
var Not_found$20g = "Not_found";
var Sys_error$21g = "Sys_error";
var End_of_file$22g = "End_of_file";
var Division_by_zero$23g = "Division_by_zero";
var Sys_blocked_io$25g = "Sys_blocked_io";
var Assert_failure$26g = "Assert_failure";
var Undefined_recursive_module$27g = "Undefined_recursive_module";
/*
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
 */

var caml_blit_string = function (s1, o1, s2, o2, n) {
  for (var i = 0; i < n; i++)
    oc$$ssetu(s2, o2 + i, oc$$srefu(s1, o1 + i));
}
var caml_callback = function (f, a) { return _(f, [a]); }
var caml_callback2 = function (f, a1, a2) { return _(f, [a1, a2]); }
var caml_callback3 = function (f, a1, a2, a3) { return _(f, [a1, a2, a3]); }
var caml_callback4 = function (f, a1, a2, a3, a4) { return _(f, [a1, a2, a3, a4]); }
var caml_callback5 = function (f, a1, a2, a3, a4, a5) { return _(f, [a1, a2, a3, a4, a5]); }
var caml_callbackN = function (f, n, args) { return _(f, args); }
// XXX caml_callback_exn ?
var compare_val = function (v1, v2, total) {
  var LESS = -1;
  var GREATER = 1;
  var EQUAL = 0;
  var UNORDERED = -2; // XXX ok?

  // XXX needs some work

  if (v1 == v2 && total) return EQUAL;

  var t1 = typeof v1;
  var t2 = typeof v2;
  if (t1 == t2) {
    switch (t1) {
    case "boolean":
      if (v1 < v2) return LESS;
      if (v1 > v2) return GREATER;
      return EQUAL;
    case "number":
      if (v1 < v2) return LESS;
      if (v1 > v2) return GREATER;
      if (v1 != v2) {
	if (!total) return UNORDERED;
	if (v1 == v1) return GREATER;
	if (v2 == v2) return LESS;
	return EQUAL;
      }
      return EQUAL;
    case "string":
      if (v1 < v2) return LESS;
      if (v1 > v2) return GREATER;
      return EQUAL;
    case "function":
      caml_invalid_argument("equal: functional value");
    case "object":
      // like NaN
      if (v1 == null) {
	if (v2 == null) return EQUAL;
	return LESS;
      }
      if (v2 == null) return GREATER;

      // XXX is there a way to get the class of an object as a value?
      // XXX is it worth special casing various JS objects?
      if (v1 instanceof Date) {
	var t1 = v1.getTime();
	var t2 = v2.getTime();
	if (t1 < t2) return LESS;
	if (t1 > t2) return GREATER;
	return EQUAL;
      }
      if (v1 instanceof Array) {
	// we should always either have both tags or neither
	// so it is OK to fall through here
	if (v1.t < v2.t) return LESS;
	if (v1.t > v2.t) return GREATER;
	var sz1 = v1.length;
	var sz2 = v2.length;
	if (sz1 < sz2) return LESS;
	if (sz1 > sz2) return GREATER;
	if (sz1 == 0) return EQUAL;
	for (var i=0; i < sz1; i++)
	  {
	    var c = compare_val(v1[i], v2[i], total);
	    if (c != EQUAL) return c;
	  }
	return EQUAL;
      }
      if (v1 instanceof oc$$ms) {
	var s1 = v1.toString();
	var s2 = v2.toString();
	if (s1 < s2) return LESS;
	if (s1 > s2) return GREATER;
	return EQUAL;
      }
      if (v1._m != null && v2._m != null) { // i.e. an OCaml object XXX better test
        var oid1 = v1[1];
        var oid2 = v2[1];
        if (oid1 < oid2) return LESS;
        if (oid1 > oid2) return GREATER;
        return EQUAL;
      }
      return UNORDERED; // XXX
    default:
      return UNORDERED;
    }
  }

  // like NaN
  if (v1 == null) {
    if (v2 == null) return EQUAL;
    return LESS;
  }
  if (v2 == null) return GREATER;

  // one boolean and one int
  if (t1 == "boolean" || t2 == "boolean")
  {
    if (v1 < v2) return LESS;
    if (v1 > v2) return GREATER;
    return EQUAL;
  }
  // one mutable and one immutable string
  if (t1 == "string" || t2 == "string")
  {
    var s1 = v1.toString();
    var s2 = v2.toString();
    if (s1 < s2) return LESS;
    if (s1 > s2) return GREATER;
    return EQUAL;
  }
  // one constructor without data (number) and one with (object Array)
  if (t1 == "number") return LESS;
  if (t2 == "number") return GREATER;
  return UNORDERED;
}
var caml_compare = function (v1, v2) {
  var res = compare_val(v1, v2, 1);
  return res < 0 ? -1 : res > 0 ? 1 : 0;
}
var caml_equal = function (v1, v2) { return compare_val(v1, v2, 0) == 0; }
var caml_failwith = function (s) { throw $(Failure$19g, s); }
var caml_fill_string = function(s, o, l, c) {
  for (var i = 0; i < l; i++)
    oc$$ssetu(s, o + i, c);
}
var caml_float_compare = function (v1, v2) {
  if (v1 === v2) return 0;
  if (v1 < v2) return -1;
  if (v1 > v2) return 1;
  if (v1 === v1) return 1;
  if (v2 === v2) return -1;
  return 0;
}
var caml_float_of_string = function (s) {
  var f = parseFloat(s);
  return isNaN(f) ? caml_failwith("float_of_string") : f;
}
var caml_classify_float = function (f) {
  if (isNaN(f)) return 4; // FP_nan
  else if (!isFinite(f)) return 3; // FP_infinite
  else if (f === 0) return 2; // FP_zero
  // can't determine subnormal from js afaik
  else return 0; // FP_normal
}

var caml_greaterthan = function (v1, v2) { return compare_val(v1, v2, 0) > 0; }
var caml_greaterequal = function (v1, v2) { return compare_val(v1, v2, 0) >= 0; }
var caml_hash_univ_param = function (count, limit, obj) {
  // globals
  hash_univ_limit = limit;
  hash_univ_count = count;
  hash_accu = 0;

  // XXX needs work
  function hash_aux(obj) {
    hash_univ_limit--;
    if (hash_univ_count < 0 || hash_univ_limit < 0) return;

    function combine(n) { hash_accu = hash_accu * 65599 + n; }
    function combine_small(n) { hash_accu = hash_accu * 19 + n; }

    switch (typeof obj) {
    case "number":
      // XXX for floats C impl examines bit rep
      // XXX for constructors without data C impl uses combine_small
      hash_univ_count--;
      combine(obj);
      break;
    case "string":
      hash_univ_count--;
      for (var i = obj.length; i > 0; i--)
        combine_small(obj.charCodeAt(i));
      break;
    case "boolean":
      hash_univ_count--;
      combine_small(obj ? 1 : 0);
      break;
    case "object":
      if (obj instanceof oc$$ms)
        hash_aux(obj.toString());
      else if (obj instanceof Array) { // possibly a block
        if (obj.t) {
          hash_univ_count--;
          combine_small(obj.t);
          for (var i = obj.length; i > 0; i--)
            hash_aux(obj[i]);
        }
      }
      else if (obj._m != null) { // OCaml object, use oid
        hash_univ_count--;
        combine(obj[1]);
      }
      break;
    default:
      break;
    }
  }

  hash_aux(obj);
  return hash_accu & 0x3FFFFFFF;
}
var caml_input_value = function () { throw "caml_input_value"; }
var caml_input_value_from_string = function () { throw "caml_input_value_from_string"; }
var caml_install_signal_handler = function () { throw "caml_install_signal_handler"; }
var caml_int_compare = function (i1, i2) { return (i1 > i2) - (i1 < i2); }
var caml_int32_compare = function (i1, i2) { return (i1 > i2) - (i1 < i2); }
var caml_int64_compare = function (i1, i2) { throw "caml_int64_compare"; }
var caml_int64_float_of_bits = function (s) {
  // see pervasives.ml; int64s are represented by strings
  switch (s) {
  case "9218868437227405312": return Number.POSITIVE_INFINITY;
  case "-4503599627370496": return Number.NEGATIVE_INFINITY;
  case "9218868437227405313": return Number.NaN;
  case "9218868437227405311" : return Number.MAX_VALUE;
  case "4503599627370496": return Number.MIN_VALUE;
  case "4372995238176751616": return 0; // XXX how to get epsilon in js?
  default: return 0;
  }
}
var caml_int_of_string = function (s) {
  var i = parseInt(s, 10);
  return isNaN(i) ? caml_failwith("int_of_string") : i;
}
var caml_int32_of_string = caml_int_of_string;
var caml_int64_of_string = caml_int_of_string;
var caml_nativeint_of_string = caml_int_of_string;
var caml_invalid_argument = function (s) { throw $(Invalid_argument$18g, s); }
var caml_is_printable = function (c) { return c > 31 && c < 127; } // XXX get this right
var caml_lessthan = function (v1, v2) { return compare_val(v1, v2, 0) -1 < -1; }
var caml_lessequal = function (v1, v2) { return compare_val(v1, v2, 0) -1 <= -1; }
var caml_make_vect = function (l, i) {
  var a = new Array(l);
  for (var j = 0; j < l; j++)
    a[j] = i;
  return a;
}
var caml_marshal_data_size = function () { throw "caml_marshal_data_size"; }
var caml_md5_chan = function () { throw "caml_md5_chan"; }
var caml_md5_string = function () { throw "caml_md5_string"; }
var caml_ml_channel_size = function () { throw "caml_ml_channel_size"; }
var caml_ml_channel_size_64 = function () { throw "caml_ml_channel_size_64"; }
var caml_ml_close_channel = function () { throw "caml_ml_close_channel"; }

var caml_ml_flush = function (c) { }

var caml_ml_input = function () { throw "caml_ml_input"; }
var caml_ml_input_char = function () { throw "caml_ml_input_char"; }
var caml_ml_input_int = function () { throw "caml_ml_input_int"; }
var caml_ml_input_scan_line = function () { throw "caml_ml_input_scan_line"; }
var caml_ml_open_descriptor_in = function () { return 0; } // XXX
var caml_ml_open_descriptor_out = function () { return 0; } // XXX
var caml_ml_out_channels_list = function () { return 0; }

var caml_ml_output = function (c, b, s, l) { print_verbatim(b); }
var caml_ml_output_char = function (c, ch) {  }

var caml_ml_output_int = function () { throw "caml_ml_output_int"; }
var caml_ml_pos_in = function () { throw "caml_ml_pos_in"; }
var caml_ml_pos_in_64 = function () { throw "caml_ml_pos_in_64"; }
var caml_ml_pos_out = function () { throw "caml_ml_pos_out"; }
var caml_ml_pos_out_64 = function () { throw "caml_ml_pos_out_64"; }
var caml_ml_seek_in = function () { throw "caml_ml_seek_in"; }
var caml_ml_seek_in_64 = function () { throw "caml_ml_seek_in_64"; }
var caml_ml_seek_out = function () { throw "caml_ml_seek_out"; }
var caml_ml_seek_out_64 = function () { throw "caml_ml_seek_out_64"; }
var caml_ml_set_binary_mode = function () { throw "caml_ml_set_binary_mode"; }
var caml_named_value = function (n) { return oc$$nv[n]; }
var caml_nativeint_compare = function (i1, i2) { return (i1 > i2) - (i1 < i2); }
var caml_notequal = function (v1, v2) { return compare_val(v1, v2, 0) != 0; }
var caml_obj_dup = function (a) {
  var l = a.length;
  var d = new Array(l);
  for (var i=0; i < l; i++)
    d[i] = a[i];
  d.t = a.t;
  return d;
}
var caml_obj_is_block = function (o) { return !(typeof o == 'number') }
var caml_obj_tag = function(o) { return o.t || 0; }
var caml_obj_set_tag = function(o, t) { o.t = t; }
var caml_obj_block = function(t, s) { if (s == 0) return t; else { var a = new Array(s); a.t = t; return a; } }
var caml_obj_truncate = function(o, s) { o.length = s; }
var caml_output_value = function () { throw "caml_output_value"; }
var caml_output_value_to_string = function () { throw "caml_output_value_to_string"; }
var caml_output_value_to_buffer = function () { throw "caml_output_value_to_buffer"; }
var caml_record_backtrace = function () { throw "caml_record_backtrace"; }
var caml_backtrace_status = function () { throw "caml_backtrace_status"; }
var caml_get_exception_backtrace = function () { throw "caml_get_exception_backtrace"; }
var caml_register_named_value = function (n, v) { oc$$nv[n] = v; }
var caml_string_compare = function (s1, s2) {
  if (oc$$slt(s1, s2)) return -1;
  else if (oc$$sgt(s1, s2)) return 1;
  else return 0;
}
var caml_sys_exit = function () { throw "caml_sys_exit"; }
  var init_time = (new Date()).getTime() / 1000;
var caml_sys_time = function () { return (new Date()).getTime() / 1000 - init_time; }
var caml_sys_get_argv = function () { return $("", $()); } // XXX put something here?
var caml_sys_get_config = function () { return $("js", 32); } // XXX browser name?
var caml_sys_open = function () { throw "caml_sys_open"; }
var caml_sys_random_seed = function() { throw "caml_sys_random_seed"; }

// lexing.c

function Short(tbl, n) {
  var s = tbl.charCodeAt(n * 2) + (tbl.charCodeAt(n * 2 + 1) << 8);
  return s & 32768 ? s + -65536 : s;
}

var caml_lex_engine = function (tbl, start_state, lexbuf)
{
  var state, base, backtrk, c;

  state = start_state;
  if (state >= 0) {
    /* First entry */
    lexbuf[6] = lexbuf[4] = lexbuf[5];
    lexbuf[7] = -1;
  } else {
    /* Reentry after refill */
    state = -state - 1;
  }
  while(1) {
    /* Lookup base address or action number for current state */
    base = Short(tbl[0], state);
    if (base < 0) return -base-1;
    /* See if it's a backtrack point */
    backtrk = Short(tbl[1], state);
    if (backtrk >= 0) {
      lexbuf[6] = lexbuf[5];
      lexbuf[7] = backtrk;
    }
    /* See if we need a refill */
    if (lexbuf[5] >= lexbuf[2]){
      if (lexbuf[8] === false){
        return -state - 1;
      }else{
        c = 256;
      }
    }else{
      /* Read next input char */
      c = lexbuf[1].charCodeAt(lexbuf[5]);
      lexbuf[5] += 1;
    }
    /* Determine next state */
    if (Short(tbl[4], base + c) == state)
      state = Short(tbl[3], base + c);
    else
      state = Short(tbl[2], state);
    /* If no transition on this char, return to last backtrack point */
    if (state < 0) {
      lexbuf[5] = lexbuf[6];
      if (lexbuf[7] == -1) {
        caml_failwith("lexing: empty token");
      } else {
        return lexbuf[7];
      }
    }else{
      /* Erase the EOF condition only if the EOF pseudo-character was
         consumed by the automaton (i.e. there was no backtrack above)
       */
      if (c == 256) lexbuf[8] = false;
    }
  }
}

/***********************************************/
/* New lexer engine, with memory of positions  */
/***********************************************/

function run_mem(p, pc, mem, curr_pos) {
  for (;;) {
    var dst, src ;

    dst = p.charCodeAt(pc++) ;
    if (dst == 0xff)
      return ;
    src = p.charCodeAt(pc++) ;
    if (src == 0xff) {
      /*      fprintf(stderr,"[%hhu] <- %d\n",dst,Int_val(curr_pos)) ;*/
      mem[dst] = curr_pos ;
    } else {
      /*      fprintf(stderr,"[%hhu] <- [%hhu]\n",dst,src) ; */
      mem[dst] = mem[src] ;
    }
  }
}

function run_tag(p, pc, mem) {
  for (;;) {
    var dst, src ;

    dst = p.charCodeAt(pc++) ;
    if (dst == 0xff)
      return ;
    src = p.charCodeAt(pc++) ;
    if (src == 0xff) {
      /*      fprintf(stderr,"[%hhu] <- -1\n",dst) ; */
      mem[dst] = -1 ;
    } else {
      /*      fprintf(stderr,"[%hhu] <- [%hhu]\n",dst,src) ; */
      mem[dst] = mem[src] ;
    }
  }
}

var caml_new_lex_engine = function (tbl, start_state, lexbuf)
{
  var state, base, backtrk, c, pstate ;
  state = start_state;
  if (state >= 0) {
    /* First entry */
    lexbuf[6] = lexbuf[4] = lexbuf[5];
    lexbuf[7] = -1;
  } else {
    /* Reentry after refill */
    state = -state - 1;
  }
  while(1) {
    /* Lookup base address or action number for current state */
    base = Short(tbl[0], state);
    if (base < 0) {
      var pc_off = Short(tbl[5], state) ;
      run_tag(tbl[10], pc_off, lexbuf[9]);
      /*      fprintf(stderr,"Perform: %d\n",-base-1) ; */
      return -base-1;
    }
    /* See if it's a backtrack point */
    backtrk = Short(tbl[1], state);
    if (backtrk >= 0) {
      var pc_off =  Short(tbl[6], state);
      run_tag(tbl[10], pc_off, lexbuf[9]);
      lexbuf[6] = lexbuf[5];
      lexbuf[7] = backtrk;

    }
    /* See if we need a refill */
    if (lexbuf[5] >= lexbuf[2]){
      if (lexbuf[8] === false){
        return -state - 1;
      }else{
        c = 256;
      }
    }else{
      /* Read next input char */
      c = lexbuf[1].charCodeAt(lexbuf[5]);
      lexbuf[5] += 1;
    }
    /* Determine next state */
    pstate=state ;
    if (Short(tbl[4], base + c) == state)
      state = Short(tbl[3], base + c);
    else
      state = Short(tbl[2], state);
    /* If no transition on this char, return to last backtrack point */
    if (state < 0) {
      lexbuf[5] = lexbuf[6];
      if (lexbuf[7] == -1) {
        caml_failwith("lexing: empty token");
      } else {
        return lexbuf[7];
      }
    }else{
      /* If some transition, get and perform memory moves */
      var base_code = Short(tbl[5], pstate) ;
      var pc_off ;
      if (Short(tbl[9], base_code + c) == pstate)
        pc_off = Short(tbl[8], base_code + c) ;
      else
        pc_off = Short(tbl[7], pstate) ;
      if (pc_off > 0) 
        run_mem(tbl[10], pc_off, lexbuf[9], lexbuf[5]) ;
      /* Erase the EOF condition only if the EOF pseudo-character was
         consumed by the automaton (i.e. there was no backtrack above)
       */
      if (c == 256) lexbuf[8] = false;
    }
  }
}

// parsing.c

var caml_parser_trace = false

/* Auxiliary for printing token just read */

function token_name(names, number)
{
  var n = 0;
  for (/*nothing*/; number > 0; number--) {
    var i = names.indexOf("\x00", n);
    if (i == -1) return "<unknown token>";
    n = i + 1;
  }
  return names.substr(n, names.indexOf("\x00", n) - n);
}

function print_token(tables, state, tok)
{
  if (typeof tok == 'number') {
    print("State " + state + ": read token " + token_name(tables[14], tok));
  } else {
    print("State " + state + ": read token " + token_name(tables[15], tok.t) + "(" + tok[0] + ")");
  }      
}      

/* The pushdown automata */

var caml_parse_engine = function (tables, env, cmd, arg)
{
  var state;
  var sp, asp;
  var errflag;
  var n, n1, n2, m, state1;

  loop: while (true) switch (cmd) {

  case 0:
    state = 0;
    sp = env[13];
    errflag = 0;

  case -1:
    n = Short(tables[5], state);
    if (n != 0) { cmd = -7; continue loop; }
    if (env[6] >= 0) { cmd = -2; continue loop; }
    env[13] = sp; env[14] = state; env[15] = errflag;
    return 0;
                                /* The ML code calls the lexer and updates */
                                /* symb_start and symb_end */
  case 1:
    sp = env[13]; state = env[14]; errflag = env[15];
    if (!(typeof arg == 'number')) {
      env[6] = tables[2][arg.t];
      env[7] = arg[0];
    } else {
      env[6] = tables[1][arg];
      env[7] = 0;
    }
    if (caml_parser_trace) print_token(tables, state, arg);
    
  case -2:
    n1 = Short(tables[7], state);
    n2 = n1 + env[6];
    if (n1 != 0 && n2 >= 0 && n2 <= tables[10] &&
        Short(tables[12], n2) == env[6]) { cmd = -4; continue loop; }
    n1 = Short(tables[8], state);
    n2 = n1 + env[6];
    if (n1 != 0 && n2 >= 0 && n2 <= tables[10] &&
        Short(tables[12], n2) == env[6]) {
      n = Short(tables[11], n2);
      cmd = -7; continue loop;
    }
    if (errflag > 0) { cmd = -3; continue; }
    env[13] = sp; env[14] = state; env[15] = errflag;
    return 5;
                                /* The ML code calls the error function */
  case 5:
    sp = env[13]; state = env[14]; errflag = env[15];
  case -3:
    if (errflag < 3) {
      errflag = 3;
      while (1) {
        state1 = env[0][sp];
        n1 = Short(tables[7], state1);
        n2 = n1 + 256;
        if (n1 != 0 && n2 >= 0 && n2 <= tables[10] &&
            Short(tables[12], n2) == 256) {
          if (caml_parser_trace) 
            print("Recovering in state " + state1);
          cmd = -5; continue loop;
        } else {
          if (caml_parser_trace){
            print("Discarding state " + state1);
          }
          if (sp <= env[5]) {
            if (caml_parser_trace){
              print("No more states to discard");
            }
            return 1; /* The ML code raises Parse_error */
          }
          sp--;
        }
      }
    } else {
      if (env[6] == 0)
        return 1; /* The ML code raises Parse_error */
      if (caml_parser_trace) print("Discarding last token read");
      env[6] = -1;
      cmd = -1; continue loop;
    }
    
  case -4:
    env[6] = -1;
    if (errflag > 0) errflag--;
  case -5:
    if (caml_parser_trace)
      print("State " + state + ": shift to state " + Short(tables[11], n2));
    state = Short(tables[11], n2);
    sp++;
    if (sp < env[4]) { cmd = -6; continue loop; }
    env[13] = sp; env[14] = state; env[15] = errflag;
    return 2;
                                 /* The ML code resizes the stacks */
  case 2:
    sp = env[13]; state = env[14]; errflag = env[15];
  case -6:
    env[0][sp] = state;
    env[1][sp] = env[7];
    env[2][sp] = env[8];
    env[3][sp] = env[9];
    cmd = -1; continue loop;

  case -7:
    if (caml_parser_trace)
      print("State " + state + ": reduce by rule " + n);
    m = Short(tables[4], n);
    env[10] = sp;
    env[12] = n;
    env[11] = m;
    sp = sp - m + 1;
    m = Short(tables[3], n);
    state1 = env[0][sp - 1];
    n1 = Short(tables[9], m);
    n2 = n1 + state1;
    if (n1 != 0 && n2 >= 0 && n2 <= tables[10] &&
        Short(tables[12], n2) == state1) {
      state = Short(tables[11], n2);
    } else {
      state = Short(tables[6], m);
    }
    if (sp < env[4]) { cmd = -8; continue loop; }
    env[13] = sp; env[14] = state; env[15] = errflag;
    return 3;
                                /* The ML code resizes the stacks */
  case 3:
    sp = env[13]; state = env[14]; errflag = env[15];
  case -8:
    env[13] = sp; env[14] = state; env[15] = errflag;
    return 4;
                                /* The ML code calls the semantic action */
  case 4:
    sp = env[13]; state = env[14]; errflag = env[15];
    env[0][sp] = state;
    env[1][sp] = arg;
    asp = env[10];
    env[3][sp] = env[3][asp];
    if (sp > asp) {
      /* This is an epsilon production. Take symb_start equal to symb_end. */
      env[2][sp] = env[3][asp];
    }
    cmd = -1; continue loop;
  }
}

var caml_set_parser_trace = function (flag)
{
  var oldflag = caml_parser_trace;
  caml_parser_trace = flag;
  return oldflag;
}

/*
  stuff below taken from js_of_ocaml/lib
  Copyright (C) 2010 Jérôme Vouillon
*/

///////////// Format
//Provides: caml_parse_format
//Requires: caml_invalid_argument
function caml_parse_format (fmt) {
  fmt = fmt.toString ();
  var len = fmt.length;
  if (len > 31) caml_invalid_argument("format_int: format too long");
  var f =
    { justify:'+', signstyle:'-', filler:' ', alternate:false,
      base:0, signedconv:false, width:0, uppercase:false,
      sign:1, prec:6, conv:'f' };
  for (var i = 0; i < len; i++) {
    var c = fmt.charAt(i);
    switch (c) {
    case '-':
      f.justify = '-'; break;
    case '+': case ' ':
      f.signstyle = c; break;
    case '0':
      f.filler = '0'; break;
    case '#':
      f.alternate = true; break;
    case '1': case '2': case '3': case '4': case '5':
    case '6': case '7': case '8': case '9':
      f.width = 0;
      while (c=fmt.charCodeAt(i) - 48, c >= 0 && c <= 9) {
        f.width = f.width * 10 + c; i++
      }
      i--;
     break;
    case '.':
      f.prec = 0;
      i++;
      while (c=fmt.charCodeAt(i) - 48, c >= 0 && c <= 9) {
        f.prec = f.prec * 10 + c; i++
      }
      i--;
    case 'd': case 'i': case 'l': case 'n': case 'L': case 'N':
      f.signedconv = true; /* fallthrough */
    case 'u':
      f.base = 10; break;
    case 'x':
      f.base = 16; break;
    case 'X':
      f.base = 16; f.uppercase = true; break;
    case 'o':
      f.base = 8; break;
    case 'e': case 'f': case 'g':
      f.signedconv = true; f.conv = c; break;
    case 'E': case 'F': case 'G':
      f.signedconv = true; f.uppercase = true;
      f.conv = c.toLowerCase (); break;
    }
  }
  return f;
}

//Provides: caml_finish_formatting
//Requires: MlString
function caml_finish_formatting(f, rawbuffer) {
  if (f.uppercase) rawbuffer = rawbuffer.toUpperCase();
  var len = rawbuffer.length;
  /* Adjust len to reflect additional chars (sign, etc) */
  if (f.signedconv && (f.sign < 0 || f.signstyle != '-')) len++;
  if (f.alternate) {
    if (f.base == 8) len += 1;
    if (f.base == 16) len += 2;
  }
  /* Do the formatting */
  var buffer = "";
  if (f.justify == '+' && f.filler == ' ')
    for (i = len; i < f.width; i++) buffer += ' ';
  if (f.signedconv) {
    if (f.sign < 0) buffer += '-';
    else if (f.signstyle != '-') buffer += f.signstyle;
  }
  if (f.alternate && f.base == 8) buffer += '0';
  if (f.alternate && f.base == 16) buffer += "0x";
  if (f.justify == '+' && f.filler == '0')
    for (i = len; i < f.width; i++) buffer += '0';
  buffer += rawbuffer;
  if (f.justify == '-')
    for (i = len; i < f.width; i++) buffer += ' ';
  return buffer;
}

//Provides: caml_format_int const
//Requires: caml_parse_format, caml_finish_formatting
function caml_format_int(fmt, i) {
  if (fmt.toString() == "%d") return (""+i);
  var f = caml_parse_format(fmt);
  if (i < 0) { if (f.signedconv) { f.sign = -1; i = -i; } else i >>>= 0; }
  var s = i.toString(f.base);
  return caml_finish_formatting(f, s);
}

//Provides: caml_format_float const
//Requires: caml_parse_format, caml_finish_formatting
function caml_format_float (fmt, x) {
  var s, f = caml_parse_format(fmt);
  if (x < 0) { f.sign = -1; x = -x; }
  if (isNaN(x)) { s = "nan"; f.filler = ' '; }
  else if (!isFinite(x)) { s = "inf"; f.filler = ' '; }
  else
    switch (f.conv) {
    case 'e':
      var s = x.toExponential(f.prec);
      // exponent should be at least two digits
      var i = s.length;
      if (s.charAt(i - 3) == 'e')
        s = s.slice (0, i - 1) + '0' + s.slice (i - 1);
      break;
    case 'f':
      s = x.toFixed(f.prec); break;
    case 'g':
      var prec = f.prec?f.prec:1;
      s = x.toExponential(prec - 1);
      var j = s.indexOf('e');
      var exp = +s.slice(j + 1);
      if (exp < -4 || x.toFixed(0).length > prec) {
        // remove trailing zeroes
        var i = j - 1; while (s.charAt(i) == '0') i--;
        if (s.charAt(i) == '.') i--;
        s = s.slice(0, i + 1) + s.slice(j);
        i = s.length;
        if (s.charAt(i - 3) == 'e')
          s = s.slice (0, i - 1) + '0' + s.slice (i - 1);
        break;
      } else {
        var p = prec;
        if (exp < 0) { p -= exp + 1; s = x.toFixed(p); }
        else while (s = x.toFixed(p), s.length > prec + 1) p--;
        if (p) {
          // remove trailing zeroes
          i = s.length - 1; while (s.charAt(i) == '0') i--;
          if (s.charAt(i) == '.') i--;
          s = s.slice(0, i + 1);
        }
      }
      break;
    }
  return caml_finish_formatting(f, s);
}
/*
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
 */

/*
function console_log(s) {
  var cs = Components.classes["@mozilla.org/consoleservice;1"].getService(Components.interfaces["nsIConsoleService"]);
  cs.logStringMessage(s);
}
*/

var oc$$nv = {}

// XXX name these sensibly and compactify code afterwards

function ___a(m, t, a) {
  return m.apply(t, a);
}

/*@cc_on @if (@_win32 && @_jscript_version >= 5)
function ___a(m, t, a) {
  if (m.apply)
    return m.apply(t, a);
  else
    // IE < 8 doesn't support apply for DOM methods, but does support "cached" methods bound to an object
    switch (a.length) {
    case 0: return m();
    case 1: return m(a[0]);
    case 2: return m(a[0], a[1]);
    case 3: return m(a[0], a[1], a[2]);
    case 4: return m(a[0], a[1], a[2], a[3]);
    case 5: return m(a[0], a[1], a[2], a[3], a[4]);
    case 6: return m(a[0], a[1], a[2], a[3], a[4], a[5]);
    case 7: return m(a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
    default: throw "unimplemented";
    }
}
@end @*/

function ___m(m, t, a)
{
  function ap(a1, a2) {
    var a = new Array();
    for (var i=0; i < a1.length; i++) a.push(a1[i]);
    for (var i=0; i < a2.length; i++) a.push(a2[i]);
    return a;
  }

  while (true) {
    var al = a.length;
    var ml = m.length;

    if (al < ml)
    {
      switch (ml - al) {
      case 1: return _f(function (z) { return m.apply(t, ap(a, arguments)) });
      case 2: return _f(function (z,y) { return m.apply(t, ap(a, arguments)) });
      case 3: return _f(function (z,y,x) { return m.apply(t, ap(a, arguments)) });
      case 4: return _f(function (z,y,x,w) { return m.apply(t, ap(a, arguments)) });
      case 5: return _f(function (z,y,x,w,v) { return m.apply(t, ap(a, arguments)) });
      case 6: return _f(function (z,y,x,w,v,u) { return m.apply(t, ap(a, arguments)) });
      case 7: return _f(function (z,y,x,w,v,u,s) { return m.apply(t, ap(a, arguments)) });
      default: throw "unimplemented";
      }
    }
    else if (al == ml)
      return m.apply(t, a);
    else // al > ml
    {
      m = _m(m, t, a.slice(0, ml));
      t = m;
      a = a.slice(ml);
    }
  }
}

var $in_tail = false;

// tail call
function __m(m, t, args)
{
  if (m.$oc) {
    if ($in_tail) {
      args.$m = m;
      args.$t = t;
      args.$tr = true;
      return args;
    }
    else
      return _m(m, t, args);
  }
  else {
    var old_in_tail = $in_tail;
    $in_tail = false;
    try { return ___a(m, t, args); }
    finally { $in_tail = old_in_tail; }
  }
}
function __(t, args) { return __m(t, t, args); }

// non tail call
function _m(m, t, args)
{
  if (m.$oc) {
    var old_in_tail = $in_tail;
    $in_tail = true;
    try {
      var v = __m(m, t, args);
      while (v && v.$tr)
        v = ___m(v.$m, v.$t, v);
      return v;
    }
    finally { $in_tail = old_in_tail; }
  }
  else {
    var old_in_tail = $in_tail;
    $in_tail = false;
    try { return ___a(m, t, args); }
    finally { $in_tail = old_in_tail; }
  }
}
function _(t, args) { return _m(t, t, args); }

function _f(f) {
  f.$oc = true;
  return f;
}

function $N(t, a) {
  var l = a.length;
  var b = new Array(l);
  for (var i=0; i < l; i++)
    b[i] = a[i];
  b.t = t;
  return b;
}
function $() { return $N(0, arguments); }
function $1() { return $N(1, arguments); }
function $2() { return $N(2, arguments); }
function $3() { return $N(3, arguments); }
function $4() { return $N(4, arguments); }
function $5() { return $N(5, arguments); }
function $6() { return $N(6, arguments); }
function $7() { return $N(7, arguments); }
function $8() { return $N(8, arguments); }
function $9() { return $N(9, arguments); }
function $t(a) { return a.t; }

function $xM(t) { return { $t: t }; }
function $xN(t, a) { a.$t = t; return a; }
function $xt(a) { return a.$t; }

function oc$$arefs(o, i) {
  return i < o.length ? o[i] : oc$Pervasives$[0]("index out of bounds");
}
function oc$$asets(o, i, v) {
  return i < o.length ? o[i] = v : oc$Pervasives$[0]("index out of bounds");
}

// mutable strings, argh

function oc$$ms(a) {
  this.a = a;
  this.length = a.length;
}

// XXX cache the string rep?
oc$$ms.prototype.toString = function () { return String.fromCharCode.apply(null, this.a); }

function oc$$lms(s) {
  var l = s.length;
  var a = new Array(l);
  for (var i = 0; i < l; i++)
    a[i] = s.charCodeAt(i);
  return new oc$$ms(a);
}
function oc$$cms(n) {
  return new oc$$ms(new Array(n));
}
function oc$$srefu(o, i) { return typeof o == "string" ? o.charCodeAt(i) : o.a[i]; }
function oc$$ssetu(o, i, v) { o.a[i] = v; }
function oc$$srefs(o, i) {
  return i < o.length ? oc$$srefu(o, i) : oc$Pervasives$[0]("index out of bounds");
}
function oc$$ssets(o, i, v) {
  return i < o.length ? oc$$ssetu(o, i, v) : oc$Pervasives$[0]("index out of bounds");
}

function oc$$seq(s1, s2) { return s1.toString() == s2.toString(); }
function oc$$sneq(s1, s2) { return s1.toString() != s2.toString(); }
function oc$$slt(s1, s2) { return s1.toString() < s2.toString(); }
function oc$$sgt(s1, s2) { return s1.toString() > s2.toString(); }
function oc$$slte(s1, s2) { return s1.toString() <= s2.toString(); }
function oc$$sgte(s1, s2) { return s1.toString() >= s2.toString(); }

/*@cc_on @if (@_win32 && @_jscript_version >= 5) if (!window.XMLHttpRequest)
window.XMLHttpRequest = function() { return new ActiveXObject('Microsoft.XMLHTTP') };
@end @*/
var oc$Pervasives$ =
  function () {
    var failwith$1026 = _f(function (s$1027) { throw $(Failure$19g, s$1027); });
    var invalid_arg$1028 = _f(function (s$1029) { throw $(Invalid_argument$18g, s$1029); });
    var Exit$1030 = $("Pervasives.Exit");
    var min$1038 = _f(function (x$1039, y$1040) { if (caml_lessequal(x$1039, y$1040)) return x$1039; return y$1040; });
    var max$1041 = _f(function (x$1042, y$1043) { if (caml_greaterequal(x$1042, y$1043)) return x$1042; return y$1043; });
    var abs$1060 = _f(function (x$1061) { if (x$1061 >= 0) return x$1061; return -x$1061; });
    var lnot$1065 = _f(function (x$1066) { return x$1066 ^ -1; });
    var min_int$1070 = 1 << (1 << 31 === 0 ? 30 : 62);
    var max_int$1071 = min_int$1070 - 1;
    var infinity$1107 = caml_int64_float_of_bits("9218868437227405312");
    var neg_infinity$1108 = caml_int64_float_of_bits("-4503599627370496");
    var nan$1109 = caml_int64_float_of_bits("9218868437227405313");
    var max_float$1110 = caml_int64_float_of_bits("9218868437227405311");
    var min_float$1111 = caml_int64_float_of_bits("4503599627370496");
    var epsilon_float$1112 = caml_int64_float_of_bits("4372995238176751616");
    var $5E$1128 = _f(function (s1$1129, s2$1130) { return s1$1129.toString() + s2$1130.toString(); });
    var char_of_int$1133 =
      _f(function (n$1134) { if (n$1134 < 0 || n$1134 > 255) return __(invalid_arg$1028, [ "char_of_int" ]); return n$1134; });
    var string_of_bool$1140 = _f(function (b$1141) { if (b$1141) return "true"; return "false"; });
    var bool_of_string$1142 =
      _f(function (param$1404) {
           if (!oc$$sneq(param$1404, "false")) return false;
           if (oc$$sneq(param$1404, "true")) return __(invalid_arg$1028, [ "bool_of_string" ]);
           return true;
         });
    var string_of_int$1143 = _f(function (n$1144) { return caml_format_int("%d", n$1144); });
    var String$1147 = $();
    var valid_float_lexem$1148 =
      _f(function (s$1149) {
           var l$1150 = s$1149.length;
           var loop$1151 =
             _f(function (i$1152) {
                  if (i$1152 >= l$1150) return __($5E$1128, [ s$1149, "." ]);
                  var match$1403 = oc$$srefs(s$1149, i$1152);
                  var $r58 = false;
                  r$58: {
                    {
                      if (!(match$1403 >= 48)) { { if (!(match$1403 !== 45)) { { $r58 = true; break r$58; } } return s$1149; } }
                      if (!(match$1403 >= 58)) { { $r58 = true; break r$58; } }
                      return s$1149;
                    }
                  }
                  if ($r58) return __(loop$1151, [ i$1152 + 1 ]);
                });
           return __(loop$1151, [ 0 ]);
         });
    var string_of_float$1153 = _f(function (f$1154) { return __(valid_float_lexem$1148, [ caml_format_float("%.12g", f$1154) ]); });
    var $40$1156 =
      _f(function (l1$1157, l2$1158) { if (l1$1157) return $(l1$1157[0], _($40$1156, [ l1$1157[1], l2$1158 ])); return l2$1158; });
    var stdin$1165 = caml_ml_open_descriptor_in(0);
    var stdout$1166 = caml_ml_open_descriptor_out(1);
    var stderr$1167 = caml_ml_open_descriptor_out(2);
    var open_out_gen$1188 =
      _f(function (mode$1189, perm$1190, name$1191) {
           return caml_ml_open_descriptor_out(caml_sys_open(name$1191, mode$1189, perm$1190));
         });
    var open_out$1192 = _f(function (name$1193) { return __(open_out_gen$1188, [ $(1, $(3, $(4, $(7, 0)))), 438, name$1193 ]); });
    var open_out_bin$1194 =
      _f(function (name$1195) { return __(open_out_gen$1188, [ $(1, $(3, $(4, $(6, 0)))), 438, name$1195 ]); });
    var flush_all$1198 =
      _f(function (param$1400) {
           var iter$1199 =
             _f(function (param$1401) {
                  if (param$1401) {
                    { try { caml_ml_flush(param$1401[0]); } catch (exn$1402) { } return __(iter$1199, [ param$1401[1] ]); }
                  }
                  return 0;
                });
           return __(iter$1199, [ caml_ml_out_channels_list(0) ]);
         });
    var output_string$1204 = _f(function (oc$1205, s$1206) { return caml_ml_output(oc$1205, s$1206, 0, s$1206.length); });
    var output$1207 =
      _f(function (oc$1208, s$1209, ofs$1210, len$1211) {
           if (ofs$1210 < 0 || (len$1211 < 0 || ofs$1210 > s$1209.length - len$1211)) return __(invalid_arg$1028, [ "output" ]);
           return caml_ml_output(oc$1208, s$1209, ofs$1210, len$1211);
         });
    var output_value$1215 = _f(function (chan$1216, v$1217) { return caml_output_value(chan$1216, v$1217, 0); });
    var close_out$1222 = _f(function (oc$1223) { caml_ml_flush(oc$1223); return caml_ml_close_channel(oc$1223); });
    var close_out_noerr$1224 =
      _f(function (oc$1225) {
           try { caml_ml_flush(oc$1225); } catch (exn$1399) { }
           try { return caml_ml_close_channel(oc$1225); } catch (exn$1398) { return 0; }
         });
    var open_in_gen$1227 =
      _f(function (mode$1228, perm$1229, name$1230) {
           return caml_ml_open_descriptor_in(caml_sys_open(name$1230, mode$1228, perm$1229));
         });
    var open_in$1231 = _f(function (name$1232) { return __(open_in_gen$1227, [ $(0, $(7, 0)), 0, name$1232 ]); });
    var open_in_bin$1233 = _f(function (name$1234) { return __(open_in_gen$1227, [ $(0, $(6, 0)), 0, name$1234 ]); });
    var input$1237 =
      _f(function (ic$1238, s$1239, ofs$1240, len$1241) {
           if (ofs$1240 < 0 || (len$1241 < 0 || ofs$1240 > s$1239.length - len$1241)) return __(invalid_arg$1028, [ "input" ]);
           return caml_ml_input(ic$1238, s$1239, ofs$1240, len$1241);
         });
    var unsafe_really_input$1242 =
      _f(function (ic$1243, s$1244, ofs$1245, len$1246) {
           if (len$1246 <= 0) return 0;
           var r$1247 = caml_ml_input(ic$1243, s$1244, ofs$1245, len$1246);
           if (r$1247 === 0) throw $(End_of_file$22g);
           return __(unsafe_really_input$1242, [ ic$1243, s$1244, ofs$1245 + r$1247, len$1246 - r$1247 ]);
         });
    var really_input$1248 =
      _f(function (ic$1249, s$1250, ofs$1251, len$1252) {
           if (ofs$1251 < 0 || (len$1252 < 0 || ofs$1251 > s$1250.length - len$1252))
             return __(invalid_arg$1028, [ "really_input" ]);
           return __(unsafe_really_input$1242, [ ic$1249, s$1250, ofs$1251, len$1252 ]);
         });
    var input_line$1254 =
      _f(function (chan$1255) {
           var build_result$1256 =
             _f(function (buf$1257, pos$1258, param$1397) {
                  if (param$1397) {
                    {
                      var hd$1259 = param$1397[0];
                      var len$1261 = hd$1259.length;
                      caml_blit_string(hd$1259, 0, buf$1257, pos$1258 - len$1261, len$1261);
                      return __(build_result$1256, [ buf$1257, pos$1258 - len$1261, param$1397[1] ]);
                    }
                  }
                  return buf$1257;
                });
           var scan$1262 =
             _f(function (accu$1263, len$1264) {
                  var n$1265 = caml_ml_input_scan_line(chan$1255);
                  if (!(n$1265 === 0)) {
                    {
                      if (n$1265 > 0) {
                        {
                          var res$1266 = oc$$cms(n$1265 - 1);
                          caml_ml_input(chan$1255, res$1266, 0, n$1265 - 1);
                          caml_ml_input_char(chan$1255);
                          if (accu$1263) {
                            {
                              var len$1267 = len$1264 + n$1265 - 1;
                              return __(build_result$1256, [ oc$$cms(len$1267), len$1267, $(res$1266, accu$1263) ]);
                            }
                          }
                          return res$1266;
                        }
                      }
                      var beg$1268 = oc$$cms(-n$1265);
                      caml_ml_input(chan$1255, beg$1268, 0, -n$1265);
                      return __(scan$1262, [ $(beg$1268, accu$1263), len$1264 - n$1265 ]);
                    }
                  }
                  if (accu$1263) return __(build_result$1256, [ oc$$cms(len$1264), len$1264, accu$1263 ]);
                  throw $(End_of_file$22g);
                });
           return __(scan$1262, [ 0, 0 ]);
         });
    var close_in_noerr$1276 =
      _f(function (ic$1277) { try { return caml_ml_close_channel(ic$1277); } catch (exn$1396) { return 0; } });
    var print_char$1279 = _f(function (c$1280) { return caml_ml_output_char(stdout$1166, c$1280); });
    var print_string$1281 = _f(function (s$1282) { return __(output_string$1204, [ stdout$1166, s$1282 ]); });
    var print_int$1283 =
      _f(function (i$1284) { return __(output_string$1204, [ stdout$1166, _(string_of_int$1143, [ i$1284 ]) ]); });
    var print_float$1285 =
      _f(function (f$1286) { return __(output_string$1204, [ stdout$1166, _(string_of_float$1153, [ f$1286 ]) ]); });
    var print_endline$1287 =
      _f(function (s$1288) {
           _(output_string$1204, [ stdout$1166, s$1288 ]);
           caml_ml_output_char(stdout$1166, 10);
           return caml_ml_flush(stdout$1166);
         });
    var print_newline$1289 = _f(function (param$1395) { caml_ml_output_char(stdout$1166, 10); return caml_ml_flush(stdout$1166); });
    var prerr_char$1290 = _f(function (c$1291) { return caml_ml_output_char(stderr$1167, c$1291); });
    var prerr_string$1292 = _f(function (s$1293) { return __(output_string$1204, [ stderr$1167, s$1293 ]); });
    var prerr_int$1294 =
      _f(function (i$1295) { return __(output_string$1204, [ stderr$1167, _(string_of_int$1143, [ i$1295 ]) ]); });
    var prerr_float$1296 =
      _f(function (f$1297) { return __(output_string$1204, [ stderr$1167, _(string_of_float$1153, [ f$1297 ]) ]); });
    var prerr_endline$1298 =
      _f(function (s$1299) {
           _(output_string$1204, [ stderr$1167, s$1299 ]);
           caml_ml_output_char(stderr$1167, 10);
           return caml_ml_flush(stderr$1167);
         });
    var prerr_newline$1300 = _f(function (param$1394) { caml_ml_output_char(stderr$1167, 10); return caml_ml_flush(stderr$1167); });
    var read_line$1301 = _f(function (param$1393) { caml_ml_flush(stdout$1166); return __(input_line$1254, [ stdin$1165 ]); });
    var read_int$1302 = _f(function (param$1392) { return caml_int_of_string(_(read_line$1301, [ 0 ])); });
    var read_float$1303 = _f(function (param$1391) { return caml_float_of_string(_(read_line$1301, [ 0 ])); });
    var LargeFile$1310 = $();
    var $5E$5E$1325 = _f(function (fmt1$1326, fmt2$1327) { return _($5E$1128, [ fmt1$1326, _($5E$1128, [ "%,", fmt2$1327 ]) ]); });
    var string_of_format$1328 =
      _f(function (fmt$1329) {
           var s$1330 = fmt$1329;
           var l$1331 = s$1330.length;
           var r$1332 = oc$$cms(l$1331);
           caml_blit_string(s$1330, 0, r$1332, 0, l$1331);
           return r$1332;
         });
    var exit_function$1334 = $(flush_all$1198);
    var at_exit$1335 =
      _f(function (f$1336) {
           var g$1337 = exit_function$1334[0];
           return exit_function$1334[0] = _f(function (param$1390) { _(f$1336, [ 0 ]); return __(g$1337, [ 0 ]); });
         });
    var do_at_exit$1338 = _f(function (param$1389) { return __(exit_function$1334[0], [ 0 ]); });
    var exit$1339 = _f(function (retcode$1340) { _(do_at_exit$1338, [ 0 ]); return caml_sys_exit(retcode$1340); });
    caml_register_named_value("Pervasives.do_at_exit", do_at_exit$1338);
    return $(invalid_arg$1028, failwith$1026, Exit$1030, min$1038, max$1041, abs$1060, max_int$1071, min_int$1070, lnot$1065,
             infinity$1107, neg_infinity$1108, nan$1109, max_float$1110, min_float$1111, epsilon_float$1112, $5E$1128,
             char_of_int$1133, string_of_bool$1140, bool_of_string$1142, string_of_int$1143, string_of_float$1153, $40$1156,
             stdin$1165, stdout$1166, stderr$1167, print_char$1279, print_string$1281, print_int$1283, print_float$1285,
             print_endline$1287, print_newline$1289, prerr_char$1290, prerr_string$1292, prerr_int$1294, prerr_float$1296,
             prerr_endline$1298, prerr_newline$1300, read_line$1301, read_int$1302, read_float$1303, open_out$1192,
             open_out_bin$1194, open_out_gen$1188, _f(function (prim$1357) { return caml_ml_flush(prim$1357); }), flush_all$1198,
             _f(function (prim$1359, prim$1358) { return caml_ml_output_char(prim$1359, prim$1358); }), output_string$1204,
             output$1207, _f(function (prim$1361, prim$1360) { return caml_ml_output_char(prim$1361, prim$1360); }),
             _f(function (prim$1363, prim$1362) { return caml_ml_output_int(prim$1363, prim$1362); }), output_value$1215,
             _f(function (prim$1365, prim$1364) { return caml_ml_seek_out(prim$1365, prim$1364); }),
             _f(function (prim$1366) { return caml_ml_pos_out(prim$1366); }),
             _f(function (prim$1367) { return caml_ml_channel_size(prim$1367); }), close_out$1222, close_out_noerr$1224,
             _f(function (prim$1369, prim$1368) { return caml_ml_set_binary_mode(prim$1369, prim$1368); }), open_in$1231,
             open_in_bin$1233, open_in_gen$1227, _f(function (prim$1370) { return caml_ml_input_char(prim$1370); }),
             input_line$1254, input$1237, really_input$1248, _f(function (prim$1371) { return caml_ml_input_char(prim$1371); }),
             _f(function (prim$1372) { return caml_ml_input_int(prim$1372); }),
             _f(function (prim$1373) { return caml_input_value(prim$1373); }),
             _f(function (prim$1375, prim$1374) { return caml_ml_seek_in(prim$1375, prim$1374); }),
             _f(function (prim$1376) { return caml_ml_pos_in(prim$1376); }),
             _f(function (prim$1377) { return caml_ml_channel_size(prim$1377); }),
             _f(function (prim$1378) { return caml_ml_close_channel(prim$1378); }), close_in_noerr$1276,
             _f(function (prim$1380, prim$1379) { return caml_ml_set_binary_mode(prim$1380, prim$1379); }),
             $(_f(function (prim$1382, prim$1381) { return caml_ml_seek_out_64(prim$1382, prim$1381); }),
               _f(function (prim$1383) { return caml_ml_pos_out_64(prim$1383); }),
               _f(function (prim$1384) { return caml_ml_channel_size_64(prim$1384); }),
               _f(function (prim$1386, prim$1385) { return caml_ml_seek_in_64(prim$1386, prim$1385); }),
               _f(function (prim$1387) { return caml_ml_pos_in_64(prim$1387); }),
               _f(function (prim$1388) { return caml_ml_channel_size_64(prim$1388); })), string_of_format$1328, $5E$5E$1325,
             exit$1339, at_exit$1335, valid_float_lexem$1148, unsafe_really_input$1242, do_at_exit$1338);
  }();
var oc$Array$ =
  function () {
    var init$1037 =
      _f(function (l$1038, f$1039) {
           if (l$1038 === 0) return $();
           var res$1040 = caml_make_vect(l$1038, _(f$1039, [ 0 ]));
           for (var i$1041 = 1; i$1041 <= -1 + l$1038; i$1041++) {
             (function (i$1041) { res$1040[i$1041] = _(f$1039, [ i$1041 ]); }(i$1041));
           }
           return res$1040;
         });
    var make_matrix$1042 =
      _f(function (sx$1043, sy$1044, init$1045) {
           var res$1046 = caml_make_vect(sx$1043, $());
           for (var x$1047 = 0; x$1047 <= -1 + sx$1043; x$1047++) {
             (function (x$1047) { res$1046[x$1047] = caml_make_vect(sy$1044, init$1045); }(x$1047));
           }
           return res$1046;
         });
    var copy$1049 =
      _f(function (a$1050) {
           var l$1051 = a$1050.length;
           if (l$1051 === 0) return $();
           var res$1052 = caml_make_vect(l$1051, a$1050[0]);
           for (var i$1053 = 1; i$1053 <= -1 + l$1051; i$1053++) {
             (function (i$1053) { res$1052[i$1053] = a$1050[i$1053]; }(i$1053));
           }
           return res$1052;
         });
    var append$1054 =
      _f(function (a1$1055, a2$1056) {
           var l1$1057 = a1$1055.length;
           var l2$1058 = a2$1056.length;
           if (l1$1057 === 0 && l2$1058 === 0) return $();
           var r$1059 = caml_make_vect(l1$1057 + l2$1058, (l1$1057 > 0 ? a1$1055 : a2$1056)[0]);
           for (var i$1060 = 0; i$1060 <= l1$1057 - 1; i$1060++) {
             (function (i$1060) { r$1059[i$1060] = a1$1055[i$1060]; }(i$1060));
           }
           for (var i$1061 = 0; i$1061 <= l2$1058 - 1; i$1061++) {
             (function (i$1061) { r$1059[i$1061 + l1$1057] = a2$1056[i$1061]; }(i$1061));
           }
           return r$1059;
         });
    var concat_aux$1062 =
      _f(function (init$1063, al$1064) {
           var size$1065 =
             _f(function (accu$1066, param$1234) {
                  if (param$1234) return __(size$1065, [ accu$1066 + (param$1234[0]).length, param$1234[1] ]);
                  return accu$1066;
                });
           var res$1069 = caml_make_vect(_(size$1065, [ 0, al$1064 ]), init$1063);
           var fill$1070 =
             _f(function (pos$1071, param$1233) {
                  if (param$1233) {
                    {
                      var h$1072 = param$1233[0];
                      for (var i$1074 = 0; i$1074 <= h$1072.length - 1; i$1074++) {
                        (function (i$1074) { res$1069[pos$1071 + i$1074] = h$1072[i$1074]; }(i$1074));
                      }
                      return __(fill$1070, [ pos$1071 + h$1072.length, param$1233[1] ]);
                    }
                  }
                  return 0;
                });
           _(fill$1070, [ 0, al$1064 ]);
           return res$1069;
         });
    var concat$1075 =
      _f(function (al$1076) {
           var find_init$1077 =
             _f(function (aa$1078) {
                  if (aa$1078) {
                    {
                      var a$1079 = aa$1078[0];
                      if (a$1079.length > 0) return __(concat_aux$1062, [ a$1079[0], aa$1078 ]);
                      return __(find_init$1077, [ aa$1078[1] ]);
                    }
                  }
                  return $();
                });
           return __(find_init$1077, [ al$1076 ]);
         });
    var sub$1081 =
      _f(function (a$1082, ofs$1083, len$1084) {
           if (ofs$1083 < 0 || (len$1084 < 0 || ofs$1083 > a$1082.length - len$1084))
             return __(oc$Pervasives$[0], [ "Array.sub" ]);
           if (len$1084 === 0) return $();
           var r$1085 = caml_make_vect(len$1084, a$1082[ofs$1083]);
           for (var i$1086 = 1; i$1086 <= len$1084 - 1; i$1086++) {
             (function (i$1086) { r$1085[i$1086] = a$1082[ofs$1083 + i$1086]; }(i$1086));
           }
           return r$1085;
         });
    var fill$1087 =
      _f(function (a$1088, ofs$1089, len$1090, v$1091) {
           if (ofs$1089 < 0 || (len$1090 < 0 || ofs$1089 > a$1088.length - len$1090))
             return __(oc$Pervasives$[0], [ "Array.fill" ]);
           for (var i$1092 = ofs$1089; i$1092 <= ofs$1089 + len$1090 - 1; i$1092++) {
             (function (i$1092) { a$1088[i$1092] = v$1091; }(i$1092));
           }
         });
    var blit$1093 =
      _f(function (a1$1094, ofs1$1095, a2$1096, ofs2$1097, len$1098) {
           if (len$1098 < 0 ||
                 (ofs1$1095 < 0 ||
                    (ofs1$1095 > a1$1094.length - len$1098 || (ofs2$1097 < 0 || ofs2$1097 > a2$1096.length - len$1098))))
             return __(oc$Pervasives$[0], [ "Array.blit" ]);
           if (ofs1$1095 < ofs2$1097)
             for (var i$1099 = len$1098 - 1; i$1099 >= 0; i$1099--) {
               (function (i$1099) { a2$1096[ofs2$1097 + i$1099] = a1$1094[ofs1$1095 + i$1099]; }(i$1099));
             }
           for (var i$1100 = 0; i$1100 <= len$1098 - 1; i$1100++) {
             (function (i$1100) { a2$1096[ofs2$1097 + i$1100] = a1$1094[ofs1$1095 + i$1100]; }(i$1100));
           }
         });
    var iter$1101 =
      _f(function (f$1102, a$1103) {
           for (var i$1104 = 0; i$1104 <= a$1103.length - 1; i$1104++) {
             (function (i$1104) { _(f$1102, [ a$1103[i$1104] ]); }(i$1104));
           }
         });
    var map$1105 =
      _f(function (f$1106, a$1107) {
           var l$1108 = a$1107.length;
           if (l$1108 === 0) return $();
           var r$1109 = caml_make_vect(l$1108, _(f$1106, [ a$1107[0] ]));
           for (var i$1110 = 1; i$1110 <= l$1108 - 1; i$1110++) {
             (function (i$1110) { r$1109[i$1110] = _(f$1106, [ a$1107[i$1110] ]); }(i$1110));
           }
           return r$1109;
         });
    var iteri$1111 =
      _f(function (f$1112, a$1113) {
           for (var i$1114 = 0; i$1114 <= a$1113.length - 1; i$1114++) {
             (function (i$1114) { _(f$1112, [ i$1114, a$1113[i$1114] ]); }(i$1114));
           }
         });
    var mapi$1115 =
      _f(function (f$1116, a$1117) {
           var l$1118 = a$1117.length;
           if (l$1118 === 0) return $();
           var r$1119 = caml_make_vect(l$1118, _(f$1116, [ 0, a$1117[0] ]));
           for (var i$1120 = 1; i$1120 <= l$1118 - 1; i$1120++) {
             (function (i$1120) { r$1119[i$1120] = _(f$1116, [ i$1120, a$1117[i$1120] ]); }(i$1120));
           }
           return r$1119;
         });
    var to_list$1121 =
      _f(function (a$1122) {
           var tolist$1123 =
             _f(function (i$1124, res$1125) {
                  if (i$1124 < 0) return res$1125;
                  return __(tolist$1123, [ i$1124 - 1, $(a$1122[i$1124], res$1125) ]);
                });
           return __(tolist$1123, [ a$1122.length - 1, 0 ]);
         });
    var list_length$1126 =
      _f(function (accu$1127, param$1232) {
           if (param$1232) return __(list_length$1126, [ 1 + accu$1127, param$1232[1] ]);
           return accu$1127;
         });
    var of_list$1130 =
      _f(function (l$1133) {
           if (l$1133) {
             {
               var a$1134 = caml_make_vect(_(list_length$1126, [ 0, l$1133 ]), l$1133[0]);
               var fill$1135 =
                 _f(function (i$1136, param$1231) {
                      if (param$1231) { { a$1134[i$1136] = param$1231[0]; return __(fill$1135, [ i$1136 + 1, param$1231[1] ]); } }
                      return a$1134;
                    });
               return __(fill$1135, [ 1, l$1133[1] ]);
             }
           }
           return $();
         });
    var fold_left$1139 =
      _f(function (f$1140, x$1141, a$1142) {
           var r$1143 = x$1141;
           for (var i$1144 = 0; i$1144 <= a$1142.length - 1; i$1144++) {
             (function (i$1144) { r$1143 = _(f$1140, [ r$1143, a$1142[i$1144] ]); }(i$1144));
           }
           return r$1143;
         });
    var fold_right$1145 =
      _f(function (f$1146, a$1147, x$1148) {
           var r$1149 = x$1148;
           for (var i$1150 = a$1147.length - 1; i$1150 >= 0; i$1150--) {
             (function (i$1150) { r$1149 = _(f$1146, [ a$1147[i$1150], r$1149 ]); }(i$1150));
           }
           return r$1149;
         });
    var Bottom$1151 = $("Array.Bottom");
    var sort$1152 =
      _f(function (cmp$1153, a$1154) {
           var maxson$1155 =
             _f(function (l$1156, i$1157) {
                  var i31$1158 = i$1157 + i$1157 + i$1157 + 1;
                  var x$1159 = i31$1158;
                  if (i31$1158 + 2 < l$1156) {
                    {
                      if (_(cmp$1153, [ oc$$arefs(a$1154, i31$1158), oc$$arefs(a$1154, i31$1158 + 1) ]) < 0)
                        x$1159 = i31$1158 + 1;
                      else;
                      if (_(cmp$1153, [ oc$$arefs(a$1154, x$1159), oc$$arefs(a$1154, i31$1158 + 2) ]) < 0)
                        x$1159 = i31$1158 + 2;
                      else;
                      return x$1159;
                    }
                  }
                  if (i31$1158 + 1 < l$1156 && _(cmp$1153, [ oc$$arefs(a$1154, i31$1158), oc$$arefs(a$1154, i31$1158 + 1) ]) < 0)
                    return i31$1158 + 1;
                  if (i31$1158 < l$1156) return i31$1158;
                  throw $(Bottom$1151, i$1157);
                });
           var trickledown$1160 =
             _f(function (l$1161, i$1162, e$1163) {
                  var j$1164 = _(maxson$1155, [ l$1161, i$1162 ]);
                  if (_(cmp$1153, [ oc$$arefs(a$1154, j$1164), e$1163 ]) > 0) {
                    {
                      oc$$asets(a$1154, i$1162, oc$$arefs(a$1154, j$1164));
                      return __(trickledown$1160, [ l$1161, j$1164, e$1163 ]);
                    }
                  }
                  return oc$$asets(a$1154, i$1162, e$1163);
                });
           var trickle$1165 =
             _f(function (l$1166, i$1167, e$1168) {
                  try {
                    return _(trickledown$1160, [ l$1166, i$1167, e$1168 ]);
                  }
                  catch (exn$1230) {
                    if (exn$1230[0] === Bottom$1151) return oc$$asets(a$1154, exn$1230[1], e$1168);
                    throw exn$1230;
                  }
                });
           var bubbledown$1170 =
             _f(function (l$1171, i$1172) {
                  var j$1173 = _(maxson$1155, [ l$1171, i$1172 ]);
                  oc$$asets(a$1154, i$1172, oc$$arefs(a$1154, j$1173));
                  return __(bubbledown$1170, [ l$1171, j$1173 ]);
                });
           var bubble$1174 =
             _f(function (l$1175, i$1176) {
                  try {
                    return _(bubbledown$1170, [ l$1175, i$1176 ]);
                  }
                  catch (exn$1229) {
                    if (exn$1229[0] === Bottom$1151) return exn$1229[1];
                    throw exn$1229;
                  }
                });
           var trickleup$1178 =
             _f(function (i$1179, e$1180) {
                  var father$1181 = (i$1179 - 1) / 3 >> 0;
                  if (i$1179 !== father$1181); else throw $(Assert_failure$26g, $("ocaml/stdlib/array.ml", 209, 4));
                  if (_(cmp$1153, [ oc$$arefs(a$1154, father$1181), e$1180 ]) < 0) {
                    {
                      oc$$asets(a$1154, i$1179, oc$$arefs(a$1154, father$1181));
                      if (father$1181 > 0) return __(trickleup$1178, [ father$1181, e$1180 ]);
                      return oc$$asets(a$1154, 0, e$1180);
                    }
                  }
                  return oc$$asets(a$1154, i$1179, e$1180);
                });
           var l$1182 = a$1154.length;
           for (var i$1183 = ((l$1182 + 1) / 3 >> 0) - 1; i$1183 >= 0; i$1183--) {
             (function (i$1183) { _(trickle$1165, [ l$1182, i$1183, oc$$arefs(a$1154, i$1183) ]); }(i$1183));
           }
           for (var i$1184 = l$1182 - 1; i$1184 >= 2; i$1184--) {
             (function (i$1184) {
                var e$1185 = oc$$arefs(a$1154, i$1184);
                oc$$asets(a$1154, i$1184, oc$$arefs(a$1154, 0));
                _(trickleup$1178, [ _(bubble$1174, [ i$1184, 0 ]), e$1185 ]);
              }(i$1184));
           }
           if (l$1182 > 1) {
             {
               var e$1186 = oc$$arefs(a$1154, 1);
               oc$$asets(a$1154, 1, oc$$arefs(a$1154, 0));
               return oc$$asets(a$1154, 0, e$1186);
             }
           }
           return 0;
         });
    var cutoff$1187 = 5;
    var stable_sort$1188 =
      _f(function (cmp$1189, a$1190) {
           var merge$1191 =
             _f(function (src1ofs$1192, src1len$1193, src2$1194, src2ofs$1195, src2len$1196, dst$1197, dstofs$1198) {
                  var src1r$1199 = src1ofs$1192 + src1len$1193;
                  var src2r$1200 = src2ofs$1195 + src2len$1196;
                  var loop$1201 =
                    _f(function (i1$1202, s1$1203, i2$1204, s2$1205, d$1206) {
                         if (_(cmp$1189, [ s1$1203, s2$1205 ]) <= 0) {
                           {
                             oc$$asets(dst$1197, d$1206, s1$1203);
                             var i1$1207 = i1$1202 + 1;
                             if (i1$1207 < src1r$1199)
                               return __(loop$1201, [ i1$1207, oc$$arefs(a$1190, i1$1207), i2$1204, s2$1205, d$1206 + 1 ]);
                             return __(blit$1093, [ src2$1194, i2$1204, dst$1197, d$1206 + 1, src2r$1200 - i2$1204 ]);
                           }
                         }
                         oc$$asets(dst$1197, d$1206, s2$1205);
                         var i2$1208 = i2$1204 + 1;
                         if (i2$1208 < src2r$1200)
                           return __(loop$1201, [ i1$1202, s1$1203, i2$1208, oc$$arefs(src2$1194, i2$1208), d$1206 + 1 ]);
                         return __(blit$1093, [ a$1190, i1$1202, dst$1197, d$1206 + 1, src1r$1199 - i1$1202 ]);
                       });
                  return __(loop$1201,
                            [
                              src1ofs$1192,
                              oc$$arefs(a$1190, src1ofs$1192),
                              src2ofs$1195,
                              oc$$arefs(src2$1194, src2ofs$1195),
                              dstofs$1198
                            ]);
                });
           var isortto$1209 =
             _f(function (srcofs$1210, dst$1211, dstofs$1212, len$1213) {
                  for (var i$1214 = 0; i$1214 <= len$1213 - 1; i$1214++) {
                    (function (i$1214) {
                       var e$1215 = oc$$arefs(a$1190, srcofs$1210 + i$1214);
                       var j$1216 = dstofs$1212 + i$1214 - 1;
                       while (j$1216 >= dstofs$1212 && _(cmp$1189, [ oc$$arefs(dst$1211, j$1216), e$1215 ]) > 0) {
                         { oc$$asets(dst$1211, j$1216 + 1, oc$$arefs(dst$1211, j$1216)); j$1216 = -1 + j$1216; }
                       }
                       oc$$asets(dst$1211, j$1216 + 1, e$1215);
                     }(i$1214));
                  }
                });
           var sortto$1217 =
             _f(function (srcofs$1218, dst$1219, dstofs$1220, len$1221) {
                  if (len$1221 <= cutoff$1187) return __(isortto$1209, [ srcofs$1218, dst$1219, dstofs$1220, len$1221 ]);
                  var l1$1222 = len$1221 / 2 >> 0;
                  var l2$1223 = len$1221 - l1$1222;
                  _(sortto$1217, [ srcofs$1218 + l1$1222, dst$1219, dstofs$1220 + l1$1222, l2$1223 ]);
                  _(sortto$1217, [ srcofs$1218, a$1190, srcofs$1218 + l2$1223, l1$1222 ]);
                  return __(merge$1191,
                            [ srcofs$1218 + l2$1223, l1$1222, dst$1219, dstofs$1220 + l1$1222, l2$1223, dst$1219, dstofs$1220 ]);
                });
           var l$1224 = a$1190.length;
           if (l$1224 <= cutoff$1187) return __(isortto$1209, [ 0, a$1190, 0, l$1224 ]);
           var l1$1225 = l$1224 / 2 >> 0;
           var l2$1226 = l$1224 - l1$1225;
           var t$1227 = caml_make_vect(l2$1226, oc$$arefs(a$1190, 0));
           _(sortto$1217, [ l1$1225, t$1227, 0, l2$1226 ]);
           _(sortto$1217, [ 0, a$1190, l2$1226, l1$1225 ]);
           return __(merge$1191, [ l2$1226, l1$1225, t$1227, 0, l2$1226, a$1190, 0 ]);
         });
    return $(init$1037, make_matrix$1042, make_matrix$1042, append$1054, concat$1075, sub$1081, copy$1049, fill$1087, blit$1093,
             to_list$1121, of_list$1130, iter$1101, map$1105, iteri$1111, mapi$1115, fold_left$1139, fold_right$1145, sort$1152,
             stable_sort$1188, stable_sort$1188);
  }();
var oc$ArrayLabels$ =
  function () {
    var include$1057 = oc$Array$;
    return $(include$1057[0], include$1057[1], include$1057[2], include$1057[3], 
             include$1057[4], include$1057[5], include$1057[6], include$1057[7], 
             include$1057[8], include$1057[9], include$1057[10], include$1057[11], 
             include$1057[12], include$1057[13], include$1057[14], include$1057[15], 
             include$1057[16], include$1057[17], include$1057[18], include$1057[19]);
  }();
var oc$Ocamljs$ =
  function () {
    var option_of_nullable$1046 = _f(function (x$1047) { if (x$1047 === null) return 0; return $(x$1047); });
    var nullable_of_option$1048 = _f(function (x$1049) { if (x$1049) return x$1049[0]; return null; });
    var is_null$1051 = _f(function (a$1052) { return caml_equal(a$1052, null); });
    var Inline$1240 = function () { var Jslib_ast$1234 = $(); var _loc$1239 = 0; return $(Jslib_ast$1234, _loc$1239); }();
    return $(option_of_nullable$1046, nullable_of_option$1048, is_null$1051, Inline$1240);
  }();
var oc$Dom$ = function () { var window$1704 = window; var document$1705 = document; return $(window$1704, document$1705); }();
var oc$Sudoku$ =
  function () {
    var D$1030 = oc$Dom$;
    var d$1031 = D$1030[1];
    var make_board$1032 =
      _f(function (param$1211) {
           var make_input$1033 =
             _f(function (param$1215) {
                  var input$1034 = _m(d$1031.createElement, d$1031, [ "input" ]);
                  _m(input$1034.setAttribute, input$1034, [ "type", "text" ]);
                  input$1034.size = 1;
                  input$1034.maxLength = 1;
                  var style$1035 = input$1034.style;
                  style$1035.border = "none";
                  style$1035.padding = "0px";
                  var enforce_digit$1036 =
                    _f(function (param$1216) {
                         var match$1217 = input$1034.value;
                         var $r30 = false;
                         r$30: {
                           {
                             if (!oc$$sneq(match$1217, "1")) { { $r30 = true; break r$30; } }
                             if (!oc$$sneq(match$1217, "2")) { { $r30 = true; break r$30; } }
                             if (!oc$$sneq(match$1217, "3")) { { $r30 = true; break r$30; } }
                             if (!oc$$sneq(match$1217, "4")) { { $r30 = true; break r$30; } }
                             if (!oc$$sneq(match$1217, "5")) { { $r30 = true; break r$30; } }
                             if (!oc$$sneq(match$1217, "6")) { { $r30 = true; break r$30; } }
                             if (!oc$$sneq(match$1217, "7")) { { $r30 = true; break r$30; } }
                             if (!oc$$sneq(match$1217, "8")) { { $r30 = true; break r$30; } }
                             if (!oc$$sneq(match$1217, "9")) { { $r30 = true; break r$30; } }
                             return input$1034.value = "";
                           }
                         }
                         if ($r30) return 0;
                       });
                  input$1034.onchange = enforce_digit$1036;
                  return input$1034;
                });
           var make_td$1037 =
             _f(function (i$1038, j$1039, input$1040) {
                  var td$1041 = _m(d$1031.createElement, d$1031, [ "td" ]);
                  var style$1042 = td$1041.style;
                  style$1042.borderStyle = "solid";
                  style$1042.borderColor = "#000000";
                  var widths$1043 =
                    _f(function (param$1214) {
                         var $r25 = false;
                         r$25: {
                           {
                             if (param$1214 < 0 || param$1214 > 8) { { $r25 = true; break r$25; } }
                             switch (param$1214)
                             {
                             case 0: return $(2, 0);
                             case 1: $r25 = true; break r$25;
                             case 2: return $(1, 1);
                             case 3: return $(1, 0);
                             case 4: $r25 = true; break r$25;
                             case 5: return $(1, 1);
                             case 6: return $(1, 0);
                             case 7: $r25 = true; break r$25;
                             case 8: return $(1, 2);
                             default: return null;
                             }
                           }
                         }
                         if ($r25) return $(1, 0);
                       });
                  var match$1213 = _(widths$1043, [ i$1038 ]);
                  var match$1212 = _(widths$1043, [ j$1039 ]);
                  var px$1048 =
                    _f(function (k$1049) { return __(oc$Pervasives$[15], [ _(oc$Pervasives$[19], [ k$1049 ]), "px" ]); });
                  style$1042.borderTopWidth = _(px$1048, [ match$1213[0] ]);
                  style$1042.borderBottomWidth = _(px$1048, [ match$1213[1] ]);
                  style$1042.borderLeftWidth = _(px$1048, [ match$1212[0] ]);
                  style$1042.borderRightWidth = _(px$1048, [ match$1212[1] ]);
                  _m(td$1041.appendChild, td$1041, [ input$1040 ]);
                  return td$1041;
                });
           var rows$1050 =
             _(oc$Array$[0],
               [
                 9,
                 _f(function (i$1051) {
                      return __(oc$Array$[0], [ 9, _f(function (j$1052) { return __(make_input$1033, [ 0 ]); }) ]);
                    })
               ]);
           var table$1053 = _m(d$1031.createElement, d$1031, [ "table" ]);
           _m(table$1053.setAttribute, table$1053, [ "cellpadding", "0px" ]);
           _m(table$1053.setAttribute, table$1053, [ "cellspacing", "0px" ]);
           var tbody$1054 = _m(d$1031.createElement, d$1031, [ "tbody" ]);
           _m(table$1053.appendChild, table$1053, [ tbody$1054 ]);
           _(oc$ArrayLabels$[13],
             [
               _f(function (i$1055, row$1056) {
                    var tr$1057 = _m(d$1031.createElement, d$1031, [ "tr" ]);
                    _(oc$ArrayLabels$[13],
                      [
                        _f(function (j$1058, cell$1059) {
                             var td$1060 = _(make_td$1037, [ i$1055, j$1058, cell$1059 ]);
                             _m(tr$1057.appendChild, tr$1057, [ td$1060 ]);
                             return 0;
                           }),
                        row$1056
                      ]);
                    _m(tbody$1054.appendChild, tbody$1054, [ tr$1057 ]);
                    return 0;
                  }),
               rows$1050
             ]);
           return $(rows$1050, table$1053);
         });
    var check_board$1061 =
      _f(function (rows$1062, param$1207) {
           var error$1063 =
             _f(function (i$1064, j$1065) {
                  var cell$1066 = oc$$arefs(oc$$arefs(rows$1062, i$1064), j$1065);
                  return cell$1066.style.backgroundColor = "#ff0000";
                });
           var check_set$1067 =
             _f(function (set$1068) {
                  var seen$1069 = caml_make_vect(9, 0);
                  return __(oc$ArrayLabels$[11],
                            [
                              _f(function (param$1208) {
                                   var j$1071 = param$1208[1];
                                   var i$1070 = param$1208[0];
                                   var cell$1072 = oc$$arefs(oc$$arefs(rows$1062, i$1070), j$1071);
                                   var v$1073 = cell$1072.value;
                                   if (oc$$sneq(v$1073, "")) {
                                     {
                                       var n$1074 = caml_int_of_string(v$1073);
                                       var match$1209 = oc$$arefs(seen$1069, n$1074 - 1);
                                       if (match$1209) {
                                         {
                                           var match$1210 = match$1209[0];
                                           _(error$1063, [ i$1070, j$1071 ]);
                                           return __(error$1063, [ match$1210[0], match$1210[1] ]);
                                         }
                                       }
                                       return oc$$asets(seen$1069, n$1074 - 1, $($(i$1070, j$1071)));
                                     }
                                   }
                                   return 0;
                                 }),
                              set$1068
                            ]);
                });
           var check_row$1077 =
             _f(function (i$1078) {
                  return __(check_set$1067, [ _(oc$Array$[0], [ 9, _f(function (j$1079) { return $(i$1078, j$1079); }) ]) ]);
                });
           var check_column$1080 =
             _f(function (j$1081) {
                  return __(check_set$1067, [ _(oc$Array$[0], [ 9, _f(function (i$1082) { return $(i$1082, j$1081); }) ]) ]);
                });
           var check_square$1083 =
             _f(function (i$1084, j$1085) {
                  var set$1086 =
                    _(oc$Array$[0],
                      [ 9, _f(function (k$1087) { return $(i$1084 * 3 + k$1087 % 3, j$1085 * 3 + (k$1087 / 3 >> 0)); }) ]);
                  return __(check_set$1067, [ set$1086 ]);
                });
           _(oc$ArrayLabels$[11],
             [
               _f(function (row$1088) {
                    return __(oc$ArrayLabels$[11],
                              [ _f(function (cell$1089) { return cell$1089.style.backgroundColor = "#ffffff"; }), row$1088 ]);
                  }),
               rows$1062
             ]);
           for (var i$1090 = 0; i$1090 <= 8; i$1090++) { (function (i$1090) { _(check_row$1077, [ i$1090 ]); }(i$1090)); }
           for (var j$1091 = 0; j$1091 <= 8; j$1091++) { (function (j$1091) { _(check_column$1080, [ j$1091 ]); }(j$1091)); }
           for (var i$1092 = 0; i$1092 <= 2; i$1092++) {
             (function (i$1092) {
                for (var j$1093 = 0; j$1093 <= 2; j$1093++) {
                  (function (j$1093) { _(check_square$1083, [ i$1092, j$1093 ]); }(j$1093));
                }
              }(i$1092));
           }
           return false;
         });
    var onload$1094 =
      _f(function (param$1205) {
           var match$1206 = _(make_board$1032, [ 0 ]);
           var check$1097 = _m(d$1031.getElementById, d$1031, [ "check" ]);
           check$1097.onclick = _(check_board$1061, [ match$1206[0] ]);
           var board$1098 = _m(d$1031.getElementById, d$1031, [ "board" ]);
           _m(board$1098.appendChild, board$1098, [ match$1206[1] ]);
           return 0;
         });
    (D$1030[0]).onload = onload$1094;
    return $(D$1030, d$1031, make_board$1032, check_board$1061, onload$1094);
  }();
var oc$Std_exit$ = (_(oc$Pervasives$[80], [ 0 ]), $());
return caml_named_value;
})();
