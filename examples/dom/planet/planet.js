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
  if (isNan(f)) return 4; // FP_nan
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

/* stuff below taken from js_of_ocaml/lib */

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
var oc$List$ =
  function () {
    var length_aux$1030 =
      _f(function (len$1031, param$1366) {
           if (param$1366) return __(length_aux$1030, [ len$1031 + 1, param$1366[1] ]);
           return len$1031;
         });
    var length$1034 = _f(function (l$1035) { return __(length_aux$1030, [ 0, l$1035 ]); });
    var hd$1036 = _f(function (param$1365) { if (param$1365) return param$1365[0]; return __(oc$Pervasives$[1], [ "hd" ]); });
    var tl$1039 = _f(function (param$1364) { if (param$1364) return param$1364[1]; return __(oc$Pervasives$[1], [ "tl" ]); });
    var nth$1042 =
      _f(function (l$1043, n$1044) {
           if (n$1044 < 0) return __(oc$Pervasives$[0], [ "List.nth" ]);
           var nth_aux$1045 =
             _f(function (l$1046, n$1047) {
                  if (!l$1046) return __(oc$Pervasives$[1], [ "nth" ]);
                  if (n$1047 === 0) return l$1046[0];
                  return __(nth_aux$1045, [ l$1046[1], n$1047 - 1 ]);
                });
           return __(nth_aux$1045, [ l$1043, n$1044 ]);
         });
    var append$1050 = oc$Pervasives$[21];
    var rev_append$1051 =
      _f(function (l1$1052, l2$1053) {
           if (l1$1052) return __(rev_append$1051, [ l1$1052[1], $(l1$1052[0], l2$1053) ]);
           return l2$1053;
         });
    var rev$1056 = _f(function (l$1057) { return __(rev_append$1051, [ l$1057, 0 ]); });
    var flatten$1058 =
      _f(function (param$1363) {
           if (param$1363) return __(oc$Pervasives$[21], [ param$1363[0], _(flatten$1058, [ param$1363[1] ]) ]);
           return 0;
         });
    var map$1062 =
      _f(function (f$1063, param$1362) {
           if (param$1362) {
             { var r$1066 = _(f$1063, [ param$1362[0] ]); return $(r$1066, _(map$1062, [ f$1063, param$1362[1] ])); }
           }
           return 0;
         });
    var rev_map$1067 =
      _f(function (f$1068, l$1069) {
           var rmap_f$1070 =
             _f(function (accu$1071, param$1361) {
                  if (param$1361) return __(rmap_f$1070, [ $(_(f$1068, [ param$1361[0] ]), accu$1071), param$1361[1] ]);
                  return accu$1071;
                });
           return __(rmap_f$1070, [ 0, l$1069 ]);
         });
    var iter$1074 =
      _f(function (f$1075, param$1360) {
           if (param$1360) { { _(f$1075, [ param$1360[0] ]); return __(iter$1074, [ f$1075, param$1360[1] ]); } }
           return 0;
         });
    var fold_left$1078 =
      _f(function (f$1079, accu$1080, l$1081) {
           if (l$1081) return __(fold_left$1078, [ f$1079, _(f$1079, [ accu$1080, l$1081[0] ]), l$1081[1] ]);
           return accu$1080;
         });
    var fold_right$1084 =
      _f(function (f$1085, l$1086, accu$1087) {
           if (l$1086) return __(f$1085, [ l$1086[0], _(fold_right$1084, [ f$1085, l$1086[1], accu$1087 ]) ]);
           return accu$1087;
         });
    var map2$1090 =
      _f(function (f$1091, l1$1092, l2$1093) {
           var $r34 = false;
           r$34: {
             {
               if (!l1$1092) { { if (l2$1093) { { $r34 = true; break r$34; } } return 0; } }
               if (!l2$1093) { { $r34 = true; break r$34; } }
               var r$1098 = _(f$1091, [ l1$1092[0], l2$1093[0] ]);
               return $(r$1098, _(map2$1090, [ f$1091, l1$1092[1], l2$1093[1] ]));
             }
           }
           if ($r34) return __(oc$Pervasives$[0], [ "List.map2" ]);
         });
    var rev_map2$1099 =
      _f(function (f$1100, l1$1101, l2$1102) {
           var rmap2_f$1103 =
             _f(function (accu$1104, l1$1105, l2$1106) {
                  var $r31 = false;
                  r$31: {
                    {
                      if (!l1$1105) { { if (l2$1106) { { $r31 = true; break r$31; } } return accu$1104; } }
                      if (!l2$1106) { { $r31 = true; break r$31; } }
                      return __(rmap2_f$1103, [ $(_(f$1100, [ l1$1105[0], l2$1106[0] ]), accu$1104), l1$1105[1], l2$1106[1] ]);
                    }
                  }
                  if ($r31) return __(oc$Pervasives$[0], [ "List.rev_map2" ]);
                });
           return __(rmap2_f$1103, [ 0, l1$1101, l2$1102 ]);
         });
    var iter2$1111 =
      _f(function (f$1112, l1$1113, l2$1114) {
           var $r30 = false;
           r$30: {
             {
               if (!l1$1113) { { if (l2$1114) { { $r30 = true; break r$30; } } return 0; } }
               if (!l2$1114) { { $r30 = true; break r$30; } }
               _(f$1112, [ l1$1113[0], l2$1114[0] ]);
               return __(iter2$1111, [ f$1112, l1$1113[1], l2$1114[1] ]);
             }
           }
           if ($r30) return __(oc$Pervasives$[0], [ "List.iter2" ]);
         });
    var fold_left2$1119 =
      _f(function (f$1120, accu$1121, l1$1122, l2$1123) {
           var $r29 = false;
           r$29: {
             {
               if (!l1$1122) { { if (l2$1123) { { $r29 = true; break r$29; } } return accu$1121; } }
               if (!l2$1123) { { $r29 = true; break r$29; } }
               return __(fold_left2$1119, [ f$1120, _(f$1120, [ accu$1121, l1$1122[0], l2$1123[0] ]), l1$1122[1], l2$1123[1] ]);
             }
           }
           if ($r29) return __(oc$Pervasives$[0], [ "List.fold_left2" ]);
         });
    var fold_right2$1128 =
      _f(function (f$1129, l1$1130, l2$1131, accu$1132) {
           var $r28 = false;
           r$28: {
             {
               if (!l1$1130) { { if (l2$1131) { { $r28 = true; break r$28; } } return accu$1132; } }
               if (!l2$1131) { { $r28 = true; break r$28; } }
               return __(f$1129, [ l1$1130[0], l2$1131[0], _(fold_right2$1128, [ f$1129, l1$1130[1], l2$1131[1], accu$1132 ]) ]);
             }
           }
           if ($r28) return __(oc$Pervasives$[0], [ "List.fold_right2" ]);
         });
    var for_all$1137 =
      _f(function (p$1138, param$1349) {
           if (param$1349) return _(p$1138, [ param$1349[0] ]) && _(for_all$1137, [ p$1138, param$1349[1] ]);
           return true;
         });
    var exists$1141 =
      _f(function (p$1142, param$1348) {
           if (param$1348) return _(p$1142, [ param$1348[0] ]) || _(exists$1141, [ p$1142, param$1348[1] ]);
           return false;
         });
    var for_all2$1145 =
      _f(function (p$1146, l1$1147, l2$1148) {
           var $r27 = false;
           r$27: {
             {
               if (!l1$1147) { { if (l2$1148) { { $r27 = true; break r$27; } } return true; } }
               if (!l2$1148) { { $r27 = true; break r$27; } }
               return _(p$1146, [ l1$1147[0], l2$1148[0] ]) && _(for_all2$1145, [ p$1146, l1$1147[1], l2$1148[1] ]);
             }
           }
           if ($r27) return __(oc$Pervasives$[0], [ "List.for_all2" ]);
         });
    var exists2$1153 =
      _f(function (p$1154, l1$1155, l2$1156) {
           var $r26 = false;
           r$26: {
             {
               if (!l1$1155) { { if (l2$1156) { { $r26 = true; break r$26; } } return false; } }
               if (!l2$1156) { { $r26 = true; break r$26; } }
               return _(p$1154, [ l1$1155[0], l2$1156[0] ]) || _(exists2$1153, [ p$1154, l1$1155[1], l2$1156[1] ]);
             }
           }
           if ($r26) return __(oc$Pervasives$[0], [ "List.exists2" ]);
         });
    var mem$1161 =
      _f(function (x$1162, param$1343) {
           if (param$1343) return caml_compare(param$1343[0], x$1162) === 0 || _(mem$1161, [ x$1162, param$1343[1] ]);
           return false;
         });
    var memq$1165 =
      _f(function (x$1166, param$1342) {
           if (param$1342) return param$1342[0] === x$1166 || _(memq$1165, [ x$1166, param$1342[1] ]);
           return false;
         });
    var assoc$1169 =
      _f(function (x$1170, param$1340) {
           if (param$1340) {
             {
               var match$1341 = param$1340[0];
               if (caml_compare(match$1341[0], x$1170) === 0) return match$1341[1];
               return __(assoc$1169, [ x$1170, param$1340[1] ]);
             }
           }
           throw $(Not_found$20g);
         });
    var assq$1174 =
      _f(function (x$1175, param$1338) {
           if (param$1338) {
             {
               var match$1339 = param$1338[0];
               if (match$1339[0] === x$1175) return match$1339[1];
               return __(assq$1174, [ x$1175, param$1338[1] ]);
             }
           }
           throw $(Not_found$20g);
         });
    var mem_assoc$1179 =
      _f(function (x$1180, param$1336) {
           if (param$1336) return caml_compare(param$1336[0][0], x$1180) === 0 || _(mem_assoc$1179, [ x$1180, param$1336[1] ]);
           return false;
         });
    var mem_assq$1184 =
      _f(function (x$1185, param$1334) {
           if (param$1334) return param$1334[0][0] === x$1185 || _(mem_assq$1184, [ x$1185, param$1334[1] ]);
           return false;
         });
    var remove_assoc$1189 =
      _f(function (x$1190, param$1333) {
           if (param$1333) {
             {
               var l$1194 = param$1333[1];
               var pair$1193 = param$1333[0];
               if (caml_compare(pair$1193[0], x$1190) === 0) return l$1194;
               return $(pair$1193, _(remove_assoc$1189, [ x$1190, l$1194 ]));
             }
           }
           return 0;
         });
    var remove_assq$1195 =
      _f(function (x$1196, param$1332) {
           if (param$1332) {
             {
               var l$1200 = param$1332[1];
               var pair$1199 = param$1332[0];
               if (pair$1199[0] === x$1196) return l$1200;
               return $(pair$1199, _(remove_assq$1195, [ x$1196, l$1200 ]));
             }
           }
           return 0;
         });
    var find$1201 =
      _f(function (p$1202, param$1331) {
           if (param$1331) {
             {
               var x$1203 = param$1331[0];
               if (_(p$1202, [ x$1203 ])) return x$1203;
               return __(find$1201, [ p$1202, param$1331[1] ]);
             }
           }
           throw $(Not_found$20g);
         });
    var find_all$1205 =
      _f(function (p$1206) {
           var find$1207 =
             _f(function (accu$1208, param$1330) {
                  if (param$1330) {
                    {
                      var l$1210 = param$1330[1];
                      var x$1209 = param$1330[0];
                      if (_(p$1206, [ x$1209 ])) return __(find$1207, [ $(x$1209, accu$1208), l$1210 ]);
                      return __(find$1207, [ accu$1208, l$1210 ]);
                    }
                  }
                  return __(rev$1056, [ accu$1208 ]);
                });
           return __(find$1207, [ 0 ]);
         });
    var partition$1212 =
      _f(function (p$1213, l$1214) {
           var part$1215 =
             _f(function (yes$1216, no$1217, param$1329) {
                  if (param$1329) {
                    {
                      var l$1219 = param$1329[1];
                      var x$1218 = param$1329[0];
                      if (_(p$1213, [ x$1218 ])) return __(part$1215, [ $(x$1218, yes$1216), no$1217, l$1219 ]);
                      return __(part$1215, [ yes$1216, $(x$1218, no$1217), l$1219 ]);
                    }
                  }
                  return $(_(rev$1056, [ yes$1216 ]), _(rev$1056, [ no$1217 ]));
                });
           return __(part$1215, [ 0, 0, l$1214 ]);
         });
    var split$1220 =
      _f(function (param$1326) {
           if (param$1326) {
             {
               var match$1328 = param$1326[0];
               var match$1327 = _(split$1220, [ param$1326[1] ]);
               return $($(match$1328[0], match$1327[0]), $(match$1328[1], match$1327[1]));
             }
           }
           return $(0, 0);
         });
    var combine$1226 =
      _f(function (l1$1227, l2$1228) {
           var $r21 = false;
           r$21: {
             {
               if (!l1$1227) { { if (l2$1228) { { $r21 = true; break r$21; } } return 0; } }
               if (!l2$1228) { { $r21 = true; break r$21; } }
               return $($(l1$1227[0], l2$1228[0]), _(combine$1226, [ l1$1227[1], l2$1228[1] ]));
             }
           }
           if ($r21) return __(oc$Pervasives$[0], [ "List.combine" ]);
         });
    var merge$1233 =
      _f(function (cmp$1234, l1$1235, l2$1236) {
           if (!l1$1235) return l2$1236;
           if (l2$1236) {
             {
               var h2$1241 = l2$1236[0];
               var h1$1239 = l1$1235[0];
               if (_(cmp$1234, [ h1$1239, h2$1241 ]) <= 0) return $(h1$1239, _(merge$1233, [ cmp$1234, l1$1235[1], l2$1236 ]));
               return $(h2$1241, _(merge$1233, [ cmp$1234, l1$1235, l2$1236[1] ]));
             }
           }
           return l1$1235;
         });
    var chop$1243 =
      _f(function (k$1244, l$1245) {
           if (k$1244 === 0) return l$1245;
           if (l$1245) return __(chop$1243, [ k$1244 - 1, l$1245[1] ]);
           throw $(Assert_failure$26g, $("ocaml/stdlib/list.ml", 213, 11));
         });
    var stable_sort$1248 =
      _f(function (cmp$1249, l$1250) {
           var rev_merge$1251 =
             _f(function (l1$1252, l2$1253, accu$1254) {
                  if (!l1$1252) return __(rev_append$1051, [ l2$1253, accu$1254 ]);
                  if (l2$1253) {
                    {
                      var h2$1259 = l2$1253[0];
                      var h1$1257 = l1$1252[0];
                      if (_(cmp$1249, [ h1$1257, h2$1259 ]) <= 0)
                        return __(rev_merge$1251, [ l1$1252[1], l2$1253, $(h1$1257, accu$1254) ]);
                      return __(rev_merge$1251, [ l1$1252, l2$1253[1], $(h2$1259, accu$1254) ]);
                    }
                  }
                  return __(rev_append$1051, [ l1$1252, accu$1254 ]);
                });
           var rev_merge_rev$1261 =
             _f(function (l1$1262, l2$1263, accu$1264) {
                  if (!l1$1262) return __(rev_append$1051, [ l2$1263, accu$1264 ]);
                  if (l2$1263) {
                    {
                      var h2$1269 = l2$1263[0];
                      var h1$1267 = l1$1262[0];
                      if (_(cmp$1249, [ h1$1267, h2$1269 ]) > 0)
                        return __(rev_merge_rev$1261, [ l1$1262[1], l2$1263, $(h1$1267, accu$1264) ]);
                      return __(rev_merge_rev$1261, [ l1$1262, l2$1263[1], $(h2$1269, accu$1264) ]);
                    }
                  }
                  return __(rev_append$1051, [ l1$1262, accu$1264 ]);
                });
           var sort$1271 =
             _f(function (n$1273, l$1274) {
                  var $r9 = false;
                  r$9: {
                    {
                      if (!(n$1273 !== 2)) {
                        {
                          if (!l$1274) { { $r9 = true; break r$9; } }
                          var match$1306 = l$1274[1];
                          if (!match$1306) { { $r9 = true; break r$9; } }
                          var x2$1276 = match$1306[0];
                          var x1$1275 = l$1274[0];
                          if (_(cmp$1249, [ x1$1275, x2$1276 ]) <= 0) return $(x1$1275, $(x2$1276, 0));
                          return $(x2$1276, $(x1$1275, 0));
                        }
                      }
                      if (n$1273 !== 3) { { $r9 = true; break r$9; } }
                      if (!l$1274) { { $r9 = true; break r$9; } }
                      var match$1308 = l$1274[1];
                      if (!match$1308) { { $r9 = true; break r$9; } }
                      var match$1309 = match$1308[1];
                      if (!match$1309) { { $r9 = true; break r$9; } }
                      var x3$1279 = match$1309[0];
                      var x2$1278 = match$1308[0];
                      var x1$1277 = l$1274[0];
                      if (!(_(cmp$1249, [ x1$1277, x2$1278 ]) <= 0)) {
                        {
                          if (_(cmp$1249, [ x1$1277, x3$1279 ]) <= 0) return $(x2$1278, $(x1$1277, $(x3$1279, 0)));
                          if (_(cmp$1249, [ x2$1278, x3$1279 ]) <= 0) return $(x2$1278, $(x3$1279, $(x1$1277, 0)));
                          return $(x3$1279, $(x2$1278, $(x1$1277, 0)));
                        }
                      }
                      if (_(cmp$1249, [ x2$1278, x3$1279 ]) <= 0) return $(x1$1277, $(x2$1278, $(x3$1279, 0)));
                      if (_(cmp$1249, [ x1$1277, x3$1279 ]) <= 0) return $(x1$1277, $(x3$1279, $(x2$1278, 0)));
                      return $(x3$1279, $(x1$1277, $(x2$1278, 0)));
                    }
                  }
                  if ($r9) {
                    {
                      var n1$1282 = n$1273 >>> 1;
                      var n2$1283 = n$1273 - n1$1282;
                      var l2$1284 = _(chop$1243, [ n1$1282, l$1274 ]);
                      var s1$1285 = _(rev_sort$1272, [ n1$1282, l$1274 ]);
                      var s2$1286 = _(rev_sort$1272, [ n2$1283, l2$1284 ]);
                      return __(rev_merge_rev$1261, [ s1$1285, s2$1286, 0 ]);
                    }
                  }
                });
           var rev_sort$1272 =
             _f(function (n$1287, l$1288) {
                  var $r15 = false;
                  r$15: {
                    {
                      if (!(n$1287 !== 2)) {
                        {
                          if (!l$1288) { { $r15 = true; break r$15; } }
                          var match$1313 = l$1288[1];
                          if (!match$1313) { { $r15 = true; break r$15; } }
                          var x2$1290 = match$1313[0];
                          var x1$1289 = l$1288[0];
                          if (_(cmp$1249, [ x1$1289, x2$1290 ]) > 0) return $(x1$1289, $(x2$1290, 0));
                          return $(x2$1290, $(x1$1289, 0));
                        }
                      }
                      if (n$1287 !== 3) { { $r15 = true; break r$15; } }
                      if (!l$1288) { { $r15 = true; break r$15; } }
                      var match$1315 = l$1288[1];
                      if (!match$1315) { { $r15 = true; break r$15; } }
                      var match$1316 = match$1315[1];
                      if (!match$1316) { { $r15 = true; break r$15; } }
                      var x3$1293 = match$1316[0];
                      var x2$1292 = match$1315[0];
                      var x1$1291 = l$1288[0];
                      if (!(_(cmp$1249, [ x1$1291, x2$1292 ]) > 0)) {
                        {
                          if (_(cmp$1249, [ x1$1291, x3$1293 ]) > 0) return $(x2$1292, $(x1$1291, $(x3$1293, 0)));
                          if (_(cmp$1249, [ x2$1292, x3$1293 ]) > 0) return $(x2$1292, $(x3$1293, $(x1$1291, 0)));
                          return $(x3$1293, $(x2$1292, $(x1$1291, 0)));
                        }
                      }
                      if (_(cmp$1249, [ x2$1292, x3$1293 ]) > 0) return $(x1$1291, $(x2$1292, $(x3$1293, 0)));
                      if (_(cmp$1249, [ x1$1291, x3$1293 ]) > 0) return $(x1$1291, $(x3$1293, $(x2$1292, 0)));
                      return $(x3$1293, $(x1$1291, $(x2$1292, 0)));
                    }
                  }
                  if ($r15) {
                    {
                      var n1$1296 = n$1287 >>> 1;
                      var n2$1297 = n$1287 - n1$1296;
                      var l2$1298 = _(chop$1243, [ n1$1296, l$1288 ]);
                      var s1$1299 = _(sort$1271, [ n1$1296, l$1288 ]);
                      var s2$1300 = _(sort$1271, [ n2$1297, l2$1298 ]);
                      return __(rev_merge$1251, [ s1$1299, s2$1300, 0 ]);
                    }
                  }
                });
           var len$1301 = _(length$1034, [ l$1250 ]);
           if (len$1301 < 2) return l$1250;
           return __(sort$1271, [ len$1301, l$1250 ]);
         });
    return $(length$1034, hd$1036, tl$1039, nth$1042, rev$1056, append$1050, rev_append$1051, flatten$1058, flatten$1058,
             iter$1074, map$1062, rev_map$1067, fold_left$1078, fold_right$1084, iter2$1111, map2$1090, rev_map2$1099,
             fold_left2$1119, fold_right2$1128, for_all$1137, exists$1141, for_all2$1145, exists2$1153, mem$1161, memq$1165,
             find$1201, find_all$1205, find_all$1205, partition$1212, assoc$1169, assq$1174, mem_assoc$1179, mem_assq$1184,
             remove_assoc$1189, remove_assq$1195, split$1220, combine$1226, stable_sort$1248, stable_sort$1248, stable_sort$1248,
             merge$1233);
  }();
var oc$Char$ =
  function () {
    var chr$1032 =
      _f(function (n$1033) { if (n$1033 < 0 || n$1033 > 255) return __(oc$Pervasives$[0], [ "Char.chr" ]); return n$1033; });
    var escaped$1038 =
      _f(function (c$1039) {
           var $r7 = false;
           r$7: {
             {
               if (!(c$1039 !== 39)) return "\\\'";
               if (!(c$1039 !== 92)) return "\\\\";
               if (c$1039 >= 14) { { $r7 = true; break r$7; } }
               switch (c$1039)
               {
               case 0: $r7 = true; break r$7;
               case 1: $r7 = true; break r$7;
               case 2: $r7 = true; break r$7;
               case 3: $r7 = true; break r$7;
               case 4: $r7 = true; break r$7;
               case 5: $r7 = true; break r$7;
               case 6: $r7 = true; break r$7;
               case 7: $r7 = true; break r$7;
               case 8: return "\\b";
               case 9: return "\\t";
               case 10: return "\\n";
               case 11: $r7 = true; break r$7;
               case 12: $r7 = true; break r$7;
               case 13: return "\\r";
               default: return null;
               }
             }
           }
           if ($r7) {
             {
               if (caml_is_printable(c$1039)) { { var s$1040 = oc$$cms(1); oc$$ssetu(s$1040, 0, c$1039); return s$1040; } }
               var n$1041 = c$1039;
               var s$1042 = oc$$cms(4);
               oc$$ssetu(s$1042, 0, 92);
               oc$$ssetu(s$1042, 1, 48 + (n$1041 / 100 >> 0));
               oc$$ssetu(s$1042, 2, 48 + (n$1041 / 10 >> 0) % 10);
               oc$$ssetu(s$1042, 3, 48 + n$1041 % 10);
               return s$1042;
             }
           }
         });
    var lowercase$1043 =
      _f(function (c$1044) {
           if (c$1044 >= 65 && c$1044 <= 90 || (c$1044 >= 192 && c$1044 <= 214 || c$1044 >= 216 && c$1044 <= 222))
             return c$1044 + 32;
           return c$1044;
         });
    var uppercase$1045 =
      _f(function (c$1046) {
           if (c$1046 >= 97 && c$1046 <= 122 || (c$1046 >= 224 && c$1046 <= 246 || c$1046 >= 248 && c$1046 <= 254))
             return c$1046 - 32;
           return c$1046;
         });
    var compare$1048 = _f(function (c1$1049, c2$1050) { return c1$1049 - c2$1050; });
    return $(chr$1032, escaped$1038, lowercase$1043, uppercase$1045, compare$1048);
  }();
var oc$String$ =
  function () {
    var make$1038 =
      _f(function (n$1039, c$1040) { var s$1041 = oc$$cms(n$1039); caml_fill_string(s$1041, 0, n$1039, c$1040); return s$1041; });
    var copy$1042 =
      _f(function (s$1043) {
           var len$1044 = s$1043.length;
           var r$1045 = oc$$cms(len$1044);
           caml_blit_string(s$1043, 0, r$1045, 0, len$1044);
           return r$1045;
         });
    var sub$1046 =
      _f(function (s$1047, ofs$1048, len$1049) {
           if (ofs$1048 < 0 || (len$1049 < 0 || ofs$1048 > s$1047.length - len$1049))
             return __(oc$Pervasives$[0], [ "String.sub" ]);
           var r$1050 = oc$$cms(len$1049);
           caml_blit_string(s$1047, ofs$1048, r$1050, 0, len$1049);
           return r$1050;
         });
    var fill$1051 =
      _f(function (s$1052, ofs$1053, len$1054, c$1055) {
           if (ofs$1053 < 0 || (len$1054 < 0 || ofs$1053 > s$1052.length - len$1054))
             return __(oc$Pervasives$[0], [ "String.fill" ]);
           return caml_fill_string(s$1052, ofs$1053, len$1054, c$1055);
         });
    var blit$1056 =
      _f(function (s1$1057, ofs1$1058, s2$1059, ofs2$1060, len$1061) {
           if (len$1061 < 0 ||
                 (ofs1$1058 < 0 ||
                    (ofs1$1058 > s1$1057.length - len$1061 || (ofs2$1060 < 0 || ofs2$1060 > s2$1059.length - len$1061))))
             return __(oc$Pervasives$[0], [ "String.blit" ]);
           return caml_blit_string(s1$1057, ofs1$1058, s2$1059, ofs2$1060, len$1061);
         });
    var iter$1062 =
      _f(function (f$1063, a$1064) {
           for (var i$1065 = 0; i$1065 <= a$1064.length - 1; i$1065++) {
             (function (i$1065) { _(f$1063, [ oc$$srefu(a$1064, i$1065) ]); }(i$1065));
           }
         });
    var concat$1066 =
      _f(function (sep$1067, l$1068) {
           if (l$1068) {
             {
               var hd$1069 = l$1068[0];
               var num$1071 = $(0);
               var len$1072 = $(0);
               _(oc$List$[9],
                 [ _f(function (s$1073) { num$1071[0]++; return len$1072[0] = len$1072[0] + s$1073.length; }), l$1068 ]);
               var r$1074 = oc$$cms(len$1072[0] + sep$1067.length * (num$1071[0] - 1));
               caml_blit_string(hd$1069, 0, r$1074, 0, hd$1069.length);
               var pos$1075 = $(hd$1069.length);
               _(oc$List$[9],
                 [
                   _f(function (s$1076) {
                        caml_blit_string(sep$1067, 0, r$1074, pos$1075[0], sep$1067.length);
                        pos$1075[0] = pos$1075[0] + sep$1067.length;
                        caml_blit_string(s$1076, 0, r$1074, pos$1075[0], s$1076.length);
                        return pos$1075[0] = pos$1075[0] + s$1076.length;
                      }),
                   l$1068[1]
                 ]);
               return r$1074;
             }
           }
           return "";
         });
    var escaped$1080 =
      _f(function (s$1081) {
           var n$1082 = 0;
           for (var i$1083 = 0; i$1083 <= s$1081.length - 1; i$1083++) {
             (function (i$1083) {
                n$1082 =
                  n$1082 +
                    function () {
                      var c$1084 = oc$$srefu(s$1081, i$1083);
                      var $r26 = false;
                      r$26: {
                        {
                          var $r27 = false;
                          r$27: {
                            {
                              if (!(c$1084 >= 14)) {
                                {
                                  if (!(c$1084 >= 11)) {
                                    { if (!(c$1084 >= 8)) { { $r27 = true; break r$27; } } $r26 = true; break r$26; }
                                  }
                                  if (!(c$1084 >= 13)) { { $r27 = true; break r$27; } }
                                  $r26 = true;
                                  break r$26;
                                }
                              }
                              if (!(c$1084 !== 34)) { { $r26 = true; break r$26; } }
                              if (!(c$1084 !== 92)) { { $r26 = true; break r$26; } }
                              $r27 = true;
                              break r$27;
                            }
                          }
                          if ($r27) { { if (caml_is_printable(c$1084)) return 1; return 4; } }
                        }
                      }
                      if ($r26) return 2;
                    }();
              }(i$1083));
           }
           if (n$1082 === s$1081.length) return s$1081;
           var s$27$1085 = oc$$cms(n$1082);
           n$1082 = 0;
           for (var i$1086 = 0; i$1086 <= s$1081.length - 1; i$1086++) {
             (function (i$1086) {
                var c$1087 = oc$$srefu(s$1081, i$1086);
                var $r24 = false;
                r$24: {
                  {
                    var switcher$1150 = -34 + c$1087;
                    if (!(switcher$1150 < 0 || switcher$1150 > 58)) {
                      {
                        if (!(-1 + switcher$1150 < 0 || -1 + switcher$1150 > 56)) { { $r24 = true; break r$24; } }
                        oc$$ssetu(s$27$1085, n$1082, 92);
                        n$1082 = 1 + n$1082;
                        oc$$ssetu(s$27$1085, n$1082, c$1087);
                      }
                    }
                    else {
                      {
                        if (switcher$1150 >= -20) { { $r24 = true; break r$24; } }
                        var s$1153 = 34 + switcher$1150;
                        switch (s$1153)
                        {
                        case 0: $r24 = true; break r$24;
                        case 1: $r24 = true; break r$24;
                        case 2: $r24 = true; break r$24;
                        case 3: $r24 = true; break r$24;
                        case 4: $r24 = true; break r$24;
                        case 5: $r24 = true; break r$24;
                        case 6: $r24 = true; break r$24;
                        case 7: $r24 = true; break r$24;
                        case 8: oc$$ssetu(s$27$1085, n$1082, 92); n$1082 = 1 + n$1082; oc$$ssetu(s$27$1085, n$1082, 98); break;
                        case 9: oc$$ssetu(s$27$1085, n$1082, 92); n$1082 = 1 + n$1082; oc$$ssetu(s$27$1085, n$1082, 116); break;
                        case 10: oc$$ssetu(s$27$1085, n$1082, 92); n$1082 = 1 + n$1082; oc$$ssetu(s$27$1085, n$1082, 110); break;
                        case 11: $r24 = true; break r$24;
                        case 12: $r24 = true; break r$24;
                        case 13: oc$$ssetu(s$27$1085, n$1082, 92); n$1082 = 1 + n$1082; oc$$ssetu(s$27$1085, n$1082, 114); break;
                        default: null;
                        }
                      }
                    }
                  }
                }
                if ($r24)
                  if (caml_is_printable(c$1087))
                    oc$$ssetu(s$27$1085, n$1082, c$1087);
                  else {
                    {
                      var a$1089 = c$1087;
                      oc$$ssetu(s$27$1085, n$1082, 92);
                      n$1082 = 1 + n$1082;
                      oc$$ssetu(s$27$1085, n$1082, 48 + (a$1089 / 100 >> 0));
                      n$1082 = 1 + n$1082;
                      oc$$ssetu(s$27$1085, n$1082, 48 + (a$1089 / 10 >> 0) % 10);
                      n$1082 = 1 + n$1082;
                      oc$$ssetu(s$27$1085, n$1082, 48 + a$1089 % 10);
                    }
                  }
                n$1082 = 1 + n$1082;
              }(i$1086));
           }
           return s$27$1085;
         });
    var map$1090 =
      _f(function (f$1091, s$1092) {
           var l$1093 = s$1092.length;
           if (l$1093 === 0) return s$1092;
           var r$1094 = oc$$cms(l$1093);
           for (var i$1095 = 0; i$1095 <= l$1093 - 1; i$1095++) {
             (function (i$1095) { oc$$ssetu(r$1094, i$1095, _(f$1091, [ oc$$srefu(s$1092, i$1095) ])); }(i$1095));
           }
           return r$1094;
         });
    var uppercase$1096 = _f(function (s$1097) { return __(map$1090, [ oc$Char$[3], s$1097 ]); });
    var lowercase$1098 = _f(function (s$1099) { return __(map$1090, [ oc$Char$[2], s$1099 ]); });
    var apply1$1100 =
      _f(function (f$1101, s$1102) {
           if (s$1102.length === 0) return s$1102;
           var r$1103 = _(copy$1042, [ s$1102 ]);
           oc$$ssetu(r$1103, 0, _(f$1101, [ oc$$srefu(s$1102, 0) ]));
           return r$1103;
         });
    var capitalize$1104 = _f(function (s$1105) { return __(apply1$1100, [ oc$Char$[3], s$1105 ]); });
    var uncapitalize$1106 = _f(function (s$1107) { return __(apply1$1100, [ oc$Char$[2], s$1107 ]); });
    var index_rec$1108 =
      _f(function (s$1109, lim$1110, i$1111, c$1112) {
           if (i$1111 >= lim$1110) throw $(Not_found$20g);
           if (oc$$srefu(s$1109, i$1111) === c$1112) return i$1111;
           return __(index_rec$1108, [ s$1109, lim$1110, i$1111 + 1, c$1112 ]);
         });
    var index$1113 = _f(function (s$1114, c$1115) { return __(index_rec$1108, [ s$1114, s$1114.length, 0, c$1115 ]); });
    var index_from$1116 =
      _f(function (s$1117, i$1118, c$1119) {
           var l$1120 = s$1117.length;
           if (i$1118 < 0 || i$1118 > l$1120) return __(oc$Pervasives$[0], [ "String.index_from" ]);
           return __(index_rec$1108, [ s$1117, l$1120, i$1118, c$1119 ]);
         });
    var rindex_rec$1121 =
      _f(function (s$1122, i$1123, c$1124) {
           if (i$1123 < 0) throw $(Not_found$20g);
           if (oc$$srefu(s$1122, i$1123) === c$1124) return i$1123;
           return __(rindex_rec$1121, [ s$1122, i$1123 - 1, c$1124 ]);
         });
    var rindex$1125 = _f(function (s$1126, c$1127) { return __(rindex_rec$1121, [ s$1126, s$1126.length - 1, c$1127 ]); });
    var rindex_from$1128 =
      _f(function (s$1129, i$1130, c$1131) {
           if (i$1130 < -1 || i$1130 >= s$1129.length) return __(oc$Pervasives$[0], [ "String.rindex_from" ]);
           return __(rindex_rec$1121, [ s$1129, i$1130, c$1131 ]);
         });
    var contains_from$1132 =
      _f(function (s$1133, i$1134, c$1135) {
           var l$1136 = s$1133.length;
           if (i$1134 < 0 || i$1134 > l$1136) return __(oc$Pervasives$[0], [ "String.contains_from" ]);
           try {
             _(index_rec$1108, [ s$1133, l$1136, i$1134, c$1135 ]);
             return true;
           }
           catch (exn$1149) {
             if (exn$1149[0] === Not_found$20g) return false;
             throw exn$1149;
           }
         });
    var contains$1137 = _f(function (s$1138, c$1139) { return __(contains_from$1132, [ s$1138, 0, c$1139 ]); });
    var rcontains_from$1140 =
      _f(function (s$1141, i$1142, c$1143) {
           if (i$1142 < 0 || i$1142 >= s$1141.length) return __(oc$Pervasives$[0], [ "String.rcontains_from" ]);
           try {
             _(rindex_rec$1121, [ s$1141, i$1142, c$1143 ]);
             return true;
           }
           catch (exn$1148) {
             if (exn$1148[0] === Not_found$20g) return false;
             throw exn$1148;
           }
         });
    var compare$1145 = _f(function (prim$1147, prim$1146) { return caml_compare(prim$1147, prim$1146); });
    return $(make$1038, copy$1042, sub$1046, fill$1051, blit$1056, concat$1066, iter$1062, escaped$1080, index$1113, rindex$1125,
             index_from$1116, rindex_from$1128, contains$1137, contains_from$1132, rcontains_from$1140, uppercase$1096,
             lowercase$1098, capitalize$1104, uncapitalize$1106, compare$1145);
  }();
var oc$Sys$ =
  function () {
    var match$1090 = caml_sys_get_argv(0);
    var match$1089 = caml_sys_get_config(0);
    var word_size$1035 = match$1089[1];
    var max_array_length$1036 = (1 << word_size$1035 - 10) - 1;
    var max_string_length$1037 = (word_size$1035 / 8 >> 0) * max_array_length$1036 - 1;
    var interactive$1048 = $(false);
    var set_signal$1057 =
      _f(function (sig_num$1058, sig_beh$1059) { caml_install_signal_handler(sig_num$1058, sig_beh$1059); return 0; });
    var sigabrt$1060 = -1;
    var sigalrm$1061 = -2;
    var sigfpe$1062 = -3;
    var sighup$1063 = -4;
    var sigill$1064 = -5;
    var sigint$1065 = -6;
    var sigkill$1066 = -7;
    var sigpipe$1067 = -8;
    var sigquit$1068 = -9;
    var sigsegv$1069 = -10;
    var sigterm$1070 = -11;
    var sigusr1$1071 = -12;
    var sigusr2$1072 = -13;
    var sigchld$1073 = -14;
    var sigcont$1074 = -15;
    var sigstop$1075 = -16;
    var sigtstp$1076 = -17;
    var sigttin$1077 = -18;
    var sigttou$1078 = -19;
    var sigvtalrm$1079 = -20;
    var sigprof$1080 = -21;
    var Break$1081 = $("Sys.Break");
    var catch_break$1082 =
      _f(function (on$1083) {
           if (on$1083) return __(set_signal$1057, [ sigint$1065, $(_f(function (param$1088) { throw $(Break$1081); })) ]);
           return __(set_signal$1057, [ sigint$1065, 0 ]);
         });
    var ocaml_version$1084 = "3.12.0";
    return $(match$1090[1], match$1090[0], interactive$1048, match$1089[0], word_size$1035, max_string_length$1037,
             max_array_length$1036, set_signal$1057, sigabrt$1060, sigalrm$1061, sigfpe$1062, sighup$1063, sigill$1064,
             sigint$1065, sigkill$1066, sigpipe$1067, sigquit$1068, sigsegv$1069, sigterm$1070, sigusr1$1071, sigusr2$1072,
             sigchld$1073, sigcont$1074, sigstop$1075, sigtstp$1076, sigttin$1077, sigttou$1078, sigvtalrm$1079, sigprof$1080,
             Break$1081, catch_break$1082, ocaml_version$1084);
  }();
var oc$Hashtbl$ =
  function () {
    var hash$1031 = _f(function (x$1032) { return caml_hash_univ_param(10, 100, x$1032); });
    var create$1051 =
      _f(function (initial_size$1052) {
           var s$1053 = _(oc$Pervasives$[3], [ _(oc$Pervasives$[4], [ 1, initial_size$1052 ]), oc$Sys$[6] ]);
           return $(0, caml_make_vect(s$1053, 0));
         });
    var clear$1054 =
      _f(function (h$1055) {
           for (var i$1056 = 0; i$1056 <= (h$1055[1]).length - 1; i$1056++) {
             (function (i$1056) { oc$$asets(h$1055[1], i$1056, 0); }(i$1056));
           }
           return h$1055[0] = 0;
         });
    var copy$1057 = _f(function (h$1058) { return $(h$1058[0], _(oc$Array$[6], [ h$1058[1] ])); });
    var length$1059 = _f(function (h$1060) { return h$1060[0]; });
    var resize$1061 =
      _f(function (hashfun$1062, tbl$1063) {
           var odata$1064 = tbl$1063[1];
           var osize$1065 = odata$1064.length;
           var nsize$1066 = _(oc$Pervasives$[3], [ 2 * osize$1065 + 1, oc$Sys$[6] ]);
           if (nsize$1066 !== osize$1065) {
             {
               var ndata$1067 = caml_make_vect(nsize$1066, 0);
               var insert_bucket$1068 =
                 _f(function (param$1302) {
                      if (param$1302) {
                        {
                          var key$1069 = param$1302[0];
                          _(insert_bucket$1068, [ param$1302[2] ]);
                          var nidx$1072 = _(hashfun$1062, [ key$1069 ]) % nsize$1066;
                          return oc$$asets(ndata$1067, nidx$1072, $(key$1069, param$1302[1], oc$$arefs(ndata$1067, nidx$1072)));
                        }
                      }
                      return 0;
                    });
               for (var i$1073 = 0; i$1073 <= osize$1065 - 1; i$1073++) {
                 (function (i$1073) { _(insert_bucket$1068, [ oc$$arefs(odata$1064, i$1073) ]); }(i$1073));
               }
               return tbl$1063[1] = ndata$1067;
             }
           }
           return 0;
         });
    var add$1074 =
      _f(function (h$1075, key$1076, info$1077) {
           var i$1078 = _(hash$1031, [ key$1076 ]) % (h$1075[1]).length;
           var bucket$1079 = $(key$1076, info$1077, oc$$arefs(h$1075[1], i$1078));
           oc$$asets(h$1075[1], i$1078, bucket$1079);
           h$1075[0] = 1 + h$1075[0];
           if (h$1075[0] > (h$1075[1]).length << 1) return __(resize$1061, [ hash$1031, h$1075 ]);
           return 0;
         });
    var remove$1080 =
      _f(function (h$1081, key$1082) {
           var remove_bucket$1083 =
             _f(function (param$1301) {
                  if (param$1301) {
                    {
                      var next$1086 = param$1301[2];
                      var k$1084 = param$1301[0];
                      if (caml_compare(k$1084, key$1082) === 0) { { h$1081[0] = -1 + h$1081[0]; return next$1086; } }
                      return $(k$1084, param$1301[1], _(remove_bucket$1083, [ next$1086 ]));
                    }
                  }
                  return 0;
                });
           var i$1087 = _(hash$1031, [ key$1082 ]) % (h$1081[1]).length;
           return oc$$asets(h$1081[1], i$1087, _(remove_bucket$1083, [ oc$$arefs(h$1081[1], i$1087) ]));
         });
    var find_rec$1088 =
      _f(function (key$1089, param$1300) {
           if (!param$1300) throw $(Not_found$20g);
           if (caml_compare(key$1089, param$1300[0]) === 0) return param$1300[1];
           return __(find_rec$1088, [ key$1089, param$1300[2] ]);
         });
    var find$1093 =
      _f(function (h$1094, key$1095) {
           var match$1299 = oc$$arefs(h$1094[1], _(hash$1031, [ key$1095 ]) % (h$1094[1]).length);
           if (!match$1299) throw $(Not_found$20g);
           if (caml_compare(key$1095, match$1299[0]) === 0) return match$1299[1];
           var rest1$1098 = match$1299[2];
           if (!rest1$1098) throw $(Not_found$20g);
           if (caml_compare(key$1095, rest1$1098[0]) === 0) return rest1$1098[1];
           var rest2$1101 = rest1$1098[2];
           if (!rest2$1101) throw $(Not_found$20g);
           if (caml_compare(key$1095, rest2$1101[0]) === 0) return rest2$1101[1];
           return __(find_rec$1088, [ key$1095, rest2$1101[2] ]);
         });
    var find_all$1105 =
      _f(function (h$1106, key$1107) {
           var find_in_bucket$1108 =
             _f(function (param$1298) {
                  if (param$1298) {
                    {
                      var rest$1111 = param$1298[2];
                      if (caml_compare(param$1298[0], key$1107) === 0)
                        return $(param$1298[1], _(find_in_bucket$1108, [ rest$1111 ]));
                      return __(find_in_bucket$1108, [ rest$1111 ]);
                    }
                  }
                  return 0;
                });
           return __(find_in_bucket$1108, [ oc$$arefs(h$1106[1], _(hash$1031, [ key$1107 ]) % (h$1106[1]).length) ]);
         });
    var replace$1112 =
      _f(function (h$1113, key$1114, info$1115) {
           var replace_bucket$1116 =
             _f(function (param$1297) {
                  if (param$1297) {
                    {
                      var next$1119 = param$1297[2];
                      var k$1117 = param$1297[0];
                      if (caml_compare(k$1117, key$1114) === 0) return $(k$1117, info$1115, next$1119);
                      return $(k$1117, param$1297[1], _(replace_bucket$1116, [ next$1119 ]));
                    }
                  }
                  throw $(Not_found$20g);
                });
           var i$1120 = _(hash$1031, [ key$1114 ]) % (h$1113[1]).length;
           var l$1121 = oc$$arefs(h$1113[1], i$1120);
           try {
             return oc$$asets(h$1113[1], i$1120, _(replace_bucket$1116, [ l$1121 ]));
           }
           catch (exn$1296) {
             if (exn$1296[0] === Not_found$20g) {
               {
                 oc$$asets(h$1113[1], i$1120, $(key$1114, info$1115, l$1121));
                 h$1113[0] = 1 + h$1113[0];
                 if (h$1113[0] > (h$1113[1]).length << 1) return __(resize$1061, [ hash$1031, h$1113 ]);
                 return 0;
               }
             }
             throw exn$1296;
           }
         });
    var mem$1122 =
      _f(function (h$1123, key$1124) {
           var mem_in_bucket$1125 =
             _f(function (param$1295) {
                  if (param$1295) return caml_compare(param$1295[0], key$1124) === 0 || _(mem_in_bucket$1125, [ param$1295[2] ]);
                  return false;
                });
           return __(mem_in_bucket$1125, [ oc$$arefs(h$1123[1], _(hash$1031, [ key$1124 ]) % (h$1123[1]).length) ]);
         });
    var iter$1129 =
      _f(function (f$1130, h$1131) {
           var do_bucket$1132 =
             _f(function (param$1294) {
                  if (param$1294) {
                    { _(f$1130, [ param$1294[0], param$1294[1] ]); return __(do_bucket$1132, [ param$1294[2] ]); }
                  }
                  return 0;
                });
           var d$1136 = h$1131[1];
           for (var i$1137 = 0; i$1137 <= d$1136.length - 1; i$1137++) {
             (function (i$1137) { _(do_bucket$1132, [ oc$$arefs(d$1136, i$1137) ]); }(i$1137));
           }
         });
    var fold$1138 =
      _f(function (f$1139, h$1140, init$1141) {
           var do_bucket$1142 =
             _f(function (b$1143, accu$1144) {
                  if (b$1143) return __(do_bucket$1142, [ b$1143[2], _(f$1139, [ b$1143[0], b$1143[1], accu$1144 ]) ]);
                  return accu$1144;
                });
           var d$1148 = h$1140[1];
           var accu$1149 = init$1141;
           for (var i$1150 = 0; i$1150 <= d$1148.length - 1; i$1150++) {
             (function (i$1150) { accu$1149 = _(do_bucket$1142, [ oc$$arefs(d$1148, i$1150), accu$1149 ]); }(i$1150));
           }
           return accu$1149;
         });
    var Make$1251 =
      _f(function (H$1170) {
           var safehash$1177 = _f(function (key$1178) { return _(H$1170[1], [ key$1178 ]) & oc$Pervasives$[6]; });
           var add$1179 =
             _f(function (h$1180, key$1181, info$1182) {
                  var i$1183 = _(safehash$1177, [ key$1181 ]) % (h$1180[1]).length;
                  var bucket$1184 = $(key$1181, info$1182, oc$$arefs(h$1180[1], i$1183));
                  oc$$asets(h$1180[1], i$1183, bucket$1184);
                  h$1180[0] = 1 + h$1180[0];
                  if (h$1180[0] > (h$1180[1]).length << 1) return __(resize$1061, [ safehash$1177, h$1180 ]);
                  return 0;
                });
           var remove$1185 =
             _f(function (h$1186, key$1187) {
                  var remove_bucket$1188 =
                    _f(function (param$1293) {
                         if (param$1293) {
                           {
                             var next$1191 = param$1293[2];
                             var k$1189 = param$1293[0];
                             if (_(H$1170[0], [ k$1189, key$1187 ])) { { h$1186[0] = -1 + h$1186[0]; return next$1191; } }
                             return $(k$1189, param$1293[1], _(remove_bucket$1188, [ next$1191 ]));
                           }
                         }
                         return 0;
                       });
                  var i$1192 = _(safehash$1177, [ key$1187 ]) % (h$1186[1]).length;
                  return oc$$asets(h$1186[1], i$1192, _(remove_bucket$1188, [ oc$$arefs(h$1186[1], i$1192) ]));
                });
           var find_rec$1193 =
             _f(function (key$1194, param$1292) {
                  if (!param$1292) throw $(Not_found$20g);
                  if (_(H$1170[0], [ key$1194, param$1292[0] ])) return param$1292[1];
                  return __(find_rec$1193, [ key$1194, param$1292[2] ]);
                });
           var find$1198 =
             _f(function (h$1199, key$1200) {
                  var match$1291 = oc$$arefs(h$1199[1], _(safehash$1177, [ key$1200 ]) % (h$1199[1]).length);
                  if (match$1291) {
                    {
                      var rest1$1203 = match$1291[2];
                      if (_(H$1170[0], [ key$1200, match$1291[0] ])) return match$1291[1];
                      if (rest1$1203) {
                        {
                          var rest2$1206 = rest1$1203[2];
                          if (_(H$1170[0], [ key$1200, rest1$1203[0] ])) return rest1$1203[1];
                          if (!rest2$1206) throw $(Not_found$20g);
                          if (_(H$1170[0], [ key$1200, rest2$1206[0] ])) return rest2$1206[1];
                          return __(find_rec$1193, [ key$1200, rest2$1206[2] ]);
                        }
                      }
                      throw $(Not_found$20g);
                    }
                  }
                  throw $(Not_found$20g);
                });
           var find_all$1210 =
             _f(function (h$1211, key$1212) {
                  var find_in_bucket$1213 =
                    _f(function (param$1290) {
                         if (param$1290) {
                           {
                             var rest$1216 = param$1290[2];
                             if (_(H$1170[0], [ param$1290[0], key$1212 ]))
                               return $(param$1290[1], _(find_in_bucket$1213, [ rest$1216 ]));
                             return __(find_in_bucket$1213, [ rest$1216 ]);
                           }
                         }
                         return 0;
                       });
                  return __(find_in_bucket$1213, [ oc$$arefs(h$1211[1], _(safehash$1177, [ key$1212 ]) % (h$1211[1]).length) ]);
                });
           var replace$1217 =
             _f(function (h$1218, key$1219, info$1220) {
                  var replace_bucket$1221 =
                    _f(function (param$1289) {
                         if (param$1289) {
                           {
                             var next$1224 = param$1289[2];
                             var k$1222 = param$1289[0];
                             if (_(H$1170[0], [ k$1222, key$1219 ])) return $(k$1222, info$1220, next$1224);
                             return $(k$1222, param$1289[1], _(replace_bucket$1221, [ next$1224 ]));
                           }
                         }
                         throw $(Not_found$20g);
                       });
                  var i$1225 = _(safehash$1177, [ key$1219 ]) % (h$1218[1]).length;
                  var l$1226 = oc$$arefs(h$1218[1], i$1225);
                  try {
                    return oc$$asets(h$1218[1], i$1225, _(replace_bucket$1221, [ l$1226 ]));
                  }
                  catch (exn$1288) {
                    if (exn$1288[0] === Not_found$20g) {
                      {
                        oc$$asets(h$1218[1], i$1225, $(key$1219, info$1220, l$1226));
                        h$1218[0] = 1 + h$1218[0];
                        if (h$1218[0] > (h$1218[1]).length << 1) return __(resize$1061, [ safehash$1177, h$1218 ]);
                        return 0;
                      }
                    }
                    throw exn$1288;
                  }
                });
           var mem$1227 =
             _f(function (h$1228, key$1229) {
                  var mem_in_bucket$1230 =
                    _f(function (param$1287) {
                         if (param$1287)
                           return _(H$1170[0], [ param$1287[0], key$1229 ]) || _(mem_in_bucket$1230, [ param$1287[2] ]);
                         return false;
                       });
                  return __(mem_in_bucket$1230, [ oc$$arefs(h$1228[1], _(safehash$1177, [ key$1229 ]) % (h$1228[1]).length) ]);
                });
           return $(create$1051, clear$1054, copy$1057, add$1179, remove$1185, find$1198, find_all$1210, replace$1217, mem$1227,
                    iter$1129, fold$1138, length$1059);
         });
    return $(create$1051, clear$1054, add$1074, copy$1057, find$1093, find_all$1105, mem$1122, remove$1080, replace$1112,
             iter$1129, fold$1138, length$1059, Make$1251, hash$1031);
  }();
var oc$Int64$ =
  function () {
    var zero$1050 = "0";
    var one$1051 = "1";
    var minus_one$1052 = "-1";
    var succ$1053 = _f(function (n$1054) { return n$1054 + "1"; });
    var pred$1055 = _f(function (n$1056) { return n$1056 - "1"; });
    var abs$1057 = _f(function (n$1058) { if (n$1058 >= "0") return n$1058; return -n$1058; });
    var min_int$1059 = "-9223372036854775808";
    var max_int$1060 = "9223372036854775807";
    var lognot$1061 = _f(function (n$1062) { return n$1062 ^ "-1"; });
    var to_string$1064 = _f(function (n$1065) { return caml_format_int("%d", n$1065); });
    var compare$1070 = _f(function (x$1071, y$1072) { return caml_int64_compare(x$1071, y$1072); });
    return $(zero$1050, one$1051, minus_one$1052, succ$1053, pred$1055, abs$1057, max_int$1060, min_int$1059, lognot$1061,
             to_string$1064, compare$1070);
  }();
var oc$Buffer$ =
  function () {
    var create$1054 = _f(function (n$1055) { return $(new Array(), 0, 0); });
    var contents$1056 =
      _f(function (b$1057) {
           var match$1149 = b$1057[1];
           if (match$1149) return match$1149[0];
           var s$1059 = (b$1057[0]).join('');
           b$1057[1] = $(s$1059);
           return s$1059;
         });
    var sub$1060 =
      _f(function (b$1061, ofs$1062, len$1063) {
           if (ofs$1062 < 0 || (len$1063 < 0 || ofs$1062 > b$1061[2] - len$1063)) _(oc$Pervasives$[0], [ "Buffer.sub" ]); else;
           var s$1064 = _(contents$1056, [ b$1061 ]);
           return s$1064.substring(ofs$1062, ofs$1062 + len$1063);
         });
    var blit$1065 =
      _f(function (src$1066, srcoff$1067, dst$1068, dstoff$1069, len$1070) { return __(oc$Pervasives$[1], [ "unimplemented" ]); });
    var nth$1071 =
      _f(function (b$1072, ofs$1073) {
           if (ofs$1073 < 0 || ofs$1073 >= b$1072[2]) _(oc$Pervasives$[0], [ "Buffer.nth" ]); else;
           var s$1074 = _(contents$1056, [ b$1072 ]);
           return s$1074.charCodeAt(ofs$1073);
         });
    var length$1075 = _f(function (b$1076) { return b$1076[2]; });
    var clear$1077 = _f(function (b$1078) { b$1078[2] = 0; (b$1078[0]).length = 0; });
    var add_char$1080 =
      _f(function (b$1081, c$1082) { b$1081[1] = 0; b$1081[2] = b$1081[2] + 1; (b$1081[0]).push(String.fromCharCode(c$1082)); });
    var add_substring$1083 =
      _f(function (b$1084, s$1085, offset$1086, len$1087) {
           if (offset$1086 < 0 || (len$1087 < 0 || offset$1086 > s$1085.length - len$1087))
             _(oc$Pervasives$[0], [ "Buffer.add_substring" ]);
           else;
           b$1084[1] = 0;
           b$1084[2] = b$1084[2] + len$1087;
           (b$1084[0]).push(s$1085.substring(offset$1086, offset$1086 + len$1087));
         });
    var add_string$1088 =
      _f(function (b$1089, s$1090) { b$1089[1] = 0; b$1089[2] = b$1089[2] + s$1090.length; (b$1089[0]).push(s$1090); });
    var add_buffer$1091 = _f(function (b$1092, bs$1093) { return __(add_string$1088, [ b$1092, _(contents$1056, [ bs$1093 ]) ]); });
    var add_channel$1094 = _f(function (b$1095, ic$1096, len$1097) { return __(oc$Pervasives$[1], [ "unsupported" ]); });
    var output_buffer$1098 = _f(function (oc$1099, b$1100) { return __(oc$Pervasives$[1], [ "unsupported" ]); });
    var closing$1101 =
      _f(function (param$1148) {
           if (!(param$1148 !== 40)) return 41;
           if (param$1148 !== 123) throw $(Assert_failure$26g, $("buffer.ml", 120, 9));
           return 125;
         });
    var advance_to_closing$1102 =
      _f(function (opening$1103, closing$1104, k$1105, s$1106, start$1107) {
           var advance$1108 =
             _f(function (k$1109, i$1110, lim$1111) {
                  if (i$1110 >= lim$1111) throw $(Not_found$20g);
                  if (oc$$srefs(s$1106, i$1110) === opening$1103) return __(advance$1108, [ k$1109 + 1, i$1110 + 1, lim$1111 ]);
                  if (!(oc$$srefs(s$1106, i$1110) === closing$1104)) return __(advance$1108, [ k$1109, i$1110 + 1, lim$1111 ]);
                  if (k$1109 === 0) return i$1110;
                  return __(advance$1108, [ k$1109 - 1, i$1110 + 1, lim$1111 ]);
                });
           return __(advance$1108, [ k$1105, start$1107, s$1106.length ]);
         });
    var advance_to_non_alpha$1112 =
      _f(function (s$1113, start$1114) {
           var advance$1115 =
             _f(function (i$1116, lim$1117) {
                  if (i$1116 >= lim$1117) return lim$1117;
                  var match$1145 = oc$$srefs(s$1113, i$1116);
                  var $r15 = false;
                  r$15: {
                    {
                      if (!(match$1145 < 95)) {
                        {
                          if (!(match$1145 >= 123)) { { if (match$1145 !== 96) { { $r15 = true; break r$15; } } return i$1116; } }
                          if (match$1145 >= 192) {
                            {
                              var s$1151 = -192 + match$1145;
                              switch (s$1151)
                              {
                              case 0: $r15 = true; break r$15;
                              case 1: $r15 = true; break r$15;
                              case 2: $r15 = true; break r$15;
                              case 3: return i$1116;
                              case 4: return i$1116;
                              case 5: return i$1116;
                              case 6: return i$1116;
                              case 7: $r15 = true; break r$15;
                              case 8: $r15 = true; break r$15;
                              case 9: $r15 = true; break r$15;
                              case 10: $r15 = true; break r$15;
                              case 11: $r15 = true; break r$15;
                              case 12: return i$1116;
                              case 13: return i$1116;
                              case 14: $r15 = true; break r$15;
                              case 15: $r15 = true; break r$15;
                              case 16: return i$1116;
                              case 17: return i$1116;
                              case 18: return i$1116;
                              case 19: return i$1116;
                              case 20: $r15 = true; break r$15;
                              case 21: return i$1116;
                              case 22: return i$1116;
                              case 23: return i$1116;
                              case 24: return i$1116;
                              case 25: $r15 = true; break r$15;
                              case 26: return i$1116;
                              case 27: $r15 = true; break r$15;
                              case 28: $r15 = true; break r$15;
                              case 29: return i$1116;
                              case 30: return i$1116;
                              case 31: return i$1116;
                              case 32: $r15 = true; break r$15;
                              case 33: $r15 = true; break r$15;
                              case 34: $r15 = true; break r$15;
                              case 35: return i$1116;
                              case 36: return i$1116;
                              case 37: return i$1116;
                              case 38: return i$1116;
                              case 39: $r15 = true; break r$15;
                              case 40: $r15 = true; break r$15;
                              case 41: $r15 = true; break r$15;
                              case 42: $r15 = true; break r$15;
                              case 43: $r15 = true; break r$15;
                              case 44: return i$1116;
                              case 45: return i$1116;
                              case 46: $r15 = true; break r$15;
                              case 47: $r15 = true; break r$15;
                              case 48: return i$1116;
                              case 49: return i$1116;
                              case 50: return i$1116;
                              case 51: return i$1116;
                              case 52: $r15 = true; break r$15;
                              case 53: return i$1116;
                              case 54: return i$1116;
                              case 55: return i$1116;
                              case 56: return i$1116;
                              case 57: $r15 = true; break r$15;
                              case 58: return i$1116;
                              case 59: $r15 = true; break r$15;
                              case 60: $r15 = true; break r$15;
                              case 61: return i$1116;
                              case 62: return i$1116;
                              case 63: return i$1116;
                              default: return null;
                              }
                            }
                          }
                          return i$1116;
                        }
                      }
                      if (!(match$1145 >= 58)) { { if (match$1145 >= 48) { { $r15 = true; break r$15; } } return i$1116; } }
                      if (!(-65 + match$1145 < 0 || -65 + match$1145 > 25)) { { $r15 = true; break r$15; } }
                      return i$1116;
                    }
                  }
                  if ($r15) return __(advance$1115, [ i$1116 + 1, lim$1117 ]);
                });
           return __(advance$1115, [ start$1114, s$1113.length ]);
         });
    var find_ident$1118 =
      _f(function (s$1119, start$1120, lim$1121) {
           if (start$1120 >= lim$1121) throw $(Not_found$20g);
           var c$1122 = oc$$srefs(s$1119, start$1120);
           var $r12 = false;
           r$12: {
             {
               if (!(c$1122 !== 40)) { { $r12 = true; break r$12; } }
               if (!(c$1122 !== 123)) { { $r12 = true; break r$12; } }
               var stop$1125 = _(advance_to_non_alpha$1112, [ s$1119, start$1120 + 1 ]);
               return $(_(oc$String$[2], [ s$1119, start$1120, stop$1125 - start$1120 ]), stop$1125);
             }
           }
           if ($r12) {
             {
               var new_start$1123 = start$1120 + 1;
               var stop$1124 = _(advance_to_closing$1102, [ c$1122, _(closing$1101, [ c$1122 ]), 0, s$1119, new_start$1123 ]);
               return $(_(oc$String$[2], [ s$1119, new_start$1123, stop$1124 - start$1120 - 1 ]), stop$1124 + 1);
             }
           }
         });
    var add_substitute$1126 =
      _f(function (b$1127, f$1128, s$1129) {
           var lim$1130 = s$1129.length;
           var subst$1131 =
             _f(function (previous$1132, i$1133) {
                  if (i$1133 < lim$1130) {
                    {
                      var current$1134 = oc$$srefs(s$1129, i$1133);
                      if (!(current$1134 !== 36)) {
                        {
                          if (previous$1132 === 92) {
                            { _(add_char$1080, [ b$1127, current$1134 ]); return __(subst$1131, [ 32, i$1133 + 1 ]); }
                          }
                          var j$1138 = i$1133 + 1;
                          var match$1144 = _(find_ident$1118, [ s$1129, j$1138, lim$1130 ]);
                          _(add_string$1088, [ b$1127, _(f$1128, [ match$1144[0] ]) ]);
                          return __(subst$1131, [ 32, match$1144[1] ]);
                        }
                      }
                      if (previous$1132 === 92) {
                        {
                          _(add_char$1080, [ b$1127, 92 ]);
                          _(add_char$1080, [ b$1127, current$1134 ]);
                          return __(subst$1131, [ 32, i$1133 + 1 ]);
                        }
                      }
                      if (current$1134 !== 92) {
                        { _(add_char$1080, [ b$1127, current$1134 ]); return __(subst$1131, [ current$1134, i$1133 + 1 ]); }
                      }
                      return __(subst$1131, [ current$1134, i$1133 + 1 ]);
                    }
                  }
                  if (previous$1132 === 92) return __(add_char$1080, [ b$1127, previous$1132 ]);
                  return 0;
                });
           return __(subst$1131, [ 32, 0 ]);
         });
    return $(create$1054, contents$1056, sub$1060, blit$1065, nth$1071, length$1075, clear$1077, clear$1077, add_char$1080,
             add_string$1088, add_substring$1083, add_substitute$1126, add_buffer$1091, add_channel$1094, output_buffer$1098);
  }();
var oc$Printf$ =
  function () {
    var Sformat$1056 =
      function () {
        var index_of_int$1037 =
          _f(function (i$1038) {
               if (i$1038 >= 0) return i$1038;
               return __(oc$Pervasives$[1],
                         [
                           _(oc$Pervasives$[15], [ "Sformat.index_of_int: negative argument ", _(oc$Pervasives$[19], [ i$1038 ]) ])
                         ]);
             });
        var add_int_index$1040 = _f(function (i$1041, idx$1042) { return __(index_of_int$1037, [ i$1041 + idx$1042 ]); });
        var succ_index$1043 = _(add_int_index$1040, [ 1 ]);
        var index_of_literal_position$1044 = _f(function (p$1045) { return __(index_of_int$1037, [ -1 + p$1045 ]); });
        var sub$1050 = _f(function (fmt$1051, idx$1052, len$1053) { return __(oc$String$[2], [ fmt$1051, idx$1052, len$1053 ]); });
        var to_string$1054 = _f(function (fmt$1055) { return __(sub$1050, [ fmt$1055, 0, fmt$1055.length ]); });
        return $(index_of_int$1037, add_int_index$1040, succ_index$1043, index_of_literal_position$1044, sub$1050, to_string$1054);
      }();
    var bad_conversion$1057 =
      _f(function (sfmt$1058, i$1059, c$1060) {
           return __(oc$Pervasives$[0],
                     [
                       _(oc$Pervasives$[15],
                         [
                           "Printf: bad conversion %",
                           _(oc$Pervasives$[15],
                             [
                               _(oc$String$[0], [ 1, c$1060 ]),
                               _(oc$Pervasives$[15],
                                 [
                                   ", at char number ",
                                   _(oc$Pervasives$[15],
                                     [
                                       _(oc$Pervasives$[19], [ i$1059 ]),
                                       _(oc$Pervasives$[15],
                                         [ " in format string ``", _(oc$Pervasives$[15], [ sfmt$1058, "\'\'" ]) ])
                                     ])
                                 ])
                             ])
                         ])
                     ]);
         });
    var bad_conversion_format$1061 =
      _f(function (fmt$1062, i$1063, c$1064) {
           return __(bad_conversion$1057, [ _(Sformat$1056[5], [ fmt$1062 ]), i$1063, c$1064 ]);
         });
    var incomplete_format$1065 =
      _f(function (fmt$1066) {
           return __(oc$Pervasives$[0],
                     [
                       _(oc$Pervasives$[15],
                         [
                           "Printf: premature end of format string ``",
                           _(oc$Pervasives$[15], [ _(Sformat$1056[5], [ fmt$1066 ]), "\'\'" ])
                         ])
                     ]);
         });
    var parse_string_conversion$1067 =
      _f(function (sfmt$1068) {
           var parse$1069 =
             _f(function (neg$1070, i$1071) {
                  if (i$1071 >= sfmt$1068.length) return $(0, neg$1070);
                  var match$1474 = oc$$srefu(sfmt$1068, i$1071);
                  var $r162 = false;
                  r$162: {
                    {
                      if (!(match$1474 >= 49)) {
                        { if (match$1474 !== 45) { { $r162 = true; break r$162; } } return __(parse$1069, [ true, 1 + i$1071 ]); }
                      }
                      if (match$1474 >= 58) { { $r162 = true; break r$162; } }
                      return $(caml_int_of_string(_(oc$String$[2], [ sfmt$1068, i$1071, sfmt$1068.length - i$1071 - 1 ])),
                               neg$1070);
                    }
                  }
                  if ($r162) return __(parse$1069, [ neg$1070, 1 + i$1071 ]);
                });
           try {
             return _(parse$1069, [ false, 1 ]);
           }
           catch (exn$1472) {
             if (exn$1472[0] === Failure$19g) return __(bad_conversion$1057, [ sfmt$1068, 0, 115 ]);
             throw exn$1472;
           }
         });
    var pad_string$1072 =
      _f(function (pad_char$1073, p$1074, neg$1075, s$1076, i$1077, len$1078) {
           if (p$1074 === len$1078 && i$1077 === 0) return s$1076;
           if (p$1074 <= len$1078) return __(oc$String$[2], [ s$1076, i$1077, len$1078 ]);
           var res$1079 = _(oc$String$[0], [ p$1074, pad_char$1073 ]);
           if (neg$1075)
             _(oc$String$[4], [ s$1076, i$1077, res$1079, 0, len$1078 ]);
           else
             _(oc$String$[4], [ s$1076, i$1077, res$1079, p$1074 - len$1078, len$1078 ]);
           return res$1079;
         });
    var format_string$1080 =
      _f(function (sfmt$1081, s$1082) {
           var match$1471 = _(parse_string_conversion$1067, [ sfmt$1081 ]);
           return __(pad_string$1072, [ 32, match$1471[0], match$1471[1], s$1082, 0, s$1082.length ]);
         });
    var extract_format$1085 =
      _f(function (fmt$1086, start$1087, stop$1088, widths$1089) {
           var skip_positional_spec$1090 =
             _f(function (start$1091) {
                  var match$1469 = oc$$srefu(fmt$1086, start$1091);
                  if (-48 + match$1469 < 0 || -48 + match$1469 > 9) return start$1091;
                  var skip_int_literal$1092 =
                    _f(function (i$1093) {
                         var match$1468 = oc$$srefu(fmt$1086, i$1093);
                         if (!(match$1468 >= 48)) { { if (match$1468 !== 36) return start$1091; return 1 + i$1093; } }
                         if (match$1468 >= 58) return start$1091;
                         return __(skip_int_literal$1092, [ 1 + i$1093 ]);
                       });
                  return __(skip_int_literal$1092, [ 1 + start$1091 ]);
                });
           var start$1094 = _(skip_positional_spec$1090, [ 1 + start$1087 ]);
           var b$1095 = _(oc$Buffer$[0], [ stop$1088 - start$1094 + 10 ]);
           _(oc$Buffer$[8], [ b$1095, 37 ]);
           var fill_format$1096 =
             _f(function (i$1097, widths$1098) {
                  if (i$1097 <= stop$1088) {
                    {
                      var match$1465 = oc$$srefu(fmt$1086, i$1097);
                      if (match$1465 !== 42) {
                        { _(oc$Buffer$[8], [ b$1095, match$1465 ]); return __(fill_format$1096, [ 1 + i$1097, widths$1098 ]); }
                      }
                      if (widths$1098) {
                        {
                          _(oc$Buffer$[9], [ b$1095, _(oc$Pervasives$[19], [ widths$1098[0] ]) ]);
                          var i$1102 = _(skip_positional_spec$1090, [ 1 + i$1097 ]);
                          return __(fill_format$1096, [ i$1102, widths$1098[1] ]);
                        }
                      }
                      throw $(Assert_failure$26g, $("printf.ml", 164, 8));
                    }
                  }
                  return 0;
                });
           _(fill_format$1096, [ start$1094, _(oc$List$[4], [ widths$1089 ]) ]);
           return __(oc$Buffer$[1], [ b$1095 ]);
         });
    var extract_format_int$1103 =
      _f(function (conv$1104, fmt$1105, start$1106, stop$1107, widths$1108) {
           var sfmt$1109 = _(extract_format$1085, [ fmt$1105, start$1106, stop$1107, widths$1108 ]);
           var $r142 = false;
           r$142: {
             {
               if (!(conv$1104 !== 78)) { { $r142 = true; break r$142; } }
               if (!(conv$1104 !== 110)) { { $r142 = true; break r$142; } }
               return sfmt$1109;
             }
           }
           if ($r142) { { oc$$ssets(sfmt$1109, sfmt$1109.length - 1, 117); return sfmt$1109; } }
         });
    var extract_format_float$1110 =
      _f(function (conv$1111, fmt$1112, start$1113, stop$1114, widths$1115) {
           var sfmt$1116 = _(extract_format$1085, [ fmt$1112, start$1113, stop$1114, widths$1115 ]);
           if (conv$1111 !== 70) return sfmt$1116;
           oc$$ssets(sfmt$1116, sfmt$1116.length - 1, 103);
           return sfmt$1116;
         });
    var sub_format$1117 =
      _f(function (incomplete_format$1118, bad_conversion_format$1119, conv$1120, fmt$1121, i$1122) {
           var len$1123 = fmt$1121.length;
           var sub_fmt$1124 =
             _f(function (c$1125, i$1126) {
                  var close$1127 = c$1125 === 40 ? 41 : 125;
                  var sub$1128 =
                    _f(function (j$1130) {
                         if (j$1130 >= len$1123) return __(incomplete_format$1118, [ fmt$1121 ]);
                         var match$1462 = oc$$srefs(fmt$1121, j$1130);
                         if (match$1462 !== 37) return __(sub$1128, [ 1 + j$1130 ]);
                         return __(sub_sub$1129, [ 1 + j$1130 ]);
                       });
                  var sub_sub$1129 =
                    _f(function (j$1131) {
                         if (j$1131 >= len$1123) return __(incomplete_format$1118, [ fmt$1121 ]);
                         var c$1132 = oc$$srefs(fmt$1121, j$1131);
                         var $r134 = false;
                         r$134: {
                           {
                             var $r133 = false;
                             r$133: {
                               {
                                 var $r135 = false;
                                 r$135: {
                                   {
                                     var switcher$1463 = -40 + c$1132;
                                     if (switcher$1463 < 0 || switcher$1463 > 1) {
                                       {
                                         var switcher$1464 = -83 + switcher$1463;
                                         if (switcher$1464 < 0 || switcher$1464 > 2) { { $r135 = true; break r$135; } }
                                         switch (switcher$1464)
                                         {
                                         case 0: $r133 = true; break r$133;
                                         case 1: $r135 = true; break r$135;
                                         case 2: $r134 = true; break r$134;
                                         default: return null;
                                         }
                                       }
                                     }
                                     if (!!!switcher$1463) { { $r133 = true; break r$133; } }
                                     $r134 = true;
                                     break r$134;
                                   }
                                 }
                                 if ($r135) return __(sub$1128, [ 1 + j$1131 ]);
                               }
                             }
                             if ($r133) {
                               { var j$1134 = _(sub_fmt$1124, [ c$1132, 1 + j$1131 ]); return __(sub$1128, [ 1 + j$1134 ]); }
                             }
                           }
                         }
                         if ($r134) {
                           {
                             if (c$1132 === close$1127) return 1 + j$1131;
                             return __(bad_conversion_format$1119, [ fmt$1121, i$1126, c$1132 ]);
                           }
                         }
                       });
                  return __(sub$1128, [ i$1126 ]);
                });
           return __(sub_fmt$1124, [ conv$1120, i$1122 ]);
         });
    var sub_format_for_printf$1135 =
      _f(function (conv$1136) { return __(sub_format$1117, [ incomplete_format$1065, bad_conversion_format$1061, conv$1136 ]); });
    var iter_on_format_args$1137 =
      _f(function (fmt$1138, add_conv$1139, add_char$1140) {
           var lim$1141 = fmt$1138.length - 1;
           var scan_flags$1142 =
             _f(function (skip$1145, i$1146) {
                  if (i$1146 > lim$1141) return __(incomplete_format$1065, [ fmt$1138 ]);
                  var match$1459 = oc$$srefu(fmt$1138, i$1146);
                  var $r111 = false;
                  r$111: {
                    {
                      var $r110 = false;
                      r$110: {
                        {
                          var $r112 = false;
                          r$112: {
                            {
                              if (!(match$1459 >= 58)) {
                                {
                                  if (!(match$1459 >= 32)) { { $r112 = true; break r$112; } }
                                  var s$1475 = -32 + match$1459;
                                  switch (s$1475)
                                  {
                                  case 0: $r110 = true; break r$110;
                                  case 1: $r112 = true; break r$112;
                                  case 2: $r112 = true; break r$112;
                                  case 3: $r110 = true; break r$110;
                                  case 4: $r112 = true; break r$112;
                                  case 5: $r112 = true; break r$112;
                                  case 6: $r112 = true; break r$112;
                                  case 7: $r112 = true; break r$112;
                                  case 8: $r112 = true; break r$112;
                                  case 9: $r112 = true; break r$112;
                                  case 10: return __(scan_flags$1142, [ skip$1145, _(add_conv$1139, [ skip$1145, i$1146, 105 ]) ]);
                                  case 11: $r110 = true; break r$110;
                                  case 12: $r112 = true; break r$112;
                                  case 13: $r110 = true; break r$110;
                                  case 14: $r111 = true; break r$111;
                                  case 15: $r112 = true; break r$112;
                                  case 16: $r111 = true; break r$111;
                                  case 17: $r111 = true; break r$111;
                                  case 18: $r111 = true; break r$111;
                                  case 19: $r111 = true; break r$111;
                                  case 20: $r111 = true; break r$111;
                                  case 21: $r111 = true; break r$111;
                                  case 22: $r111 = true; break r$111;
                                  case 23: $r111 = true; break r$111;
                                  case 24: $r111 = true; break r$111;
                                  case 25: $r111 = true; break r$111;
                                  default: return null;
                                  }
                                }
                              }
                              if (match$1459 !== 95) { { $r112 = true; break r$112; } }
                              return __(scan_flags$1142, [ true, 1 + i$1146 ]);
                            }
                          }
                          if ($r112) return __(scan_conv$1143, [ skip$1145, i$1146 ]);
                        }
                      }
                      if ($r110) return __(scan_flags$1142, [ skip$1145, 1 + i$1146 ]);
                    }
                  }
                  if ($r111) return __(scan_flags$1142, [ skip$1145, 1 + i$1146 ]);
                });
           var scan_conv$1143 =
             _f(function (skip$1147, i$1148) {
                  if (i$1148 > lim$1141) return __(incomplete_format$1065, [ fmt$1138 ]);
                  var conv$1149 = oc$$srefu(fmt$1138, i$1148);
                  var $r126 = false;
                  r$126: {
                    {
                      var $r125 = false;
                      r$125: {
                        {
                          var $r124 = false;
                          r$124: {
                            {
                              var $r123 = false;
                              r$123: {
                                {
                                  var $r122 = false;
                                  r$122: {
                                    {
                                      var $r121 = false;
                                      r$121: {
                                        {
                                          var $r120 = false;
                                          r$120: {
                                            {
                                              var $r119 = false;
                                              r$119: {
                                                {
                                                  var $r118 = false;
                                                  r$118: {
                                                    {
                                                      var $r127 = false;
                                                      r$127: {
                                                        {
                                                          if (conv$1149 >= 126) { { $r127 = true; break r$127; } }
                                                          switch (conv$1149)
                                                          {
                                                          case 0: $r127 = true; break r$127;
                                                          case 1: $r127 = true; break r$127;
                                                          case 2: $r127 = true; break r$127;
                                                          case 3: $r127 = true; break r$127;
                                                          case 4: $r127 = true; break r$127;
                                                          case 5: $r127 = true; break r$127;
                                                          case 6: $r127 = true; break r$127;
                                                          case 7: $r127 = true; break r$127;
                                                          case 8: $r127 = true; break r$127;
                                                          case 9: $r127 = true; break r$127;
                                                          case 10: $r127 = true; break r$127;
                                                          case 11: $r127 = true; break r$127;
                                                          case 12: $r127 = true; break r$127;
                                                          case 13: $r127 = true; break r$127;
                                                          case 14: $r127 = true; break r$127;
                                                          case 15: $r127 = true; break r$127;
                                                          case 16: $r127 = true; break r$127;
                                                          case 17: $r127 = true; break r$127;
                                                          case 18: $r127 = true; break r$127;
                                                          case 19: $r127 = true; break r$127;
                                                          case 20: $r127 = true; break r$127;
                                                          case 21: $r127 = true; break r$127;
                                                          case 22: $r127 = true; break r$127;
                                                          case 23: $r127 = true; break r$127;
                                                          case 24: $r127 = true; break r$127;
                                                          case 25: $r127 = true; break r$127;
                                                          case 26: $r127 = true; break r$127;
                                                          case 27: $r127 = true; break r$127;
                                                          case 28: $r127 = true; break r$127;
                                                          case 29: $r127 = true; break r$127;
                                                          case 30: $r127 = true; break r$127;
                                                          case 31: $r127 = true; break r$127;
                                                          case 32: $r127 = true; break r$127;
                                                          case 33: $r118 = true; break r$118;
                                                          case 34: $r127 = true; break r$127;
                                                          case 35: $r127 = true; break r$127;
                                                          case 36: $r127 = true; break r$127;
                                                          case 37: $r118 = true; break r$118;
                                                          case 38: $r127 = true; break r$127;
                                                          case 39: $r127 = true; break r$127;
                                                          case 40:
                                                            return __
                                                                   (scan_fmt$1144,
                                                                    [ _(add_conv$1139, [ skip$1147, i$1148, conv$1149 ]) ]);
                                                          case 41: $r126 = true; break r$126;
                                                          case 42: $r127 = true; break r$127;
                                                          case 43: $r127 = true; break r$127;
                                                          case 44: $r118 = true; break r$118;
                                                          case 45: $r127 = true; break r$127;
                                                          case 46: $r127 = true; break r$127;
                                                          case 47: $r127 = true; break r$127;
                                                          case 48: $r127 = true; break r$127;
                                                          case 49: $r127 = true; break r$127;
                                                          case 50: $r127 = true; break r$127;
                                                          case 51: $r127 = true; break r$127;
                                                          case 52: $r127 = true; break r$127;
                                                          case 53: $r127 = true; break r$127;
                                                          case 54: $r127 = true; break r$127;
                                                          case 55: $r127 = true; break r$127;
                                                          case 56: $r127 = true; break r$127;
                                                          case 57: $r127 = true; break r$127;
                                                          case 58: $r127 = true; break r$127;
                                                          case 59: $r127 = true; break r$127;
                                                          case 60: $r127 = true; break r$127;
                                                          case 61: $r127 = true; break r$127;
                                                          case 62: $r127 = true; break r$127;
                                                          case 63: $r127 = true; break r$127;
                                                          case 64: $r127 = true; break r$127;
                                                          case 65: $r127 = true; break r$127;
                                                          case 66: $r123 = true; break r$123;
                                                          case 67: $r120 = true; break r$120;
                                                          case 68: $r127 = true; break r$127;
                                                          case 69: $r122 = true; break r$122;
                                                          case 70: $r122 = true; break r$122;
                                                          case 71: $r122 = true; break r$122;
                                                          case 72: $r127 = true; break r$127;
                                                          case 73: $r127 = true; break r$127;
                                                          case 74: $r127 = true; break r$127;
                                                          case 75: $r127 = true; break r$127;
                                                          case 76: $r125 = true; break r$125;
                                                          case 77: $r127 = true; break r$127;
                                                          case 78: $r121 = true; break r$121;
                                                          case 79: $r127 = true; break r$127;
                                                          case 80: $r127 = true; break r$127;
                                                          case 81: $r127 = true; break r$127;
                                                          case 82: $r127 = true; break r$127;
                                                          case 83: $r119 = true; break r$119;
                                                          case 84: $r127 = true; break r$127;
                                                          case 85: $r127 = true; break r$127;
                                                          case 86: $r127 = true; break r$127;
                                                          case 87: $r127 = true; break r$127;
                                                          case 88: $r121 = true; break r$121;
                                                          case 89: $r127 = true; break r$127;
                                                          case 90: $r127 = true; break r$127;
                                                          case 91: $r119 = true; break r$119;
                                                          case 92: $r127 = true; break r$127;
                                                          case 93: $r127 = true; break r$127;
                                                          case 94: $r127 = true; break r$127;
                                                          case 95: $r127 = true; break r$127;
                                                          case 96: $r127 = true; break r$127;
                                                          case 97: $r124 = true; break r$124;
                                                          case 98: $r123 = true; break r$123;
                                                          case 99: $r120 = true; break r$120;
                                                          case 100: $r121 = true; break r$121;
                                                          case 101: $r122 = true; break r$122;
                                                          case 102: $r122 = true; break r$122;
                                                          case 103: $r122 = true; break r$122;
                                                          case 104: $r127 = true; break r$127;
                                                          case 105: $r121 = true; break r$121;
                                                          case 106: $r127 = true; break r$127;
                                                          case 107: $r127 = true; break r$127;
                                                          case 108: $r125 = true; break r$125;
                                                          case 109: $r127 = true; break r$127;
                                                          case 110: $r125 = true; break r$125;
                                                          case 111: $r121 = true; break r$121;
                                                          case 112: $r127 = true; break r$127;
                                                          case 113: $r127 = true; break r$127;
                                                          case 114: $r124 = true; break r$124;
                                                          case 115: $r119 = true; break r$119;
                                                          case 116: $r124 = true; break r$124;
                                                          case 117: $r121 = true; break r$121;
                                                          case 118: $r127 = true; break r$127;
                                                          case 119: $r127 = true; break r$127;
                                                          case 120: $r121 = true; break r$121;
                                                          case 121: $r127 = true; break r$127;
                                                          case 122: $r127 = true; break r$127;
                                                          case 123:
                                                            var i$1157 = _(add_conv$1139, [ skip$1147, i$1148, conv$1149 ]);
                                                            var j$1158 =
                                                              _(sub_format_for_printf$1135, [ conv$1149, fmt$1138, i$1157 ]);
                                                            var loop$1159 =
                                                              _f(function 
                                                                 (i$1160) {
                                                                   if (
                                                                   i$1160 < j$1158 - 2)
                                                                    return __
                                                                    (loop$1159,
                                                                    [ _(add_char$1140, [ i$1160, oc$$srefs(fmt$1138, i$1160) ]) ]);
                                                                   return 0;
                                                                 });
                                                            _(loop$1159, [ i$1157 ]);
                                                            return __(scan_conv$1143, [ skip$1147, j$1158 - 1 ]);
                                                          case 124: $r127 = true; break r$127;
                                                          case 125: $r126 = true; break r$126;
                                                          default: return null;
                                                          }
                                                        }
                                                      }
                                                      if ($r127)
                                                        return __(bad_conversion_format$1061, [ fmt$1138, i$1148, conv$1149 ]);
                                                    }
                                                  }
                                                  if ($r118) return 1 + i$1148;
                                                }
                                              }
                                              if ($r119) return __(add_conv$1139, [ skip$1147, i$1148, 115 ]);
                                            }
                                          }
                                          if ($r120) return __(add_conv$1139, [ skip$1147, i$1148, 99 ]);
                                        }
                                      }
                                      if ($r121) return __(add_conv$1139, [ skip$1147, i$1148, 105 ]);
                                    }
                                  }
                                  if ($r122) return __(add_conv$1139, [ skip$1147, i$1148, 102 ]);
                                }
                              }
                              if ($r123) return __(add_conv$1139, [ skip$1147, i$1148, 66 ]);
                            }
                          }
                          if ($r124) return __(add_conv$1139, [ skip$1147, i$1148, conv$1149 ]);
                        }
                      }
                      if ($r125) {
                        {
                          var j$1155 = 1 + i$1148;
                          if (j$1155 > lim$1141) return __(add_conv$1139, [ skip$1147, i$1148, 105 ]);
                          var c$1156 = oc$$srefs(fmt$1138, j$1155);
                          var $r113 = false;
                          r$113: {
                            {
                              var $r114 = false;
                              r$114: {
                                {
                                  var switcher$1461 = -88 + c$1156;
                                  if (switcher$1461 < 0 || switcher$1461 > 32) { { $r114 = true; break r$114; } }
                                  switch (switcher$1461)
                                  {
                                  case 0: $r113 = true; break r$113;
                                  case 1: $r114 = true; break r$114;
                                  case 2: $r114 = true; break r$114;
                                  case 3: $r114 = true; break r$114;
                                  case 4: $r114 = true; break r$114;
                                  case 5: $r114 = true; break r$114;
                                  case 6: $r114 = true; break r$114;
                                  case 7: $r114 = true; break r$114;
                                  case 8: $r114 = true; break r$114;
                                  case 9: $r114 = true; break r$114;
                                  case 10: $r114 = true; break r$114;
                                  case 11: $r114 = true; break r$114;
                                  case 12: $r113 = true; break r$113;
                                  case 13: $r114 = true; break r$114;
                                  case 14: $r114 = true; break r$114;
                                  case 15: $r114 = true; break r$114;
                                  case 16: $r114 = true; break r$114;
                                  case 17: $r113 = true; break r$113;
                                  case 18: $r114 = true; break r$114;
                                  case 19: $r114 = true; break r$114;
                                  case 20: $r114 = true; break r$114;
                                  case 21: $r114 = true; break r$114;
                                  case 22: $r114 = true; break r$114;
                                  case 23: $r113 = true; break r$113;
                                  case 24: $r114 = true; break r$114;
                                  case 25: $r114 = true; break r$114;
                                  case 26: $r114 = true; break r$114;
                                  case 27: $r114 = true; break r$114;
                                  case 28: $r114 = true; break r$114;
                                  case 29: $r113 = true; break r$113;
                                  case 30: $r114 = true; break r$114;
                                  case 31: $r114 = true; break r$114;
                                  case 32: $r113 = true; break r$113;
                                  default: return null;
                                  }
                                }
                              }
                              if ($r114) return __(add_conv$1139, [ skip$1147, i$1148, 105 ]);
                            }
                          }
                          if ($r113) return __(add_char$1140, [ _(add_conv$1139, [ skip$1147, i$1148, conv$1149 ]), 105 ]);
                        }
                      }
                    }
                  }
                  if ($r126) return __(add_conv$1139, [ skip$1147, i$1148, conv$1149 ]);
                });
           var scan_fmt$1144 =
             _f(function (i$1161) {
                  if (!(i$1161 < lim$1141)) return i$1161;
                  if (oc$$srefs(fmt$1138, i$1161) === 37) return __(scan_fmt$1144, [ _(scan_flags$1142, [ false, 1 + i$1161 ]) ]);
                  return __(scan_fmt$1144, [ 1 + i$1161 ]);
                });
           _(scan_fmt$1144, [ 0 ]);
           return 0;
         });
    var summarize_format_type$1162 =
      _f(function (fmt$1163) {
           var len$1164 = fmt$1163.length;
           var b$1165 = _(oc$Buffer$[0], [ len$1164 ]);
           var add_char$1166 = _f(function (i$1167, c$1168) { _(oc$Buffer$[8], [ b$1165, c$1168 ]); return 1 + i$1167; });
           var add_conv$1169 =
             _f(function (skip$1170, i$1171, c$1172) {
                  if (skip$1170) _(oc$Buffer$[9], [ b$1165, "%_" ]); else _(oc$Buffer$[8], [ b$1165, 37 ]);
                  return __(add_char$1166, [ i$1171, c$1172 ]);
                });
           _(iter_on_format_args$1137, [ fmt$1163, add_conv$1169, add_char$1166 ]);
           return __(oc$Buffer$[1], [ b$1165 ]);
         });
    var Ac$1180 = $();
    var ac_of_format$1184 =
      _f(function (fmt$1185) {
           var ac$1186 = $(0, 0, 0);
           var incr_ac$1187 =
             _f(function (skip$1188, c$1189) {
                  var inc$1190 = c$1189 === 97 ? 2 : 1;
                  if (c$1189 === 114) ac$1186[2] = ac$1186[2] + 1; else;
                  if (skip$1188) return ac$1186[1] = ac$1186[1] + inc$1190;
                  return ac$1186[0] = ac$1186[0] + inc$1190;
                });
           var add_conv$1191 =
             _f(function (skip$1193, i$1194, c$1195) {
                  if (c$1195 !== 41 && c$1195 !== 125) _(incr_ac$1187, [ skip$1193, c$1195 ]); else;
                  return 1 + i$1194;
                });
           var add_char$1192 = _f(function (i$1196, c$1197) { return 1 + i$1196; });
           _(iter_on_format_args$1137, [ fmt$1185, add_conv$1191, add_char$1192 ]);
           return ac$1186;
         });
    var count_arguments_of_format$1198 =
      _f(function (fmt$1199) { var ac$1200 = _(ac_of_format$1184, [ fmt$1199 ]); return ac$1200[0]; });
    var list_iter_i$1201 =
      _f(function (f$1202, l$1203) {
           var loop$1204 =
             _f(function (i$1205, param$1457) {
                  if (param$1457) {
                    {
                      var xs$1208 = param$1457[1];
                      var x$1206 = param$1457[0];
                      if (xs$1208) { { _(f$1202, [ i$1205, x$1206 ]); return __(loop$1204, [ 1 + i$1205, xs$1208 ]); } }
                      return __(f$1202, [ i$1205, x$1206 ]);
                    }
                  }
                  return 0;
                });
           return __(loop$1204, [ 0, l$1203 ]);
         });
    var kapr$1209 =
      _f(function (kpr$1210, fmt$1211) {
           var nargs$1212 = _(count_arguments_of_format$1198, [ fmt$1211 ]);
           if (nargs$1212 < 0 || nargs$1212 > 6) {
             {
               var loop$1240 =
                 _f(function (i$1241, args$1242) {
                      if (i$1241 >= nargs$1212) {
                        {
                          var a$1243 = caml_make_vect(nargs$1212, 0);
                          _(list_iter_i$1201,
                            [
                              _f(function (i$1244, arg$1245) { return oc$$asets(a$1243, nargs$1212 - i$1244 - 1, arg$1245); }),
                              args$1242
                            ]);
                          return __(kpr$1210, [ fmt$1211, a$1243 ]);
                        }
                      }
                      return _f(function (x$1246) { return __(loop$1240, [ 1 + i$1241, $(x$1246, args$1242) ]); });
                    });
               return __(loop$1240, [ 0, 0 ]);
             }
           }
           switch (nargs$1212)
           {
           case 0: return __(kpr$1210, [ fmt$1211, $() ]);
           case 1:
             return _f(function (x$1213) {
                         var a$1214 = caml_make_vect(1, 0);
                         oc$$asets(a$1214, 0, x$1213);
                         return __(kpr$1210, [ fmt$1211, a$1214 ]);
                       });
           case 2:
             return _f(function (x$1215, y$1216) {
                         var a$1217 = caml_make_vect(2, 0);
                         oc$$asets(a$1217, 0, x$1215);
                         oc$$asets(a$1217, 1, y$1216);
                         return __(kpr$1210, [ fmt$1211, a$1217 ]);
                       });
           case 3:
             return _f(function (x$1218, y$1219, z$1220) {
                         var a$1221 = caml_make_vect(3, 0);
                         oc$$asets(a$1221, 0, x$1218);
                         oc$$asets(a$1221, 1, y$1219);
                         oc$$asets(a$1221, 2, z$1220);
                         return __(kpr$1210, [ fmt$1211, a$1221 ]);
                       });
           case 4:
             return _f(function (x$1222, y$1223, z$1224, t$1225) {
                         var a$1226 = caml_make_vect(4, 0);
                         oc$$asets(a$1226, 0, x$1222);
                         oc$$asets(a$1226, 1, y$1223);
                         oc$$asets(a$1226, 2, z$1224);
                         oc$$asets(a$1226, 3, t$1225);
                         return __(kpr$1210, [ fmt$1211, a$1226 ]);
                       });
           case 5:
             return _f(function (x$1227, y$1228, z$1229, t$1230, u$1231) {
                         var a$1232 = caml_make_vect(5, 0);
                         oc$$asets(a$1232, 0, x$1227);
                         oc$$asets(a$1232, 1, y$1228);
                         oc$$asets(a$1232, 2, z$1229);
                         oc$$asets(a$1232, 3, t$1230);
                         oc$$asets(a$1232, 4, u$1231);
                         return __(kpr$1210, [ fmt$1211, a$1232 ]);
                       });
           case 6:
             return _f(function (x$1233, y$1234, z$1235, t$1236, u$1237, v$1238) {
                         var a$1239 = caml_make_vect(6, 0);
                         oc$$asets(a$1239, 0, x$1233);
                         oc$$asets(a$1239, 1, y$1234);
                         oc$$asets(a$1239, 2, z$1235);
                         oc$$asets(a$1239, 3, t$1236);
                         oc$$asets(a$1239, 4, u$1237);
                         oc$$asets(a$1239, 5, v$1238);
                         return __(kpr$1210, [ fmt$1211, a$1239 ]);
                       });
           default: return null;
           }
         });
    var scan_positional_spec$1252 =
      _f(function (fmt$1253, got_spec$1254, n$1255, i$1256) {
           var d$1257 = oc$$srefu(fmt$1253, i$1256);
           if (-48 + d$1257 < 0 || -48 + d$1257 > 9) return __(got_spec$1254, [ 0, i$1256 ]);
           var get_int_literal$1258 =
             _f(function (accu$1259, j$1260) {
                  var d$1261 = oc$$srefu(fmt$1253, j$1260);
                  var $r82 = false;
                  r$82: {
                    {
                      if (!(d$1261 >= 48)) {
                        {
                          if (d$1261 !== 36) { { $r82 = true; break r$82; } }
                          if (accu$1259 === 0) return __(oc$Pervasives$[1], [ "printf: bad positional specification (0)." ]);
                          return __(got_spec$1254, [ $(_(Sformat$1056[3], [ accu$1259 ])), 1 + j$1260 ]);
                        }
                      }
                      if (d$1261 >= 58) { { $r82 = true; break r$82; } }
                      return __(get_int_literal$1258, [ 10 * accu$1259 + (d$1261 - 48), 1 + j$1260 ]);
                    }
                  }
                  if ($r82) return __(got_spec$1254, [ 0, i$1256 ]);
                });
           return __(get_int_literal$1258, [ d$1257 - 48, 1 + i$1256 ]);
         });
    var next_index$1262 =
      _f(function (spec$1263, n$1264) { if (spec$1263) return n$1264; return __(Sformat$1056[2], [ n$1264 ]); });
    var get_index$1265 = _f(function (spec$1266, n$1267) { if (spec$1266) return spec$1266[0]; return n$1267; });
    var format_float_lexeme$1269 =
      function () {
        var make_valid_float_lexeme$1270 =
          _f(function (s$1271) {
               var l$1272 = s$1271.length;
               var valid_float_loop$1273 =
                 _f(function (i$1274) {
                      if (i$1274 >= l$1272) return __(oc$Pervasives$[15], [ s$1271, "." ]);
                      var match$1452 = oc$$srefs(s$1271, i$1274);
                      var $r75 = false;
                      r$75: {
                        {
                          var switcher$1453 = -46 + match$1452;
                          if (!!(switcher$1453 < 0 || switcher$1453 > 23)) {
                            { if (switcher$1453 !== 55) { { $r75 = true; break r$75; } } return s$1271; }
                          }
                          if (!(-1 + switcher$1453 < 0 || -1 + switcher$1453 > 21)) { { $r75 = true; break r$75; } }
                          return s$1271;
                        }
                      }
                      if ($r75) return __(valid_float_loop$1273, [ i$1274 + 1 ]);
                    });
               return __(valid_float_loop$1273, [ 0 ]);
             });
        return _f(function (sfmt$1275, x$1276) {
                    var s$1277 = caml_format_float(sfmt$1275, x$1276);
                    var match$1451 = caml_classify_float(x$1276);
                    if (match$1451 >= 3) return s$1277;
                    return __(make_valid_float_lexeme$1270, [ s$1277 ]);
                  });
      }();
    var scan_format$1278 =
      _f(function (fmt$1279, args$1280, n$1281, pos$1282, cont_s$1283, cont_a$1284, cont_t$1285, cont_f$1286, cont_m$1287) {
           var get_arg$1288 =
             _f(function (spec$1289, n$1290) { return oc$$arefs(args$1280, _(get_index$1265, [ spec$1289, n$1290 ])); });
           var scan_positional$1291 =
             _f(function (n$1294, widths$1295, i$1296) {
                  var got_spec$1297 =
                    _f(function (spec$1298, i$1299) { return __(scan_flags$1292, [ spec$1298, n$1294, widths$1295, i$1299 ]); });
                  return __(scan_positional_spec$1252, [ fmt$1279, got_spec$1297, n$1294, i$1296 ]);
                });
           var scan_flags$1292 =
             _f(function (spec$1300, n$1301, widths$1302, i$1303) {
                  var match$1446 = oc$$srefu(fmt$1279, i$1303);
                  var $r30 = false;
                  r$30: {
                    {
                      var $r31 = false;
                      r$31: {
                        {
                          var switcher$1447 = -32 + match$1446;
                          if (switcher$1447 < 0 || switcher$1447 > 25) { { $r31 = true; break r$31; } }
                          switch (switcher$1447)
                          {
                          case 0: $r30 = true; break r$30;
                          case 1: $r31 = true; break r$31;
                          case 2: $r31 = true; break r$31;
                          case 3: $r30 = true; break r$30;
                          case 4: $r31 = true; break r$31;
                          case 5: $r31 = true; break r$31;
                          case 6: $r31 = true; break r$31;
                          case 7: $r31 = true; break r$31;
                          case 8: $r31 = true; break r$31;
                          case 9: $r31 = true; break r$31;
                          case 10:
                            var got_spec$1304 =
                              _f(function (wspec$1305, i$1306) {
                                   var width$1307 = _(get_arg$1288, [ wspec$1305, n$1301 ]);
                                   return __(scan_flags$1292,
                                             [
                                               spec$1300,
                                               _(next_index$1262, [ wspec$1305, n$1301 ]),
                                               $(width$1307, widths$1302),
                                               i$1306
                                             ]);
                                 });
                            return __(scan_positional_spec$1252, [ fmt$1279, got_spec$1304, n$1301, 1 + i$1303 ]);
                          case 11: $r30 = true; break r$30;
                          case 12: $r31 = true; break r$31;
                          case 13: $r30 = true; break r$30;
                          case 14: $r30 = true; break r$30;
                          case 15: $r31 = true; break r$31;
                          case 16: $r30 = true; break r$30;
                          case 17: $r30 = true; break r$30;
                          case 18: $r30 = true; break r$30;
                          case 19: $r30 = true; break r$30;
                          case 20: $r30 = true; break r$30;
                          case 21: $r30 = true; break r$30;
                          case 22: $r30 = true; break r$30;
                          case 23: $r30 = true; break r$30;
                          case 24: $r30 = true; break r$30;
                          case 25: $r30 = true; break r$30;
                          default: return null;
                          }
                        }
                      }
                      if ($r31) return __(scan_conv$1293, [ spec$1300, n$1301, widths$1302, i$1303 ]);
                    }
                  }
                  if ($r30) return __(scan_flags$1292, [ spec$1300, n$1301, widths$1302, 1 + i$1303 ]);
                });
           var scan_conv$1293 =
             _f(function (spec$1308, n$1309, widths$1310, i$1311) {
                  var conv$1312 = oc$$srefu(fmt$1279, i$1311);
                  var $r67 = false;
                  r$67: {
                    {
                      var $r66 = false;
                      r$66: {
                        {
                          var $r65 = false;
                          r$65: {
                            {
                              var $r64 = false;
                              r$64: {
                                {
                                  var $r63 = false;
                                  r$63: {
                                    {
                                      var $r62 = false;
                                      r$62: {
                                        {
                                          var $r61 = false;
                                          r$61: {
                                            {
                                              var $r68 = false;
                                              r$68: {
                                                {
                                                  if (conv$1312 >= 124) { { $r68 = true; break r$68; } }
                                                  switch (conv$1312)
                                                  {
                                                  case 0: $r68 = true; break r$68;
                                                  case 1: $r68 = true; break r$68;
                                                  case 2: $r68 = true; break r$68;
                                                  case 3: $r68 = true; break r$68;
                                                  case 4: $r68 = true; break r$68;
                                                  case 5: $r68 = true; break r$68;
                                                  case 6: $r68 = true; break r$68;
                                                  case 7: $r68 = true; break r$68;
                                                  case 8: $r68 = true; break r$68;
                                                  case 9: $r68 = true; break r$68;
                                                  case 10: $r68 = true; break r$68;
                                                  case 11: $r68 = true; break r$68;
                                                  case 12: $r68 = true; break r$68;
                                                  case 13: $r68 = true; break r$68;
                                                  case 14: $r68 = true; break r$68;
                                                  case 15: $r68 = true; break r$68;
                                                  case 16: $r68 = true; break r$68;
                                                  case 17: $r68 = true; break r$68;
                                                  case 18: $r68 = true; break r$68;
                                                  case 19: $r68 = true; break r$68;
                                                  case 20: $r68 = true; break r$68;
                                                  case 21: $r68 = true; break r$68;
                                                  case 22: $r68 = true; break r$68;
                                                  case 23: $r68 = true; break r$68;
                                                  case 24: $r68 = true; break r$68;
                                                  case 25: $r68 = true; break r$68;
                                                  case 26: $r68 = true; break r$68;
                                                  case 27: $r68 = true; break r$68;
                                                  case 28: $r68 = true; break r$68;
                                                  case 29: $r68 = true; break r$68;
                                                  case 30: $r68 = true; break r$68;
                                                  case 31: $r68 = true; break r$68;
                                                  case 32: $r68 = true; break r$68;
                                                  case 33: return __(cont_f$1286, [ n$1309, 1 + i$1311 ]);
                                                  case 34: $r68 = true; break r$68;
                                                  case 35: $r68 = true; break r$68;
                                                  case 36: $r68 = true; break r$68;
                                                  case 37: return __(cont_s$1283, [ n$1309, "%", 1 + i$1311 ]);
                                                  case 38: $r68 = true; break r$68;
                                                  case 39: $r68 = true; break r$68;
                                                  case 40: $r67 = true; break r$67;
                                                  case 41: return __(cont_s$1283, [ n$1309, "", 1 + i$1311 ]);
                                                  case 42: $r68 = true; break r$68;
                                                  case 43: $r68 = true; break r$68;
                                                  case 44: return __(cont_s$1283, [ n$1309, "", 1 + i$1311 ]);
                                                  case 45: $r68 = true; break r$68;
                                                  case 46: $r68 = true; break r$68;
                                                  case 47: $r68 = true; break r$68;
                                                  case 48: $r68 = true; break r$68;
                                                  case 49: $r68 = true; break r$68;
                                                  case 50: $r68 = true; break r$68;
                                                  case 51: $r68 = true; break r$68;
                                                  case 52: $r68 = true; break r$68;
                                                  case 53: $r68 = true; break r$68;
                                                  case 54: $r68 = true; break r$68;
                                                  case 55: $r68 = true; break r$68;
                                                  case 56: $r68 = true; break r$68;
                                                  case 57: $r68 = true; break r$68;
                                                  case 58: $r68 = true; break r$68;
                                                  case 59: $r68 = true; break r$68;
                                                  case 60: $r68 = true; break r$68;
                                                  case 61: $r68 = true; break r$68;
                                                  case 62: $r68 = true; break r$68;
                                                  case 63: $r68 = true; break r$68;
                                                  case 64: $r68 = true; break r$68;
                                                  case 65: $r68 = true; break r$68;
                                                  case 66: $r65 = true; break r$65;
                                                  case 67: $r62 = true; break r$62;
                                                  case 68: $r68 = true; break r$68;
                                                  case 69: $r64 = true; break r$64;
                                                  case 70:
                                                    var x$1327 = _(get_arg$1288, [ spec$1308, n$1309 ]);
                                                    var s$1328 =
                                                      widths$1310 === 0 ?
                                                        _(oc$Pervasives$[20], [ x$1327 ]) :
                                                        _(format_float_lexeme$1269,
                                                          [
                                                            _(extract_format_float$1110,
                                                              [ conv$1312, fmt$1279, pos$1282, i$1311, widths$1310 ]),
                                                            x$1327
                                                          ]);
                                                    return __(cont_s$1283,
                                                              [ _(next_index$1262, [ spec$1308, n$1309 ]), s$1328, 1 + i$1311 ]);
                                                  case 71: $r64 = true; break r$64;
                                                  case 72: $r68 = true; break r$68;
                                                  case 73: $r68 = true; break r$68;
                                                  case 74: $r68 = true; break r$68;
                                                  case 75: $r68 = true; break r$68;
                                                  case 76: $r66 = true; break r$66;
                                                  case 77: $r68 = true; break r$68;
                                                  case 78: $r63 = true; break r$63;
                                                  case 79: $r68 = true; break r$68;
                                                  case 80: $r68 = true; break r$68;
                                                  case 81: $r68 = true; break r$68;
                                                  case 82: $r68 = true; break r$68;
                                                  case 83: $r61 = true; break r$61;
                                                  case 84: $r68 = true; break r$68;
                                                  case 85: $r68 = true; break r$68;
                                                  case 86: $r68 = true; break r$68;
                                                  case 87: $r68 = true; break r$68;
                                                  case 88: $r63 = true; break r$63;
                                                  case 89: $r68 = true; break r$68;
                                                  case 90: $r68 = true; break r$68;
                                                  case 91: $r68 = true; break r$68;
                                                  case 92: $r68 = true; break r$68;
                                                  case 93: $r68 = true; break r$68;
                                                  case 94: $r68 = true; break r$68;
                                                  case 95: $r68 = true; break r$68;
                                                  case 96: $r68 = true; break r$68;
                                                  case 97:
                                                    var printer$1330 = _(get_arg$1288, [ spec$1308, n$1309 ]);
                                                    var n$1331 = _(Sformat$1056[2], [ _(get_index$1265, [ spec$1308, n$1309 ]) ]);
                                                    var arg$1332 = _(get_arg$1288, [ 0, n$1331 ]);
                                                    return __(cont_a$1284,
                                                              [
                                                                _(next_index$1262, [ spec$1308, n$1331 ]),
                                                                printer$1330,
                                                                arg$1332,
                                                                1 + i$1311
                                                              ]);
                                                  case 98: $r65 = true; break r$65;
                                                  case 99: $r62 = true; break r$62;
                                                  case 100: $r63 = true; break r$63;
                                                  case 101: $r64 = true; break r$64;
                                                  case 102: $r64 = true; break r$64;
                                                  case 103: $r64 = true; break r$64;
                                                  case 104: $r68 = true; break r$68;
                                                  case 105: $r63 = true; break r$63;
                                                  case 106: $r68 = true; break r$68;
                                                  case 107: $r68 = true; break r$68;
                                                  case 108: $r66 = true; break r$66;
                                                  case 109: $r68 = true; break r$68;
                                                  case 110: $r66 = true; break r$66;
                                                  case 111: $r63 = true; break r$63;
                                                  case 112: $r68 = true; break r$68;
                                                  case 113: $r68 = true; break r$68;
                                                  case 114: $r68 = true; break r$68;
                                                  case 115: $r61 = true; break r$61;
                                                  case 116:
                                                    var printer$1333 = _(get_arg$1288, [ spec$1308, n$1309 ]);
                                                    return __(cont_t$1285,
                                                              [
                                                                _(next_index$1262, [ spec$1308, n$1309 ]),
                                                                printer$1333,
                                                                1 + i$1311
                                                              ]);
                                                  case 117: $r63 = true; break r$63;
                                                  case 118: $r68 = true; break r$68;
                                                  case 119: $r68 = true; break r$68;
                                                  case 120: $r63 = true; break r$63;
                                                  case 121: $r68 = true; break r$68;
                                                  case 122: $r68 = true; break r$68;
                                                  case 123: $r67 = true; break r$67;
                                                  default: return null;
                                                  }
                                                }
                                              }
                                              if ($r68) return __(bad_conversion_format$1061, [ fmt$1279, i$1311, conv$1312 ]);
                                            }
                                          }
                                          if ($r61) {
                                            {
                                              var x$1318 = _(get_arg$1288, [ spec$1308, n$1309 ]);
                                              var x$1319 =
                                                conv$1312 === 115 ?
                                                  x$1318 :
                                                  _(oc$Pervasives$[15],
                                                    [ "\"", _(oc$Pervasives$[15], [ _(oc$String$[7], [ x$1318 ]), "\"" ]) ]);
                                              var s$1320 =
                                                i$1311 === 1 + pos$1282 ?
                                                  x$1319 :
                                                  _(format_string$1080,
                                                    [ _(extract_format$1085, [ fmt$1279, pos$1282, i$1311, widths$1310 ]), x$1319 ]);
                                              return __(cont_s$1283,
                                                        [ _(next_index$1262, [ spec$1308, n$1309 ]), s$1320, 1 + i$1311 ]);
                                            }
                                          }
                                        }
                                      }
                                      if ($r62) {
                                        {
                                          var x$1321 = _(get_arg$1288, [ spec$1308, n$1309 ]);
                                          var s$1322 =
                                            conv$1312 === 99 ?
                                              _(oc$String$[0], [ 1, x$1321 ]) :
                                              _(oc$Pervasives$[15],
                                                [ "\'", _(oc$Pervasives$[15], [ _(oc$Char$[1], [ x$1321 ]), "\'" ]) ]);
                                          return __(cont_s$1283, [ _(next_index$1262, [ spec$1308, n$1309 ]), s$1322, 1 + i$1311 ]);
                                        }
                                      }
                                    }
                                  }
                                  if ($r63) {
                                    {
                                      var x$1323 = _(get_arg$1288, [ spec$1308, n$1309 ]);
                                      var s$1324 =
                                        caml_format_int(_(extract_format$1085, [ fmt$1279, pos$1282, i$1311, widths$1310 ]),
                                                        x$1323);
                                      return __(cont_s$1283, [ _(next_index$1262, [ spec$1308, n$1309 ]), s$1324, 1 + i$1311 ]);
                                    }
                                  }
                                }
                              }
                              if ($r64) {
                                {
                                  var x$1325 = _(get_arg$1288, [ spec$1308, n$1309 ]);
                                  var s$1326 =
                                    caml_format_float(_(extract_format$1085, [ fmt$1279, pos$1282, i$1311, widths$1310 ]), x$1325);
                                  return __(cont_s$1283, [ _(next_index$1262, [ spec$1308, n$1309 ]), s$1326, 1 + i$1311 ]);
                                }
                              }
                            }
                          }
                          if ($r65) {
                            {
                              var x$1329 = _(get_arg$1288, [ spec$1308, n$1309 ]);
                              return __(cont_s$1283,
                                        [
                                          _(next_index$1262, [ spec$1308, n$1309 ]),
                                          _(oc$Pervasives$[17], [ x$1329 ]),
                                          1 + i$1311
                                        ]);
                            }
                          }
                        }
                      }
                      if ($r66) {
                        {
                          var match$1449 = oc$$srefu(fmt$1279, 1 + i$1311);
                          var $r56 = false;
                          r$56: {
                            {
                              var $r57 = false;
                              r$57: {
                                {
                                  var switcher$1450 = -88 + match$1449;
                                  if (switcher$1450 < 0 || switcher$1450 > 32) { { $r57 = true; break r$57; } }
                                  switch (switcher$1450)
                                  {
                                  case 0: $r56 = true; break r$56;
                                  case 1: $r57 = true; break r$57;
                                  case 2: $r57 = true; break r$57;
                                  case 3: $r57 = true; break r$57;
                                  case 4: $r57 = true; break r$57;
                                  case 5: $r57 = true; break r$57;
                                  case 6: $r57 = true; break r$57;
                                  case 7: $r57 = true; break r$57;
                                  case 8: $r57 = true; break r$57;
                                  case 9: $r57 = true; break r$57;
                                  case 10: $r57 = true; break r$57;
                                  case 11: $r57 = true; break r$57;
                                  case 12: $r56 = true; break r$56;
                                  case 13: $r57 = true; break r$57;
                                  case 14: $r57 = true; break r$57;
                                  case 15: $r57 = true; break r$57;
                                  case 16: $r57 = true; break r$57;
                                  case 17: $r56 = true; break r$56;
                                  case 18: $r57 = true; break r$57;
                                  case 19: $r57 = true; break r$57;
                                  case 20: $r57 = true; break r$57;
                                  case 21: $r57 = true; break r$57;
                                  case 22: $r57 = true; break r$57;
                                  case 23: $r56 = true; break r$56;
                                  case 24: $r57 = true; break r$57;
                                  case 25: $r57 = true; break r$57;
                                  case 26: $r57 = true; break r$57;
                                  case 27: $r57 = true; break r$57;
                                  case 28: $r57 = true; break r$57;
                                  case 29: $r56 = true; break r$56;
                                  case 30: $r57 = true; break r$57;
                                  case 31: $r57 = true; break r$57;
                                  case 32: $r56 = true; break r$56;
                                  default: return null;
                                  }
                                }
                              }
                              if ($r57) {
                                {
                                  var x$1339 = _(get_arg$1288, [ spec$1308, n$1309 ]);
                                  var s$1340 =
                                    caml_format_int(_(extract_format$1085, [ fmt$1279, pos$1282, i$1311, widths$1310 ]), x$1339);
                                  return __(cont_s$1283, [ _(next_index$1262, [ spec$1308, n$1309 ]), s$1340, 1 + i$1311 ]);
                                }
                              }
                            }
                          }
                          if ($r56) {
                            {
                              var i$1334 = 1 + i$1311;
                              var s$1335 =
                                function () {
                                  var $r51 = false;
                                  r$51: {
                                    {
                                      var switcher$1448 = -108 + conv$1312;
                                      if (switcher$1448 < 0 || switcher$1448 > 2) { { $r51 = true; break r$51; } }
                                      switch (switcher$1448)
                                      {
                                      case 0:
                                        var x$1336 = _(get_arg$1288, [ spec$1308, n$1309 ]);
                                        return caml_format_int(_(extract_format$1085, [ fmt$1279, pos$1282, i$1334, widths$1310 ]),
                                                               x$1336);
                                      case 1: $r51 = true; break r$51;
                                      case 2:
                                        var x$1337 = _(get_arg$1288, [ spec$1308, n$1309 ]);
                                        return caml_format_int(_(extract_format$1085, [ fmt$1279, pos$1282, i$1334, widths$1310 ]),
                                                               x$1337);
                                      default: return null;
                                      }
                                    }
                                  }
                                  if ($r51) {
                                    {
                                      var x$1338 = _(get_arg$1288, [ spec$1308, n$1309 ]);
                                      return caml_format_int(_(extract_format$1085, [ fmt$1279, pos$1282, i$1334, widths$1310 ]),
                                                             x$1338);
                                    }
                                  }
                                }();
                              return __(cont_s$1283, [ _(next_index$1262, [ spec$1308, n$1309 ]), s$1335, 1 + i$1334 ]);
                            }
                          }
                        }
                      }
                    }
                  }
                  if ($r67) {
                    {
                      var xf$1341 = _(get_arg$1288, [ spec$1308, n$1309 ]);
                      var i$1342 = 1 + i$1311;
                      var j$1343 = _(sub_format_for_printf$1135, [ conv$1312, fmt$1279, i$1342 ]);
                      if (conv$1312 === 123)
                        return __(cont_s$1283,
                                  [ _(next_index$1262, [ spec$1308, n$1309 ]), _(summarize_format_type$1162, [ xf$1341 ]), j$1343 ]);
                      return __(cont_m$1287, [ _(next_index$1262, [ spec$1308, n$1309 ]), xf$1341, j$1343 ]);
                    }
                  }
                });
           return __(scan_positional$1291, [ n$1281, 0, 1 + pos$1282 ]);
         });
    var mkprintf$1344 =
      _f(function (to_s$1345, get_out$1346, outc$1347, outs$1348, flush$1349, k$1350, fmt$1351) {
           var out$1352 = _(get_out$1346, [ fmt$1351 ]);
           var pr$1353 =
             _f(function (k$1354, n$1355, fmt$1356, v$1357) {
                  var len$1358 = fmt$1356.length;
                  var doprn$1359 =
                    _f(function (n$1365, i$1366) {
                         if (i$1366 >= len$1358) return _(k$1354, [ out$1352 ]);
                         var c$1367 = oc$$srefu(fmt$1356, i$1366);
                         if (c$1367 !== 37) {
                           { _(outc$1347, [ out$1352, c$1367 ]); return __(doprn$1359, [ n$1365, 1 + i$1366 ]); }
                         }
                         return __(scan_format$1278,
                                   [
                                     fmt$1356,
                                     v$1357,
                                     n$1365,
                                     i$1366,
                                     cont_s$1360,
                                     cont_a$1361,
                                     cont_t$1362,
                                     cont_f$1363,
                                     cont_m$1364
                                   ]);
                       });
                  var cont_s$1360 =
                    _f(function (n$1368, s$1369, i$1370) {
                         _(outs$1348, [ out$1352, s$1369 ]);
                         return __(doprn$1359, [ n$1368, i$1370 ]);
                       });
                  var cont_a$1361 =
                    _f(function (n$1371, printer$1372, arg$1373, i$1374) {
                         if (to_s$1345)
                           _(outs$1348, [ out$1352, _(printer$1372, [ 0, arg$1373 ]) ]);
                         else
                           _(printer$1372, [ out$1352, arg$1373 ]);
                         return __(doprn$1359, [ n$1371, i$1374 ]);
                       });
                  var cont_t$1362 =
                    _f(function (n$1375, printer$1376, i$1377) {
                         if (to_s$1345) _(outs$1348, [ out$1352, _(printer$1376, [ 0 ]) ]); else _(printer$1376, [ out$1352 ]);
                         return __(doprn$1359, [ n$1375, i$1377 ]);
                       });
                  var cont_f$1363 =
                    _f(function (n$1378, i$1379) { _(flush$1349, [ out$1352 ]); return __(doprn$1359, [ n$1378, i$1379 ]); });
                  var cont_m$1364 =
                    _f(function (n$1380, xf$1381, i$1382) {
                         var m$1383 = _(Sformat$1056[1], [ _(count_arguments_of_format$1198, [ xf$1381 ]), n$1380 ]);
                         return __(pr$1353,
                                   [
                                     _f(function (param$1445) { return __(doprn$1359, [ m$1383, i$1382 ]); }),
                                     n$1380,
                                     xf$1381,
                                     v$1357
                                   ]);
                       });
                  return __(doprn$1359, [ n$1355, 0 ]);
                });
           var kpr$1384 = _(pr$1353, [ k$1350, _(Sformat$1056[0], [ 0 ]) ]);
           return __(kapr$1209, [ kpr$1384, fmt$1351 ]);
         });
    var kfprintf$1385 =
      _f(function (k$1386, oc$1387) {
           return __(mkprintf$1344,
                     [
                       false,
                       _f(function (param$1444) { return oc$1387; }),
                       oc$Pervasives$[45],
                       oc$Pervasives$[46],
                       oc$Pervasives$[43],
                       k$1386
                     ]);
         });
    var ifprintf$1388 =
      _f(function (oc$1389) {
           return __(kapr$1209, [ _f(function (param$1442) { return _f(function (prim$1443) { return 0; }); }) ]);
         });
    var fprintf$1390 = _f(function (oc$1391) { return __(kfprintf$1385, [ _f(function (prim$1441) { return 0; }), oc$1391 ]); });
    var printf$1392 = _f(function (fmt$1393) { return __(fprintf$1390, [ oc$Pervasives$[23], fmt$1393 ]); });
    var eprintf$1394 = _f(function (fmt$1395) { return __(fprintf$1390, [ oc$Pervasives$[24], fmt$1395 ]); });
    var kbprintf$1396 =
      _f(function (k$1397, b$1398) {
           return __(mkprintf$1344,
                     [
                       false,
                       _f(function (param$1439) { return b$1398; }),
                       oc$Buffer$[8],
                       oc$Buffer$[9],
                       _f(function (prim$1440) { return 0; }),
                       k$1397
                     ]);
         });
    var bprintf$1399 = _f(function (b$1400) { return __(kbprintf$1396, [ _f(function (prim$1438) { return 0; }), b$1400 ]); });
    var get_buff$1401 = _f(function (fmt$1402) { var len$1403 = 2 * fmt$1402.length; return __(oc$Buffer$[0], [ len$1403 ]); });
    var get_contents$1404 =
      _f(function (b$1405) { var s$1406 = _(oc$Buffer$[1], [ b$1405 ]); _(oc$Buffer$[6], [ b$1405 ]); return s$1406; });
    var get_cont$1407 = _f(function (k$1408, b$1409) { return __(k$1408, [ _(get_contents$1404, [ b$1409 ]) ]); });
    var ksprintf$1410 =
      _f(function (k$1411) {
           return __(mkprintf$1344,
                     [
                       true,
                       get_buff$1401,
                       oc$Buffer$[8],
                       oc$Buffer$[9],
                       _f(function (prim$1437) { return 0; }),
                       _(get_cont$1407, [ k$1411 ])
                     ]);
         });
    var sprintf$1413 =
      _f(function (fmt$1414) { return __(ksprintf$1410, [ _f(function (s$1415) { return s$1415; }), fmt$1414 ]); });
    var CamlinternalPr$1430 =
      function () {
        var Tformat$1429 = $(ac_of_format$1184, sub_format$1117, summarize_format_type$1162, scan_format$1278, kapr$1209);
        return $(Sformat$1056, Tformat$1429);
      }();
    return $(fprintf$1390, printf$1392, eprintf$1394, ifprintf$1388, sprintf$1413, bprintf$1399, kfprintf$1385, ksprintf$1410,
             kbprintf$1396, ksprintf$1410,
             $(function () { var let$1436 = CamlinternalPr$1430[0]; return $(let$1436[0], let$1436[2], let$1436[4], let$1436[5]); }
               (), CamlinternalPr$1430[1]));
  }();
var oc$Random$ =
  function () {
    var init$1046 = _f(function (prim$1096) { return 0; });
    var full_init$1047 = _f(function (prim$1095) { return 0; });
    var self_init$1048 = _f(function (prim$1094) { return 0; });
    var bits$1049 = _f(function (param$1093) { return Math.floor(Math.random() * 1073741824); });
    var int$1050 = _f(function (b$1051) { return Math.floor(Math.random() * b$1051); });
    var int32$1052 = _f(function (b$1053) { return Math.floor(Math.random() * b$1053); });
    var nativeint$1054 = _f(function (b$1055) { return Math.floor(Math.random() * b$1055); });
    var int64$1056 = _f(function (param$1092) { return oc$Int64$[0]; });
    var float$1057 = _f(function (b$1058) { return Math.random() * b$1058; });
    var bool$1059 = _f(function (param$1091) { return Math.random() < 0.5; });
    var State$1076 =
      function () {
        var make$1061 = _f(function (prim$1090) { return 0; });
        var make_self_init$1062 = _f(function (prim$1089) { return 0; });
        var copy$1063 = _f(function (prim$1088) { return 0; });
        var bits$1064 = _f(function (param$1087) { return __(bits$1049, [ 0 ]); });
        var int$1065 = _f(function (param$1086, b$1066) { return __(int$1050, [ b$1066 ]); });
        var int32$1067 = _f(function (param$1085, b$1068) { return __(int32$1052, [ b$1068 ]); });
        var nativeint$1069 = _f(function (param$1084, b$1070) { return __(nativeint$1054, [ b$1070 ]); });
        var int64$1071 = _f(function (param$1083, b$1072) { return __(int64$1056, [ b$1072 ]); });
        var float$1073 = _f(function (param$1082, b$1074) { return __(float$1057, [ b$1074 ]); });
        var bool$1075 = _f(function (param$1081) { return __(bool$1059, [ 0 ]); });
        return $(make$1061, make_self_init$1062, copy$1063, bits$1064, int$1065, int32$1067, nativeint$1069, int64$1071,
                 float$1073, bool$1075);
      }();
    var get_state$1077 = _f(function (prim$1080) { return 0; });
    var set_state$1078 = _f(function (prim$1079) { return 0; });
    return $(init$1046, full_init$1047, self_init$1048, bits$1049, int$1050, int32$1052, nativeint$1054, int64$1056, float$1057,
             bool$1059, State$1076, get_state$1077, set_state$1078);
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
var oc$Javascript$ =
  function () {
    var typeof$1050 = _f(function (o$1051) { return typeof o$1051; });
    var true_$1052 = true;
    var false_$1053 = false;
    var new_Date$1091 = _f(function (param$1119) { return new Date(); });
    var Js_string$1116 = $();
    var Math$1118 = function () { var pi$1117 = Math.PI; return $(pi$1117); }();
    return $(typeof$1050, true_$1052, false_$1053, new_Date$1091, Js_string$1116, Math$1118);
  }();
var oc$Lwt_sequence$ =
  function () {
    var Empty$1030 = $("Lwt_sequence.Empty");
    var get$1051 = _f(function (node$1052) { return node$1052[2]; });
    var set$1053 = _f(function (node$1054, data$1055) { return node$1054[2] = data$1055; });
    var remove$1056 =
      _f(function (node$1057) {
           if (node$1057[3]) {
             { node$1057[3] = false; var seq$1058 = node$1057; seq$1058[0][1] = seq$1058[1]; return seq$1058[1][0] = seq$1058[0]; }
           }
           return 0;
         });
    var create$1059 =
      _f(function (param$1137) {
           var seq$1060 = $(seq$1060, seq$1060);
           seq$1060[0] = seq$1060;
           seq$1060[1] = seq$1060;
           return seq$1060;
         });
    var is_empty$1061 = _f(function (seq$1062) { return seq$1062[1] === seq$1062; });
    var add_l$1063 =
      _f(function (data$1064, seq$1065) {
           var node$1066 = $(seq$1065, seq$1065[1], data$1064, true);
           seq$1065[1][0] = node$1066;
           seq$1065[1] = node$1066;
           return node$1066;
         });
    var add_r$1067 =
      _f(function (data$1068, seq$1069) {
           var node$1070 = $(seq$1069[0], seq$1069, data$1068, true);
           seq$1069[0][1] = node$1070;
           seq$1069[0] = node$1070;
           return node$1070;
         });
    var take_l$1071 =
      _f(function (seq$1072) {
           if (_(is_empty$1061, [ seq$1072 ])) throw $(Empty$1030);
           var node$1073 = seq$1072[1];
           _(remove$1056, [ node$1073 ]);
           return node$1073[2];
         });
    var take_r$1074 =
      _f(function (seq$1075) {
           if (_(is_empty$1061, [ seq$1075 ])) throw $(Empty$1030);
           var node$1076 = seq$1075[0];
           _(remove$1056, [ node$1076 ]);
           return node$1076[2];
         });
    var take_opt_l$1077 =
      _f(function (seq$1078) {
           if (_(is_empty$1061, [ seq$1078 ])) return 0;
           var node$1079 = seq$1078[1];
           _(remove$1056, [ node$1079 ]);
           return $(node$1079[2]);
         });
    var take_opt_r$1080 =
      _f(function (seq$1081) {
           if (_(is_empty$1061, [ seq$1081 ])) return 0;
           var node$1082 = seq$1081[0];
           _(remove$1056, [ node$1082 ]);
           return $(node$1082[2]);
         });
    var transfer_l$1083 =
      _f(function (s1$1084, s2$1085) {
           s2$1085[1][0] = s1$1084[0];
           s1$1084[0][1] = s2$1085[1];
           s2$1085[1] = s1$1084[1];
           s1$1084[1][0] = s2$1085;
           s1$1084[0] = s1$1084;
           return s1$1084[1] = s1$1084;
         });
    var transfer_r$1086 =
      _f(function (s1$1087, s2$1088) {
           s2$1088[0][1] = s1$1087[1];
           s1$1087[1][0] = s2$1088[0];
           s2$1088[0] = s1$1087[0];
           s1$1087[0][1] = s2$1088;
           s1$1087[0] = s1$1087;
           return s1$1087[1] = s1$1087;
         });
    var iter_l$1089 =
      _f(function (f$1090, seq$1091) {
           var loop$1092 =
             _f(function (curr$1093) {
                  if (curr$1093 !== seq$1091) {
                    {
                      var node$1094 = curr$1093;
                      if (node$1094[3]) _(f$1090, [ node$1094[2] ]); else;
                      return __(loop$1092, [ node$1094[1] ]);
                    }
                  }
                  return 0;
                });
           return __(loop$1092, [ seq$1091[1] ]);
         });
    var iter_r$1095 =
      _f(function (f$1096, seq$1097) {
           var loop$1098 =
             _f(function (curr$1099) {
                  if (curr$1099 !== seq$1097) {
                    {
                      var node$1100 = curr$1099;
                      if (node$1100[3]) _(f$1096, [ node$1100[2] ]); else;
                      return __(loop$1098, [ node$1100[0] ]);
                    }
                  }
                  return 0;
                });
           return __(loop$1098, [ seq$1097[0] ]);
         });
    var iter_node_l$1101 =
      _f(function (f$1102, seq$1103) {
           var loop$1104 =
             _f(function (curr$1105) {
                  if (curr$1105 !== seq$1103) {
                    {
                      var node$1106 = curr$1105;
                      if (node$1106[3]) _(f$1102, [ node$1106 ]); else;
                      return __(loop$1104, [ node$1106[1] ]);
                    }
                  }
                  return 0;
                });
           return __(loop$1104, [ seq$1103[1] ]);
         });
    var iter_node_r$1107 =
      _f(function (f$1108, seq$1109) {
           var loop$1110 =
             _f(function (curr$1111) {
                  if (curr$1111 !== seq$1109) {
                    {
                      var node$1112 = curr$1111;
                      if (node$1112[3]) _(f$1108, [ node$1112 ]); else;
                      return __(loop$1110, [ node$1112[0] ]);
                    }
                  }
                  return 0;
                });
           return __(loop$1110, [ seq$1109[0] ]);
         });
    var fold_l$1113 =
      _f(function (f$1114, seq$1115, acc$1116) {
           var loop$1117 =
             _f(function (curr$1118, acc$1119) {
                  if (curr$1118 === seq$1115) return acc$1119;
                  var node$1120 = curr$1118;
                  if (node$1120[3]) return __(loop$1117, [ node$1120[1], _(f$1114, [ node$1120[2], acc$1119 ]) ]);
                  return __(loop$1117, [ node$1120[1], acc$1119 ]);
                });
           return __(loop$1117, [ seq$1115[1], acc$1116 ]);
         });
    var fold_r$1121 =
      _f(function (f$1122, seq$1123, acc$1124) {
           var loop$1125 =
             _f(function (curr$1126, acc$1127) {
                  if (curr$1126 === seq$1123) return acc$1127;
                  var node$1128 = curr$1126;
                  if (node$1128[3]) return __(loop$1125, [ node$1128[0], _(f$1122, [ node$1128[2], acc$1127 ]) ]);
                  return __(loop$1125, [ node$1128[1], acc$1127 ]);
                });
           return __(loop$1125, [ seq$1123[0], acc$1124 ]);
         });
    return $(get$1051, set$1053, remove$1056, create$1059, is_empty$1061, add_l$1063, add_r$1067, Empty$1030, take_l$1071,
             take_r$1074, take_opt_l$1077, take_opt_r$1080, transfer_l$1083, transfer_r$1086, iter_l$1089, iter_r$1095,
             iter_node_l$1101, iter_node_r$1107, fold_l$1113, fold_r$1121);
  }();
var oc$Lwt$ =
  function () {
    var Canceled$1030 = $("Lwt.Canceled");
    var max_removed$1096 = 42;
    var repr_rec$1097 =
      _f(function (t$1098) {
           var match$1539 = t$1098[0];
           switch ($t(match$1539))
           {
           case 3:
             var t$27$1099 = match$1539[0];
             var t$27$27$1100 = _(repr_rec$1097, [ t$27$1099 ]);
             if (t$27$27$1100 !== t$27$1099) t$1098[0] = $3(t$27$27$1100); else;
             return t$27$27$1100;
           default: return t$1098;
           }
         });
    var repr$1101 = _f(function (t$1102) { return __(repr_rec$1097, [ t$1102 ]); });
    var run_waiters_rec$1103 =
      _f(function (state$1104, ws$1105, rem$1106) {
           if (typeof ws$1105 == 'number')
             switch (ws$1105)
             {
             case 0: if (rem$1106) return __(run_waiters_rec$1103, [ state$1104, rem$1106[0], rem$1106[1] ]); return 0;
             default: return null;
             }
           else
             switch ($t(ws$1105))
             {
             case 0:
               var match$1538 = ws$1105[0][0];
               if (match$1538) {
                 {
                   var f$1115 = match$1538[0];
                   if (rem$1106) {
                     { _(f$1115, [ state$1104 ]); return __(run_waiters_rec$1103, [ state$1104, rem$1106[0], rem$1106[1] ]); }
                   }
                   return __(f$1115, [ state$1104 ]);
                 }
               }
               if (rem$1106) return __(run_waiters_rec$1103, [ state$1104, rem$1106[0], rem$1106[1] ]);
               return 0;
             case 1:
               var f$1109 = ws$1105[0];
               if (rem$1106) {
                 { _(f$1109, [ state$1104 ]); return __(run_waiters_rec$1103, [ state$1104, rem$1106[0], rem$1106[1] ]); }
               }
               return __(f$1109, [ state$1104 ]);
             case 2: return __(run_waiters_rec$1103, [ state$1104, ws$1105[0], $(ws$1105[1], rem$1106) ]);
             default: return null;
             }
         });
    var run_waiters$1121 =
      _f(function (waiters$1122, state$1123) { return __(run_waiters_rec$1103, [ state$1123, waiters$1122, 0 ]); });
    var restart$1124 =
      _f(function (t$1125, state$1126, caller$1127) {
           var t$1128 = _(repr_rec$1097, [ t$1125 ]);
           var match$1530 = t$1128[0];
           var $r127 = false;
           r$127:
             switch ($t(match$1530))
             {
             case 1: if (!(match$1530[0][0] === Canceled$1030)) { { $r127 = true; break r$127; } } return 0;
             case 2:
               var waiters$1129 = match$1530[0][1];
               t$1128[0] = state$1126;
               return __(run_waiters$1121, [ waiters$1129, state$1126 ]);
             default: $r127 = true; break r$127;
             }
           if ($r127) return __(oc$Pervasives$[0], [ caller$1127 ]);
         });
    var restart_cancel$1130 =
      _f(function (t$1131) {
           var t$1132 = _(repr_rec$1097, [ t$1131 ]);
           var match$1527 = t$1132[0];
           switch ($t(match$1527))
           {
           case 2:
             var waiters$1133 = match$1527[0][1];
             var state$1135 = $1($(Canceled$1030));
             t$1132[0] = state$1135;
             return __(run_waiters$1121, [ waiters$1133, state$1135 ]);
           default: return 0;
           }
         });
    var wakeup$1136 = _f(function (t$1137, v$1138) { return __(restart$1124, [ t$1137, $(v$1138), "Lwt.wakeup" ]); });
    var wakeup_exn$1139 = _f(function (t$1140, e$1141) { return __(restart$1124, [ t$1140, $1(e$1141), "Lwt.wakeup_exn" ]); });
    var cancel$1142 =
      _f(function (t$1143) {
           var match$1523 = _(repr$1101, [ t$1143 ])[0];
           switch ($t(match$1523))
           {
           case 2:
             var cancel$1144 = match$1523[0][0];
             var f$1145 = cancel$1144[0];
             cancel$1144[0] = _f(function (prim$1522) { return 0; });
             return __(f$1145, [ 0 ]);
           default: return 0;
           }
         });
    var append$1146 =
      _f(function (l1$1147, l2$1148) {
           if (typeof l1$1147 == 'number') return l2$1148;
           if (typeof l2$1148 == 'number') return l1$1147;
           return $2(l1$1147, l2$1148);
         });
    var cleanup$1149 =
      _f(function (ws$1152) {
           if (typeof ws$1152 == 'number')
             switch (ws$1152) { default: return ws$1152; }
           else
             switch ($t(ws$1152))
             {
             case 0: var match$1519 = ws$1152[0][0]; if (match$1519) return ws$1152; return 0;
             case 2: return __(append$1146, [ _(cleanup$1149, [ ws$1152[0] ]), _(cleanup$1149, [ ws$1152[1] ]) ]);
             default: return ws$1152;
             }
         });
    var connect$1153 =
      _f(function (t1$1154, t2$1155) {
           var t1$1156 = _(repr$1101, [ t1$1154 ]);
           var t2$1157 = _(repr$1101, [ t2$1155 ]);
           var match$1517 = t1$1156[0];
           switch ($t(match$1517))
           {
           case 2:
             if (t1$1156 === t2$1157) return 0;
             var sleeper1$1158 = match$1517[0];
             var state2$1160 = t2$1157[0];
             switch ($t(state2$1160))
             {
             case 2:
               var sleeper2$1159 = state2$1160[0];
               t2$1157[0] = $3(t1$1156);
               sleeper1$1158[0][0] = sleeper2$1159[0][0];
               var waiters$1161 = _(append$1146, [ sleeper1$1158[1], sleeper2$1159[1] ]);
               var removed$1162 = sleeper1$1158[2] + sleeper2$1159[2];
               if (removed$1162 > max_removed$1096) {
                 { sleeper1$1158[2] = 0; return sleeper1$1158[1] = _(cleanup$1149, [ waiters$1161 ]); }
               }
               sleeper1$1158[2] = removed$1162;
               return sleeper1$1158[1] = waiters$1161;
             default: t1$1156[0] = state2$1160; return __(run_waiters$1121, [ sleeper1$1158[1], state2$1160 ]);
             }
             break;
           default: return __(oc$Pervasives$[0], [ "Lwt.connect" ]);
           }
         });
    var fast_connect$1163 =
      _f(function (t$1164, state$1165) {
           var t$1166 = _(repr$1101, [ t$1164 ]);
           var match$1513 = t$1166[0];
           switch ($t(match$1513))
           {
           case 2:
             var waiters$1167 = match$1513[0][1];
             t$1166[0] = state$1165;
             return __(run_waiters$1121, [ waiters$1167, state$1165 ]);
           default: return __(oc$Pervasives$[0], [ "Lwt.fast_connect" ]);
           }
         });
    var return$1168 = _f(function (v$1169) { return $($(v$1169)); });
    var fail$1170 = _f(function (e$1171) { return $($1(e$1171)); });
    var temp$1172 = _f(function (r$1173) { return $($2($(r$1173, 0, 0))); });
    var no_cancel$1174 = $(_f(function (prim$1511) { return 0; }));
    var wait$1175 = _f(function (param$1509) { var t$1176 = $($2($(no_cancel$1174, 0, 0))); return $(t$1176, t$1176); });
    var task$1177 =
      _f(function (param$1506) {
           var t$1178 = $($2($($(_f(function (param$1508) { return __(restart_cancel$1130, [ t$1178 ]); })), 0, 0)));
           return $(t$1178, t$1178);
         });
    var apply$1179 =
      _f(function (f$1180, x$1181) { try { return _(f$1180, [ x$1181 ]); } catch (e$1182) { return __(fail$1170, [ e$1182 ]); } });
    var add_immutable_waiter$1183 =
      _f(function (sleeper$1184, waiter$1185) {
           return sleeper$1184[1] =
                    function () {
                      var match$1505 = sleeper$1184[1];
                      if (typeof match$1505 == 'number') return $1(waiter$1185);
                      return $2($1(waiter$1185), sleeper$1184[1]);
                    }();
         });
    var add_removable_waiter$1186 =
      _f(function (sleeper$1187, waiter$1188) {
           return sleeper$1187[1] =
                    function () {
                      var match$1504 = sleeper$1187[1];
                      if (typeof match$1504 == 'number') return $(waiter$1188);
                      return $2($(waiter$1188), sleeper$1187[1]);
                    }();
         });
    var on_cancel$1189 =
      _f(function (t$1190, f$1191) {
           var t$1192 = _(repr$1101, [ t$1190 ]);
           var match$1502 = t$1192[0];
           var $r88 = false;
           r$88:
             switch ($t(match$1502))
             {
             case 1: if (!(match$1502[0][0] === Canceled$1030)) { { $r88 = true; break r$88; } } return __(f$1191, [ 0 ]);
             case 2:
               return __(add_immutable_waiter$1183,
                         [
                           match$1502[0],
                           _f(function (param$1499) {
                                var $r87 = false;
                                r$87:
                                  switch ($t(param$1499))
                                  {
                                  case 1:
                                    if (!(param$1499[0][0] === Canceled$1030)) { { $r87 = true; break r$87; } }
                                    try { return _(f$1191, [ 0 ]); } catch (exn$1500) { return 0; }
                                    break;
                                  default: $r87 = true; break r$87;
                                  }
                                if ($r87) return 0;
                              })
                         ]);
             default: $r88 = true; break r$88;
             }
           if ($r88) return 0;
         });
    var bind$1194 =
      _f(function (t$1195, f$1196) {
           var match$1497 = _(repr$1101, [ t$1195 ])[0];
           switch ($t(match$1497))
           {
           case 0: return __(f$1196, [ match$1497[0] ]);
           case 1: return __(fail$1170, [ match$1497[0] ]);
           case 2:
             var sleeper$1199 = match$1497[0];
             var res$1200 = _(temp$1172, [ sleeper$1199[0] ]);
             _(add_immutable_waiter$1183,
               [
                 sleeper$1199,
                 _f(function (param$1496) {
                      switch ($t(param$1496))
                      {
                      case 0:
                        return __(connect$1153,
                                  [
                                    res$1200,
                                    function () {
                                      try {
                                        return _(f$1196, [ param$1496[0] ]);
                                      }
                                      catch (exn$1203) {
                                        return _(fail$1170, [ exn$1203 ]);
                                      }
                                    }()
                                  ]);
                      case 1: return __(fast_connect$1163, [ res$1200, $1(param$1496[0]) ]);
                      default: throw $(Assert_failure$26g, $("lwt.ml", 315, 20));
                      }
                    })
               ]);
             return res$1200;
           case 3: throw $(Assert_failure$26g, $("lwt.ml", 318, 8));
           default: return null;
           }
         });
    var $3E$3E$3D$1204 = _f(function (t$1205, f$1206) { return __(bind$1194, [ t$1205, f$1206 ]); });
    var $3D$3C$3C$1207 = _f(function (f$1208, t$1209) { return __(bind$1194, [ t$1209, f$1208 ]); });
    var map$1210 =
      _f(function (f$1211, t$1212) {
           var match$1494 = _(repr$1101, [ t$1212 ])[0];
           switch ($t(match$1494))
           {
           case 0: return __(return$1168, [ _(f$1211, [ match$1494[0] ]) ]);
           case 1: return __(fail$1170, [ match$1494[0] ]);
           case 2:
             var sleeper$1215 = match$1494[0];
             var res$1216 = _(temp$1172, [ sleeper$1215[0] ]);
             _(add_immutable_waiter$1183,
               [
                 sleeper$1215,
                 _f(function (param$1493) {
                      switch ($t(param$1493))
                      {
                      case 0:
                        return __(fast_connect$1163,
                                  [
                                    res$1216,
                                    function () {
                                      try { return $(_(f$1211, [ param$1493[0] ])); } catch (exn$1219) { return $1(exn$1219); }
                                    }()
                                  ]);
                      case 1: return __(fast_connect$1163, [ res$1216, $1(param$1493[0]) ]);
                      default: throw $(Assert_failure$26g, $("lwt.ml", 335, 20));
                      }
                    })
               ]);
             return res$1216;
           case 3: throw $(Assert_failure$26g, $("lwt.ml", 338, 8));
           default: return null;
           }
         });
    var $3E$7C$3D$1220 = _f(function (t$1221, f$1222) { return __(map$1210, [ f$1222, t$1221 ]); });
    var $3D$7C$3C$1223 = _f(function (f$1224, t$1225) { return __(map$1210, [ f$1224, t$1225 ]); });
    var catch$1226 =
      _f(function (x$1227, f$1228) {
           var t$1229 = function () { try { return _(x$1227, [ 0 ]); } catch (exn$1230) { return _(fail$1170, [ exn$1230 ]); } }();
           var match$1491 = _(repr$1101, [ t$1229 ])[0];
           switch ($t(match$1491))
           {
           case 0: return t$1229;
           case 1: return __(f$1228, [ match$1491[0] ]);
           case 2:
             var sleeper$1233 = match$1491[0];
             var res$1234 = _(temp$1172, [ sleeper$1233[0] ]);
             _(add_immutable_waiter$1183,
               [
                 sleeper$1233,
                 _f(function (state$1235) {
                      switch ($t(state$1235))
                      {
                      case 0: return __(fast_connect$1163, [ res$1234, state$1235 ]);
                      case 1:
                        return __(connect$1153,
                                  [
                                    res$1234,
                                    function () {
                                      try {
                                        return _(f$1228, [ state$1235[0] ]);
                                      }
                                      catch (exn$1237) {
                                        return _(fail$1170, [ exn$1237 ]);
                                      }
                                    }()
                                  ]);
                      default: throw $(Assert_failure$26g, $("lwt.ml", 356, 20));
                      }
                    })
               ]);
             return res$1234;
           case 3: throw $(Assert_failure$26g, $("lwt.ml", 359, 8));
           default: return null;
           }
         });
    var try_bind$1238 =
      _f(function (x$1239, f$1240, g$1241) {
           var t$1242 = function () { try { return _(x$1239, [ 0 ]); } catch (exn$1243) { return _(fail$1170, [ exn$1243 ]); } }();
           var match$1488 = _(repr$1101, [ t$1242 ])[0];
           switch ($t(match$1488))
           {
           case 0: return __(f$1240, [ match$1488[0] ]);
           case 1: return __(g$1241, [ match$1488[0] ]);
           case 2:
             var sleeper$1246 = match$1488[0];
             var res$1247 = _(temp$1172, [ sleeper$1246[0] ]);
             _(add_immutable_waiter$1183,
               [
                 sleeper$1246,
                 _f(function (param$1487) {
                      switch ($t(param$1487))
                      {
                      case 0:
                        return __(connect$1153,
                                  [
                                    res$1247,
                                    function () {
                                      try {
                                        return _(f$1240, [ param$1487[0] ]);
                                      }
                                      catch (exn$1250) {
                                        return _(fail$1170, [ exn$1250 ]);
                                      }
                                    }()
                                  ]);
                      case 1:
                        return __(connect$1153,
                                  [
                                    res$1247,
                                    function () {
                                      try {
                                        return _(g$1241, [ param$1487[0] ]);
                                      }
                                      catch (exn$1251) {
                                        return _(fail$1170, [ exn$1251 ]);
                                      }
                                    }()
                                  ]);
                      default: throw $(Assert_failure$26g, $("lwt.ml", 374, 20));
                      }
                    })
               ]);
             return res$1247;
           case 3: throw $(Assert_failure$26g, $("lwt.ml", 377, 8));
           default: return null;
           }
         });
    var poll$1252 =
      _f(function (t$1253) {
           var match$1484 = _(repr$1101, [ t$1253 ])[0];
           switch ($t(match$1484))
           {
           case 0: return $(match$1484[0]);
           case 1: throw match$1484[0];
           case 2: return 0;
           case 3: throw $(Assert_failure$26g, $("lwt.ml", 384, 16));
           default: return null;
           }
         });
    var ignore_result$1256 =
      _f(function (t$1257) {
           var match$1482 = _(repr$1101, [ t$1257 ])[0];
           switch ($t(match$1482))
           {
           case 0: return 0;
           case 1: throw match$1482[0];
           case 2:
             return __(add_immutable_waiter$1183,
                       [
                         match$1482[0],
                         _f(function (param$1480) {
                              switch ($t(param$1480))
                              {
                              case 0: return 0;
                              case 1: throw param$1480[0];
                              default: throw $(Assert_failure$26g, $("lwt.ml", 397, 20));
                              }
                            })
                       ]);
           case 3: throw $(Assert_failure$26g, $("lwt.ml", 399, 8));
           default: return null;
           }
         });
    var protected$1262 =
      _f(function (t$1263) {
           var match$1476 = _(repr$1101, [ t$1263 ])[0];
           switch ($t(match$1476))
           {
           case 0: return t$1263;
           case 1: return t$1263;
           case 2:
             var match$1475 = _(task$1177, [ 0 ]);
             var wakener$1266 = match$1475[1];
             _(add_immutable_waiter$1183,
               [
                 match$1476[0],
                 _f(function (state$1267) {
                      try {
                        switch ($t(state$1267))
                        {
                        case 0: return _(wakeup$1136, [ wakener$1266, state$1267[0] ]);
                        case 1: return _(wakeup_exn$1139, [ wakener$1266, state$1267[0] ]);
                        default: throw $(Assert_failure$26g, $("lwt.ml", 411, 25));
                        }
                      }
                      catch (exn$1473) {
                        if (exn$1473[0] === Invalid_argument$18g) return 0;
                        throw exn$1473;
                      }
                    })
               ]);
             return match$1475[0];
           case 3: throw $(Assert_failure$26g, $("lwt.ml", 418, 8));
           default: return null;
           }
         });
    var nth_ready$1270 =
      _f(function (l$1271, n$1272) {
           if (l$1271) {
             {
               var rem$1274 = l$1271[1];
               var x$1275 = _(repr$1101, [ l$1271[0] ]);
               var match$1471 = x$1275[0];
               switch ($t(match$1471))
               {
               case 2: return __(nth_ready$1270, [ rem$1274, n$1272 ]);
               default: if (n$1272 > 0) return __(nth_ready$1270, [ rem$1274, n$1272 - 1 ]); return x$1275;
               }
             }
           }
           throw $(Assert_failure$26g, $("lwt.ml", 423, 8));
         });
    var ready_count$1276 =
      _f(function (l$1277) {
           return __(oc$List$[12],
                     [
                       _f(function (acc$1278, x$1279) {
                            var match$1469 = _(repr$1101, [ x$1279 ])[0];
                            switch ($t(match$1469)) { case 2: return acc$1278; default: return acc$1278 + 1; }
                          }),
                       0,
                       l$1277
                     ]);
         });
    var remove_waiters$1280 =
      _f(function (l$1281) {
           return __(oc$List$[9],
                     [
                       _f(function (t$1282) {
                            var match$1468 = _(repr$1101, [ t$1282 ])[0];
                            switch ($t(match$1468))
                            {
                            case 2:
                              var sleeper$1283 = match$1468[0];
                              var removed$1284 = sleeper$1283[2] + 1;
                              if (removed$1284 > max_removed$1096) {
                                { sleeper$1283[2] = 0; return sleeper$1283[1] = _(cleanup$1149, [ sleeper$1283[1] ]); }
                              }
                              return sleeper$1283[2] = removed$1284;
                            default: return 0;
                            }
                          }),
                       l$1281
                     ]);
         });
    var choose$1285 =
      _f(function (l$1286) {
           var ready$1287 = _(ready_count$1276, [ l$1286 ]);
           if (ready$1287 > 0) return __(nth_ready$1270, [ l$1286, _(oc$Random$[4], [ ready$1287 ]) ]);
           var res$1288 = _(temp$1172, [ $(_f(function (param$1467) { return __(oc$List$[9], [ cancel$1142, l$1286 ]); })) ]);
           var waiter$1289 = $($(handle_result$1290));
           var handle_result$1290 =
             _f(function (state$1291) {
                  waiter$1289[0] = 0;
                  _(remove_waiters$1280, [ l$1286 ]);
                  return __(fast_connect$1163, [ res$1288, state$1291 ]);
                });
           waiter$1289[0][0] = handle_result$1290;
           _(oc$List$[9],
             [
               _f(function (t$1292) {
                    var match$1466 = _(repr$1101, [ t$1292 ])[0];
                    switch ($t(match$1466))
                    {
                    case 2: return __(add_removable_waiter$1186, [ match$1466[0], waiter$1289 ]);
                    default: throw $(Assert_failure$26g, $("lwt.ml", 474, 15));
                    }
                  }),
               l$1286
             ]);
           return res$1288;
         });
    var nchoose_terminate$1294 =
      _f(function (res$1295, acc$1296, param$1464) {
           if (param$1464) {
             {
               var l$1298 = param$1464[1];
               var state$1300 = _(repr$1101, [ param$1464[0] ])[0];
               switch ($t(state$1300))
               {
               case 0: return __(nchoose_terminate$1294, [ res$1295, $(state$1300[0], acc$1296), l$1298 ]);
               case 1: return __(fast_connect$1163, [ res$1295, state$1300 ]);
               default: return __(nchoose_terminate$1294, [ res$1295, acc$1296, l$1298 ]);
               }
             }
           }
           return __(fast_connect$1163, [ res$1295, $(_(oc$List$[4], [ acc$1296 ])) ]);
         });
    var nchoose_sleep$1301 =
      _f(function (l$1302) {
           var res$1303 = _(temp$1172, [ $(_f(function (param$1463) { return __(oc$List$[9], [ cancel$1142, l$1302 ]); })) ]);
           var waiter$1304 = $($(handle_result$1305));
           var handle_result$1305 =
             _f(function (state$1306) {
                  waiter$1304[0] = 0;
                  _(remove_waiters$1280, [ l$1302 ]);
                  return __(nchoose_terminate$1294, [ res$1303, 0, l$1302 ]);
                });
           waiter$1304[0][0] = handle_result$1305;
           _(oc$List$[9],
             [
               _f(function (t$1307) {
                    var match$1462 = _(repr$1101, [ t$1307 ])[0];
                    switch ($t(match$1462))
                    {
                    case 2: return __(add_removable_waiter$1186, [ match$1462[0], waiter$1304 ]);
                    default: throw $(Assert_failure$26g, $("lwt.ml", 505, 13));
                    }
                  }),
               l$1302
             ]);
           return res$1303;
         });
    var nchoose$1309 =
      _f(function (l$1310) {
           var init$1311 =
             _f(function (param$1458) {
                  if (param$1458) {
                    {
                      var l$1314 = param$1458[1];
                      var t$1315 = _(repr$1101, [ param$1458[0] ]);
                      var match$1459 = t$1315[0];
                      switch ($t(match$1459))
                      {
                      case 0: return __(collect$1312, [ $(match$1459[0], 0), l$1314 ]);
                      case 1: return __(fail$1170, [ match$1459[0] ]);
                      default: return __(init$1311, [ l$1314 ]);
                      }
                    }
                  }
                  return __(nchoose_sleep$1301, [ l$1310 ]);
                });
           var collect$1312 =
             _f(function (acc$1318, param$1460) {
                  if (param$1460) {
                    {
                      var l$1320 = param$1460[1];
                      var t$1321 = _(repr$1101, [ param$1460[0] ]);
                      var match$1461 = t$1321[0];
                      switch ($t(match$1461))
                      {
                      case 0: return __(collect$1312, [ $(match$1461[0], acc$1318), l$1320 ]);
                      case 1: return __(fail$1170, [ match$1461[0] ]);
                      default: return __(collect$1312, [ acc$1318, l$1320 ]);
                      }
                    }
                  }
                  return __(return$1168, [ _(oc$List$[4], [ acc$1318 ]) ]);
                });
           return __(init$1311, [ l$1310 ]);
         });
    var cancel_and_nth_ready$1324 =
      _f(function (l$1325, n$1326) {
           if (l$1325) {
             {
               var rem$1328 = l$1325[1];
               var x$1329 = _(repr$1101, [ l$1325[0] ]);
               var match$1456 = x$1329[0];
               switch ($t(match$1456))
               {
               case 2: _(cancel$1142, [ x$1329 ]); return __(nth_ready$1270, [ rem$1328, n$1326 ]);
               default:
                 if (n$1326 > 0) return __(nth_ready$1270, [ rem$1328, n$1326 - 1 ]);
                 _(oc$List$[9], [ cancel$1142, rem$1328 ]);
                 return x$1329;
               }
             }
           }
           throw $(Assert_failure$26g, $("lwt.ml", 541, 8));
         });
    var pick$1330 =
      _f(function (l$1331) {
           var ready$1332 = _(ready_count$1276, [ l$1331 ]);
           if (ready$1332 > 0) return __(cancel_and_nth_ready$1324, [ l$1331, _(oc$Random$[4], [ ready$1332 ]) ]);
           var res$1333 = _(temp$1172, [ $(_f(function (param$1455) { return __(oc$List$[9], [ cancel$1142, l$1331 ]); })) ]);
           var waiter$1334 = $($(handle_result$1335));
           var handle_result$1335 =
             _f(function (state$1336) {
                  waiter$1334[0] = 0;
                  _(remove_waiters$1280, [ l$1331 ]);
                  _(oc$List$[9], [ cancel$1142, l$1331 ]);
                  return __(fast_connect$1163, [ res$1333, state$1336 ]);
                });
           waiter$1334[0][0] = handle_result$1335;
           _(oc$List$[9],
             [
               _f(function (t$1337) {
                    var match$1454 = _(repr$1101, [ t$1337 ])[0];
                    switch ($t(match$1454))
                    {
                    case 2: return __(add_removable_waiter$1186, [ match$1454[0], waiter$1334 ]);
                    default: throw $(Assert_failure$26g, $("lwt.ml", 574, 15));
                    }
                  }),
               l$1331
             ]);
           return res$1333;
         });
    var npick_sleep$1339 =
      _f(function (l$1340) {
           var res$1341 = _(temp$1172, [ $(_f(function (param$1453) { return __(oc$List$[9], [ cancel$1142, l$1340 ]); })) ]);
           var waiter$1342 = $($(handle_result$1343));
           var handle_result$1343 =
             _f(function (state$1344) {
                  waiter$1342[0] = 0;
                  _(remove_waiters$1280, [ l$1340 ]);
                  _(oc$List$[9], [ cancel$1142, l$1340 ]);
                  return __(nchoose_terminate$1294, [ res$1341, 0, l$1340 ]);
                });
           waiter$1342[0][0] = handle_result$1343;
           _(oc$List$[9],
             [
               _f(function (t$1345) {
                    var match$1452 = _(repr$1101, [ t$1345 ])[0];
                    switch ($t(match$1452))
                    {
                    case 2: return __(add_removable_waiter$1186, [ match$1452[0], waiter$1342 ]);
                    default: throw $(Assert_failure$26g, $("lwt.ml", 594, 13));
                    }
                  }),
               l$1340
             ]);
           return res$1341;
         });
    var npick$1347 =
      _f(function (threads$1348) {
           var init$1349 =
             _f(function (param$1448) {
                  if (param$1448) {
                    {
                      var l$1352 = param$1448[1];
                      var t$1353 = _(repr$1101, [ param$1448[0] ]);
                      var match$1449 = t$1353[0];
                      switch ($t(match$1449))
                      {
                      case 0: return __(collect$1350, [ $(match$1449[0], 0), l$1352 ]);
                      case 1: _(oc$List$[9], [ cancel$1142, threads$1348 ]); return __(fail$1170, [ match$1449[0] ]);
                      default: return __(init$1349, [ l$1352 ]);
                      }
                    }
                  }
                  return __(npick_sleep$1339, [ threads$1348 ]);
                });
           var collect$1350 =
             _f(function (acc$1356, param$1450) {
                  if (param$1450) {
                    {
                      var l$1358 = param$1450[1];
                      var t$1359 = _(repr$1101, [ param$1450[0] ]);
                      var match$1451 = t$1359[0];
                      switch ($t(match$1451))
                      {
                      case 0: return __(collect$1350, [ $(match$1451[0], acc$1356), l$1358 ]);
                      case 1: _(oc$List$[9], [ cancel$1142, threads$1348 ]); return __(fail$1170, [ match$1451[0] ]);
                      default: return __(collect$1350, [ acc$1356, l$1358 ]);
                      }
                    }
                  }
                  _(oc$List$[9], [ cancel$1142, threads$1348 ]);
                  return __(return$1168, [ _(oc$List$[4], [ acc$1356 ]) ]);
                });
           return __(init$1349, [ threads$1348 ]);
         });
    var join$1362 =
      _f(function (l$1363) {
           var res$1364 = _(temp$1172, [ $(_f(function (param$1447) { return __(oc$List$[9], [ cancel$1142, l$1363 ]); })) ]);
           var sleeping$1365 = $(0);
           var waiter$1366 = $($(handle_result$1367));
           var handle_result$1367 =
             _f(function (state$1368) {
                  switch ($t(state$1368))
                  {
                  case 1:
                    waiter$1366[0] = 0;
                    _(remove_waiters$1280, [ l$1363 ]);
                    return __(fast_connect$1163, [ res$1364, state$1368 ]);
                  default:
                    sleeping$1365[0]--;
                    if (sleeping$1365[0] === 0) { { waiter$1366[0] = 0; return __(fast_connect$1163, [ res$1364, state$1368 ]); } }
                    return 0;
                  }
                });
           waiter$1366[0][0] = handle_result$1367;
           var init$1370 =
             _f(function (param$1442) {
                  if (param$1442) {
                    {
                      var rest$1372 = param$1442[1];
                      var t$1371 = param$1442[0];
                      var match$1446 = _(repr$1101, [ t$1371 ])[0];
                      switch ($t(match$1446))
                      {
                      case 1:
                        var loop$1375 =
                          _f(function (param$1443) {
                               if (param$1443) {
                                 {
                                   var l$1377 = param$1443[1];
                                   var t$1376 = param$1443[0];
                                   var match$1444 = _(repr$1101, [ t$1376 ])[0];
                                   switch ($t(match$1444))
                                   {
                                   case 1: return t$1376;
                                   case 2:
                                     var sleeper$1378 = match$1444[0];
                                     var removed$1379 = sleeper$1378[2] + 1;
                                     if (removed$1379 > max_removed$1096) {
                                       { sleeper$1378[2] = 0; sleeper$1378[1] = _(cleanup$1149, [ sleeper$1378[1] ]); }
                                     }
                                     else
                                       sleeper$1378[2] = removed$1379;
                                     return __(loop$1375, [ l$1377 ]);
                                   default: return __(loop$1375, [ l$1377 ]);
                                   }
                                 }
                               }
                               return t$1371;
                             });
                        waiter$1366[0] = 0;
                        return __(loop$1375, [ l$1363 ]);
                      case 2:
                        sleeping$1365[0]++;
                        _(add_removable_waiter$1186, [ match$1446[0], waiter$1366 ]);
                        return __(init$1370, [ rest$1372 ]);
                      default: return __(init$1370, [ rest$1372 ]);
                      }
                    }
                  }
                  if (sleeping$1365[0] === 0) return __(return$1168, [ 0 ]);
                  return res$1364;
                });
           return __(init$1370, [ l$1363 ]);
         });
    var $3C$3F$3E$1380 = _f(function (t1$1381, t2$1382) { return __(choose$1285, [ $(t1$1381, $(t2$1382, 0)) ]); });
    var $3C$26$3E$1383 = _f(function (t1$1384, t2$1385) { return __(join$1362, [ $(t1$1384, $(t2$1385, 0)) ]); });
    var finalize$1386 =
      _f(function (f$1387, g$1388) {
           return __(try_bind$1238,
                     [
                       f$1387,
                       _f(function (x$1389) {
                            return __($3E$3E$3D$1204,
                                      [ _(g$1388, [ 0 ]), _f(function (param$1440) { return __(return$1168, [ x$1389 ]); }) ]);
                          }),
                       _f(function (e$1390) {
                            return __($3E$3E$3D$1204,
                                      [ _(g$1388, [ 0 ]), _f(function (param$1441) { return __(fail$1170, [ e$1390 ]); }) ]);
                          })
                     ]);
         });
    var State$1401 = $();
    var state$1402 =
      _f(function (t$1403) {
           var match$1437 = _(repr$1101, [ t$1403 ])[0];
           switch ($t(match$1437))
           {
           case 0: return $(match$1437[0]);
           case 1: return $1(match$1437[0]);
           case 2: return 0;
           case 3: throw $(Assert_failure$26g, $("lwt.ml", 708, 14));
           default: return null;
           }
         });
    var paused$1410 = _(oc$Lwt_sequence$[3], [ 0 ]);
    var pause$1411 =
      _f(function (param$1434) {
           var match$1436 = _(task$1177, [ 0 ]);
           var waiter$1412 = match$1436[0];
           var node$1414 = _(oc$Lwt_sequence$[6], [ match$1436[1], paused$1410 ]);
           _(on_cancel$1189, [ waiter$1412, _f(function (param$1435) { return __(oc$Lwt_sequence$[2], [ node$1414 ]); }) ]);
           return waiter$1412;
         });
    var wakeup_paused$1415 =
      _f(function (param$1432) {
           var match$1433 = _(oc$Lwt_sequence$[10], [ paused$1410 ]);
           if (match$1433) { { _(wakeup$1136, [ match$1433[0], 0 ]); return __(wakeup_paused$1415, [ 0 ]); } }
           return 0;
         });
    return $(return$1168, fail$1170, bind$1194, $3E$3E$3D$1204, $3D$3C$3C$1207, map$1210, $3E$7C$3D$1220, $3D$7C$3C$1223,
             catch$1226, try_bind$1238, finalize$1386, choose$1285, nchoose$1309, join$1362, $3C$3F$3E$1380, $3C$26$3E$1383,
             ignore_result$1256, wait$1175, wakeup$1136, wakeup_exn$1139, state$1402, Canceled$1030, task$1177, on_cancel$1189,
             cancel$1142, pick$1330, npick$1347, protected$1262, pause$1411, wakeup_paused$1415, poll$1252, apply$1179);
  }();
var oc$Lwt_dom$ =
  function () {
    var sleep$1030 =
      _f(function (ms$1031) {
           var match$1062 = _(oc$Lwt$[22], [ 0 ]);
           var t$1032 = match$1062[0];
           var timeout$1034 = _f(function (param$1061) { return __(oc$Lwt$[18], [ match$1062[1], 0 ]); });
           var id$1035 =
             function () { var v$1064 = oc$Dom$[0]; return _m(v$1064.setTimeout, v$1064, [ timeout$1034, ms$1031 * 1000. ]); }();
           _(oc$Lwt$[23],
             [
               t$1032,
               _f(function (param$1060) {
                    return function () { var v$1063 = oc$Dom$[0]; return __m(v$1063.clearTimeout, v$1063, [ id$1035 ]); }();
                  })
             ]);
           return t$1032;
         });
    var yield$1036 = _f(function (param$1059) { return __(sleep$1030, [ 0. ]); });
    var http_request$1037 =
      _f(function ($2Aopt$2A$1038, meth$1041, url$1042) {
           var headers$1039 = $2Aopt$2A$1038 ? $2Aopt$2A$1038[0] : 0;
           var match$1058 = _(oc$Lwt$[22], [ 0 ]);
           var t$1043 = match$1058[0];
           var match$1057 = typeof meth$1041 == 'number' ? $("GET", 0) : $("POST", $(meth$1041[1]));
           var body$1046 = match$1057[1];
           var r$1048 = new XMLHttpRequest(0);
           _m(r$1048.open, r$1048, [ match$1057[0], url$1042, true ]);
           _(oc$List$[9],
             [
               _f(function (param$1056) { return __m(r$1048.setRequestHeader, r$1048, [ param$1056[0], param$1056[1] ]); }),
               headers$1039
             ]);
           var fired$1051 = $(false);
           var onreadystatechange$1052 =
             _f(function (param$1055) {
                  if (r$1048.readyState === 4) {
                    { if (!fired$1051[0]) _(oc$Lwt$[18], [ match$1058[1], r$1048 ]); else; return fired$1051[0] = true; }
                  }
                  return 0;
                });
           r$1048.onreadystatechange = onreadystatechange$1052;
           _m(r$1048.send, r$1048, [ body$1046 ? body$1046[0] : null ]);
           _(oc$Lwt$[23], [ t$1043, _f(function (param$1054) { return __m(r$1048.abort, r$1048, [  ]); }) ]);
           return t$1043;
         });
    return $(sleep$1030, yield$1036, http_request$1037);
  }();
var oc$Planet$ =
  function () {
    var width$1030 = 600;
    var pi$1032 = 4. * Math.atan(1.);
    var obliquity$1033 = 23.5 * pi$1032 / 180.;
    var gamma$1034 = 2.;
    var dark$1035 = Math.pow(0.2, gamma$1034);
    var doc$1036 = oc$Dom$[1];
    var button$1037 =
      _f(function (txt$1038, action$1039) {
           var b$1040 = _m(doc$1036.createElement, doc$1036, [ "input" ]);
           b$1040.type = "button";
           b$1040.value = txt$1038;
           b$1040.onclick = _f(function (param$1579) { _(action$1039, [ 0 ]); return true; });
           return b$1040;
         });
    var toggle_button$1041 =
      _f(function (txt1$1042, txt2$1043, action$1044) {
           var state$1045 = $(false);
           var b$1046 = _m(doc$1036.createElement, doc$1036, [ "input" ]);
           b$1046.type = "button";
           b$1046.value = txt1$1042;
           b$1046.onclick =
             _f(function (param$1578) {
                  state$1045[0] = !state$1045[0];
                  b$1046.value = state$1045[0] ? txt2$1043 : txt1$1042;
                  _(action$1044, [ state$1045[0] ]);
                  return true;
                });
           return b$1046;
         });
    var checkbox$1047 =
      _f(function (txt$1048, checked$1049, action$1050) {
           var b$1051 = _m(doc$1036.createElement, doc$1036, [ "input" ]);
           b$1051.type = "checkbox";
           b$1051.checked = checked$1049;
           b$1051.onclick = _f(function (param$1577) { _(action$1050, [ b$1051.checked ]); return true; });
           var lab$1052 = _m(doc$1036.createElement, doc$1036, [ "label" ]);
           _m(lab$1052.appendChild, lab$1052, [ b$1051 ]);
           _m(lab$1052.appendChild, lab$1052, [ _m(doc$1036.createTextNode, doc$1036, [ txt$1048 ]) ]);
           return lab$1052;
         });
    var radio$1053 =
      _f(function (txt$1054, name$1055, checked$1056, action$1057) {
           var b$1058 = _m(doc$1036.createElement, doc$1036, [ "input" ]);
           b$1058.name = name$1055;
           b$1058.type = "radio";
           b$1058.checked = checked$1056;
           b$1058.onclick = _f(function (param$1576) { _(action$1057, [ 0 ]); return true; });
           var lab$1059 = _m(doc$1036.createElement, doc$1036, [ "label" ]);
           _m(lab$1059.appendChild, lab$1059, [ b$1058 ]);
           _m(lab$1059.appendChild, lab$1059, [ _m(doc$1036.createTextNode, doc$1036, [ txt$1054 ]) ]);
           return lab$1059;
         });
    var vertex$1067 = _f(function (x$1068, y$1069, z$1070) { return $(x$1068, y$1069, z$1070); });
    var vect$1078 =
      _f(function (param$1572, param$1573) {
           return $(param$1573[0] - param$1572[0], param$1573[1] - param$1572[1], param$1573[2] - param$1572[2]);
         });
    var cross_product$1085 =
      _f(function (param$1569, param$1570) {
           var z2$1091 = param$1570[2];
           var y2$1090 = param$1570[1];
           var x2$1089 = param$1570[0];
           var z1$1088 = param$1569[2];
           var y1$1087 = param$1569[1];
           var x1$1086 = param$1569[0];
           return $(y1$1087 * z2$1091 - y2$1090 * z1$1088, z1$1088 * x2$1089 - z2$1091 * x1$1086,
                    x1$1086 * y2$1090 - x2$1089 * y1$1087);
         });
    var dot_product$1092 =
      _f(function (param$1567, param$1568) {
           return param$1567[0] * param$1568[0] + param$1567[1] * param$1568[1] + param$1567[2] * param$1568[2];
         });
    var matrix_vect_mul$1099 =
      _f(function (m$1100, param$1565) {
           var r3$1106 = m$1100[2];
           var r2$1105 = m$1100[1];
           var r1$1104 = m$1100[0];
           var z$1103 = param$1565[2];
           var y$1102 = param$1565[1];
           var x$1101 = param$1565[0];
           var x$27$1107 = x$1101 * r1$1104[0] + y$1102 * r1$1104[1] + z$1103 * r1$1104[2];
           var y$27$1108 = x$1101 * r2$1105[0] + y$1102 * r2$1105[1] + z$1103 * r2$1105[2];
           var z$27$1109 = x$1101 * r3$1106[0] + y$1102 * r3$1106[1] + z$1103 * r3$1106[2];
           return $(x$27$1107, y$27$1108, z$27$1109);
         });
    var matrix_transp$1110 =
      _f(function (m$1111) {
           var r3$1114 = m$1111[2];
           var r2$1113 = m$1111[1];
           var r1$1112 = m$1111[0];
           return $($(r1$1112[0], r2$1113[0], r3$1114[0]), $(r1$1112[1], r2$1113[1], r3$1114[1]),
                    $(r1$1112[2], r2$1113[2], r3$1114[2]));
         });
    var matrix_mul$1115 =
      _f(function (m$1116, m$27$1117) {
           var m$27$1118 = _(matrix_transp$1110, [ m$27$1117 ]);
           return $(_(matrix_vect_mul$1099, [ m$27$1118, m$1116[0] ]), 
                    _(matrix_vect_mul$1099, [ m$27$1118, m$1116[1] ]), 
                    _(matrix_vect_mul$1099, [ m$27$1118, m$1116[2] ]));
         });
    var normalize$1119 =
      _f(function (v$1120) {
           var z$1123 = v$1120[2];
           var y$1122 = v$1120[1];
           var x$1121 = v$1120[0];
           var r$1124 = Math.sqrt(x$1121 * x$1121 + y$1122 * y$1122 + z$1123 * z$1123);
           return $(x$1121 / r$1124, y$1122 / r$1124, z$1123 / r$1124);
         });
    var xz_rotation$1125 =
      _f(function (phi$1126) {
           var cos_phi$1127 = Math.cos(phi$1126);
           var sin_phi$1128 = Math.sin(phi$1126);
           return $(_(vertex$1067, [ cos_phi$1127, 0., sin_phi$1128 ]), 
                    _(vertex$1067, [ 0., 1., 0. ]), _(vertex$1067, [ -sin_phi$1128, 0., cos_phi$1127 ]));
         });
    var xy_rotation$1129 =
      _f(function (phi$1130) {
           var cos_phi$1131 = Math.cos(phi$1130);
           var sin_phi$1132 = Math.sin(phi$1130);
           return $(_(vertex$1067, [ cos_phi$1131, sin_phi$1132, 0. ]), 
                    _(vertex$1067, [ -sin_phi$1132, cos_phi$1131, 0. ]), 
                    _(vertex$1067, [ 0., 0., 1. ]));
         });
    var yz_rotation$1133 =
      _f(function (phi$1134) {
           var cos_phi$1135 = Math.cos(phi$1134);
           var sin_phi$1136 = Math.sin(phi$1134);
           return $(_(vertex$1067, [ 1., 0., 0. ]), _(vertex$1067, [ 0., cos_phi$1135, sin_phi$1136 ]),
                    _(vertex$1067, [ 0., -sin_phi$1136, cos_phi$1135 ]));
         });
    var matrix_identity$1137 = _(xz_rotation$1125, [ 0. ]);
    var rotate_normal$1138 =
      _f(function (m$1139, v$1140) { return __(matrix_vect_mul$1099, [ _(matrix_transp$1110, [ m$1139 ]), v$1140 ]); });
    var face$1148 = _f(function (v1$1149, v2$1150, v3$1151) { return $(v1$1149, v2$1150, v3$1151); });
    var rotate_object$1157 =
      _f(function (m$1158, o$1159) {
           return $(_(oc$Array$[12], [ _f(function (v$1160) { return __(matrix_vect_mul$1099, [ m$1158, v$1160 ]); }), o$1159[0] ]),
                    o$1159[1]);
         });
    var octahedron$1161 =
      $($(_(vertex$1067, [ 0., 0., 1. ]), _(vertex$1067, [ 1., 0., 0. ]), 
          _(vertex$1067, [ 0., 1., 0. ]), _(vertex$1067, [ -1., 0., 0. ]), 
          _(vertex$1067, [ 0., -1., 0. ]), _(vertex$1067, [ 0., 0., -1. ])),
        $(_(face$1148, [ 0, 1, 2 ]), _(face$1148, [ 0, 2, 3 ]), _(face$1148, [ 0, 3, 4 ]), 
          _(face$1148, [ 0, 4, 1 ]), _(face$1148, [ 1, 5, 2 ]), _(face$1148, [ 1, 4, 5 ]), 
          _(face$1148, [ 3, 5, 4 ]), _(face$1148, [ 3, 2, 5 ])));
    var tesselate_sphere$1162 =
      _f(function (p_div$1163, t_div$1164) {
           var p_delta$1165 = 2. * pi$1032 / p_div$1163;
           var t_delta$1166 = pi$1032 / t_div$1164;
           var t_offset$1167 = (pi$1032 - t_delta$1166) / 2.;
           var n$1168 = t_div$1164 * p_div$1163;
           var vertices$1169 = caml_make_vect(n$1168 + 2, _(vertex$1067, [ 0., 0., 0. ]));
           var faces$1170 = caml_make_vect(n$1168 * 2, _(face$1148, [ 0, 0, 0 ]));
           var south$1172 = n$1168 + 1;
           oc$$asets(vertices$1169, n$1168, _(vertex$1067, [ 0., -1., 0. ]));
           oc$$asets(vertices$1169, south$1172, _(vertex$1067, [ 0., 1., 0. ]));
           for (var i$1173 = 0; i$1173 <= p_div$1163 - 1; i$1173++) {
             (function (i$1173) {
                for (var j$1174 = 0; j$1174 <= t_div$1164 - 1; j$1174++) {
                  (function (j$1174) {
                     var phi$1175 = i$1173 * p_delta$1165;
                     var theta$1176 = j$1174 * t_delta$1166 - t_offset$1167;
                     var x$1177 = Math.cos(phi$1175) * Math.cos(theta$1176);
                     var y$1178 = Math.sin(theta$1176);
                     var z$1179 = Math.sin(phi$1175) * Math.cos(theta$1176);
                     var k$1180 = i$1173 * t_div$1164 + j$1174;
                     oc$$asets(vertices$1169, k$1180, _(vertex$1067, [ x$1177, y$1178, z$1179 ]));
                     if (j$1174 === 0) {
                       {
                         oc$$asets(faces$1170, 2 * k$1180, _(face$1148, [ n$1168, k$1180, (k$1180 + t_div$1164) % n$1168 ]));
                         oc$$asets(faces$1170, 2 * k$1180 + 1,
                                   _(face$1148, [ south$1172, (k$1180 + 2 * t_div$1164 - 1) % n$1168, k$1180 + t_div$1164 - 1 ]));
                       }
                     }
                     else {
                       {
                         oc$$asets(faces$1170, 2 * k$1180, _(face$1148, [ k$1180, (k$1180 + t_div$1164) % n$1168, k$1180 - 1 ]));
                         oc$$asets(faces$1170, 2 * k$1180 + 1,
                                   _(face$1148, [ k$1180 - 1, (k$1180 + t_div$1164) % n$1168, (k$1180 + t_div$1164 - 1) % n$1168 ]));
                       }
                     }
                   }(j$1174));
                }
              }(i$1173));
           }
           return $(vertices$1169, faces$1170);
         });
    var divide$1181 =
      _f(function (all$1182, o$1183) {
           var vn$1184 = all$1182 ? (o$1183[0]).length + ((o$1183[1]).length * 3 / 2 >> 0) : (o$1183[0]).length + 16;
           var vertices$1185 = caml_make_vect(vn$1184, _(vertex$1067, [ 0., 0., 0. ]));
           var j$1186 = $((o$1183[0]).length);
           _(oc$Array$[8], [ o$1183[0], 0, vertices$1185, 0, j$1186[0] ]);
           var fn$1187 = all$1182 ? 4 * (o$1183[1]).length : (o$1183[1]).length + 24;
           var faces$1188 = caml_make_vect(fn$1187, _(face$1148, [ 0, 0, 0 ]));
           var midpoints$1189 = _(oc$Hashtbl$[0], [ 17 ]);
           var midpoint$1207 =
             _f(function (v1$1208, v2$1209) {
                  var p$1210 = v1$1208 < v2$1209 ? $(v1$1208, v2$1209) : $(v2$1209, v1$1208);
                  try {
                    return _(oc$Hashtbl$[4], [ midpoints$1189, p$1210 ]);
                  }
                  catch (exn$1550) {
                    if (exn$1550[0] === Not_found$20g) {
                      {
                        var v1$1211 = oc$$arefs(o$1183[0], v1$1208);
                        var v2$1212 = oc$$arefs(o$1183[0], v2$1209);
                        var v$1213 =
                          $((v1$1211[0] + v2$1212[0]) / 2., (v1$1211[1] + v2$1212[1]) / 2., (v1$1211[2] + v2$1212[2]) / 2.);
                        var v$1214 =
                          all$1182 || (Math.abs(v1$1211[1]) === 1. || Math.abs(v2$1212[1]) === 1.) ?
                            _(normalize$1119, [ v$1213 ]) :
                            v$1213;
                        var res$1215 = j$1186[0];
                        if (res$1215 < vertices$1185.length); else throw $(Assert_failure$26g, $("planet.ml", 270, 0));
                        oc$$asets(vertices$1185, res$1215, v$1214);
                        _(oc$Hashtbl$[2], [ midpoints$1189, p$1210, res$1215 ]);
                        j$1186[0]++;
                        return res$1215;
                      }
                    }
                    throw exn$1550;
                  }
                });
           var k$1216 = 0;
           for (var i$1217 = 0; i$1217 <= (o$1183[1]).length - 1; i$1217++) {
             (function (i$1217) {
                var match$1549 = oc$$arefs(o$1183[1], i$1217);
                var v3$1220 = match$1549[2];
                var v2$1219 = match$1549[1];
                var v1$1218 = match$1549[0];
                if (all$1182 ||
                      (Math.abs(oc$$arefs(o$1183[0], v1$1218)[1]) === 1. ||
                         (Math.abs(oc$$arefs(o$1183[0], v2$1219)[1]) === 1. || Math.abs(oc$$arefs(o$1183[0], v3$1220)[1]) === 1.))) {
                  {
                    var w1$1221 = _(midpoint$1207, [ v1$1218, v2$1219 ]);
                    var w2$1222 = _(midpoint$1207, [ v2$1219, v3$1220 ]);
                    var w3$1223 = _(midpoint$1207, [ v3$1220, v1$1218 ]);
                    oc$$asets(faces$1188, k$1216, $(v1$1218, w1$1221, w3$1223));
                    oc$$asets(faces$1188, k$1216 + 1, $(w1$1221, v2$1219, w2$1222));
                    oc$$asets(faces$1188, k$1216 + 2, $(w3$1223, w2$1222, v3$1220));
                    oc$$asets(faces$1188, k$1216 + 3, $(w1$1221, w2$1222, w3$1223));
                    k$1216 = k$1216 + 4;
                  }
                }
                else {
                  { oc$$asets(faces$1188, k$1216, oc$$arefs(o$1183[1], i$1217)); k$1216 = 1 + k$1216; }
                }
              }(i$1217));
           }
           if (j$1186[0] === vertices$1185.length); else throw $(Assert_failure$26g, $("planet.ml", 296, 2));
           if (k$1216 === faces$1188.length); else throw $(Assert_failure$26g, $("planet.ml", 297, 2));
           return $(vertices$1185, faces$1188);
         });
    var create_canvas$1224 =
      _f(function (w$1225, h$1226) {
           var c$1227 = _m(doc$1036.createElement, doc$1036, [ "canvas" ]);
           c$1227.width = w$1225;
           c$1227.height = h$1226;
           return c$1227;
         });
    var $3E$3E$3D$1228 = oc$Lwt$[2];
    var lwt_wrap$1229 =
      _f(function (f$1230) {
           var match$1543 = _(oc$Lwt$[22], [ 0 ]);
           var cont$1233 = _f(function (x$1234) { return __(oc$Lwt$[18], [ match$1543[1], x$1234 ]); });
           _(f$1230, [ cont$1233 ]);
           return match$1543[0];
         });
    var load_image$1235 =
      _f(function (src$1236) {
           var img$1237 = _m(doc$1036.createElement, doc$1036, [ "img" ]);
           return __($3E$3E$3D$1228,
                     [
                       _(lwt_wrap$1229, [ _f(function (c$1238) { img$1237.onload = c$1238; return img$1237.src = src$1236; }) ]),
                       _f(function (param$1542) { return __(oc$Lwt$[0], [ img$1237 ]); })
                     ]);
         });
    var shadow$1239 =
      _f(function (texture$1240) {
           var w$1241 = texture$1240.width;
           var h$1242 = texture$1240.height;
           var canvas$1243 = _(create_canvas$1224, [ w$1241, h$1242 ]);
           var ctx$1244 = _m(canvas$1243.getContext, canvas$1243, [ "2d" ]);
           var match$1541 = $(w$1241 / 8 >> 0, h$1242 / 8 >> 0);
           var h$1246 = match$1541[1];
           var w$1245 = match$1541[0];
           var img$1247 = _m(ctx$1244.getImageData, ctx$1244, [ 0., 0., w$1245, h$1246 ]);
           var data$1248 = img$1247.data;
           var inv_gamma$1249 = 1. / gamma$1034;
           var update_shadow$1250 =
             _f(function (obliquity$1251) {
                  var cos_obl$1252 = Math.cos(obliquity$1251);
                  var sin_obl$1253 = -Math.sin(obliquity$1251);
                  for (var j$1254 = 0; j$1254 <= h$1246 - 1; j$1254++) {
                    (function (j$1254) {
                       for (var i$1255 = 0; i$1255 <= (w$1245 / 2 >> 0) - 1; i$1255++) {
                         (function (i$1255) {
                            var k$1256 = 4. * (i$1255 + j$1254 * w$1245) >> 0;
                            var k$27$1257 = 4. * (w$1245 - i$1255 + j$1254 * w$1245 - 1.) >> 0;
                            var theta$1258 = (j$1254 / h$1246 - 0.5) * pi$1032;
                            var phi$1259 = i$1255 / w$1245 * 2. * pi$1032;
                            var x$1260 = Math.cos(phi$1259) * Math.cos(theta$1258);
                            var y$1261 = Math.sin(theta$1258);
                            var match$1540 =
                              $(x$1260 * cos_obl$1252 + y$1261 * sin_obl$1253, -x$1260 * sin_obl$1253 + y$1261 * cos_obl$1252);
                            var x$1262 = match$1540[0];
                            var c$1264 = x$1262 > 0. ? dark$1035 : dark$1035 - x$1262 * (1. - dark$1035) * 1.2;
                            var c$1265 = c$1264 <= 1. ? c$1264 : 1.;
                            var c$1266 = 255 - (255.99 * Math.pow(c$1265, inv_gamma$1249) >> 0);
                            oc$$asets(data$1248, k$1256 + 3, c$1266);
                            oc$$asets(data$1248, k$27$1257 + 3, c$1266);
                          }(i$1255));
                       }
                     }(j$1254));
                  }
                  _m(ctx$1244.putImageData, ctx$1244, [ img$1247, 0., 0. ]);
                  ctx$1244.globalCompositeOperation = "copy";
                  _m(ctx$1244.save, ctx$1244, [  ]);
                  _m(ctx$1244.scale, ctx$1244, [ 8. * (w$1245 + 2) / w$1245, 8. * (h$1246 + 2) / h$1246 ]);
                  _m(ctx$1244.translate, ctx$1244, [ -1., -1. ]);
                  _m(ctx$1244.drawImage, ctx$1244, [ canvas$1243, 0., 0. ]);
                  return __m(ctx$1244.restore, ctx$1244, [  ]);
                });
           _(update_shadow$1250, [ obliquity$1033 ]);
           var w$1267 = texture$1240.width;
           var h$1268 = texture$1240.height;
           var canvas$27$1269 = _(create_canvas$1224, [ w$1267, h$1268 ]);
           var ctx$27$1270 = _m(canvas$27$1269.getContext, canvas$27$1269, [ "2d" ]);
           var no_lighting$1271 = $(false);
           var update_texture$1272 =
             _f(function (lighting$1273, phi$1274) {
                  if (lighting$1273) {
                    {
                      no_lighting$1271[0] = false;
                      var phi$1275 = phi$1274 % (2. * pi$1032);
                      _m(ctx$27$1270.drawImage, ctx$27$1270, [ texture$1240, 0., 0. ]);
                      var i$1276 = (2. * pi$1032 - phi$1275) * w$1267 / 2. / pi$1032 % w$1267 >> 0;
                      _m(ctx$27$1270.drawImage, ctx$27$1270, [ canvas$1243, i$1276, 0. ]);
                      return __m(ctx$27$1270.drawImage, ctx$27$1270, [ canvas$1243, i$1276 - w$1267, 0. ]);
                    }
                  }
                  if (!no_lighting$1271[0]) {
                    { _m(ctx$27$1270.drawImage, ctx$27$1270, [ texture$1240, 0., 0. ]); return no_lighting$1271[0] = true; }
                  }
                  return 0;
                });
           return $(canvas$27$1269, update_shadow$1250, update_texture$1272);
         });
    var to_uv$1277 =
      _f(function (tw$1278, th$1279, param$1539) {
           var cst1$1283 = (tw$1278 / 2. - 0.99) / pi$1032;
           var cst2$1284 = th$1279 / 2.;
           var cst3$1285 = (th$1279 - 0.99) / pi$1032;
           var u$1286 = (tw$1278 - Math.atan2(param$1539[2], param$1539[0]) * cst1$1283 >> 0) % tw$1278;
           var v$1287 = cst2$1284 + Math.asin(param$1539[1]) * cst3$1285 >> 0;
           if (0. <= u$1286); else throw $(Assert_failure$26g, $("planet.ml", 413, 0));
           if (u$1286 < tw$1278); else throw $(Assert_failure$26g, $("planet.ml", 414, 0));
           if (0. <= v$1287); else throw $(Assert_failure$26g, $("planet.ml", 415, 0));
           if (v$1287 < th$1279); else throw $(Assert_failure$26g, $("planet.ml", 416, 0));
           return $(u$1286, v$1287);
         });
    var min$1288 = _f(function (u$1289, v$1290) { if (u$1289 < v$1290) return u$1289; return v$1290; });
    var max$1291 = _f(function (u$1292, v$1293) { if (u$1292 < v$1293) return v$1293; return u$1292; });
    var precompute_mapping_info$1294 =
      _f(function (tw$1295, th$1296, uv$1297, f$1298) {
           var match$1538 = oc$$arefs(uv$1297, f$1298[0]);
           var v1$1303 = match$1538[1];
           var u1$1302 = match$1538[0];
           var match$1537 = oc$$arefs(uv$1297, f$1298[1]);
           var v2$1305 = match$1537[1];
           var u2$1304 = match$1537[0];
           var match$1536 = oc$$arefs(uv$1297, f$1298[2]);
           var v3$1307 = match$1536[1];
           var u3$1306 = match$1536[0];
           var mid$1308 = tw$1295 / 2.;
           var u1$1309 = u1$1302 === 0. && (u2$1304 > mid$1308 || u3$1306 > mid$1308) ? tw$1295 - 2. : u1$1302;
           var u2$1310 = u2$1304 === 0. && (u1$1309 > mid$1308 || u3$1306 > mid$1308) ? tw$1295 - 2. : u2$1304;
           var u3$1311 = u3$1306 === 0. && (u2$1310 > mid$1308 || u1$1309 > mid$1308) ? tw$1295 - 2. : u3$1306;
           var mth$1312 = th$1296 - 2.;
           var u1$1313 = v1$1303 === 0. || v1$1303 >= mth$1312 ? (u2$1310 + u3$1311) / 2. : u1$1309;
           var u2$1314 = v2$1305 === 0. || v2$1305 >= mth$1312 ? (u1$1313 + u3$1311) / 2. : u2$1310;
           var u3$1315 = v3$1307 === 0. || v3$1307 >= mth$1312 ? (u2$1314 + u1$1313) / 2. : u3$1311;
           var u1$1316 = _(max$1291, [ 1., u1$1313 ]);
           var u2$1317 = _(max$1291, [ 1., u2$1314 ]);
           var u3$1318 = _(max$1291, [ 1., u3$1315 ]);
           var v1$1319 = _(max$1291, [ 1., v1$1303 ]);
           var v2$1320 = _(max$1291, [ 1., v2$1305 ]);
           var v3$1321 = _(max$1291, [ 1., v3$1307 ]);
           var du2$1322 = u2$1317 - u1$1316;
           var du3$1323 = u3$1318 - u1$1316;
           var dv2$1324 = v2$1320 - v1$1319;
           var dv3$1325 = v3$1321 - v1$1319;
           var su$1326 = dv2$1324 * du3$1323 - dv3$1325 * du2$1322;
           var sv$1327 = du2$1322 * dv3$1325 - du3$1323 * dv2$1324;
           var dv3$1328 = dv3$1325 / sv$1327;
           var dv2$1329 = dv2$1324 / sv$1327;
           var du3$1330 = du3$1323 / su$1326;
           var du2$1331 = du2$1322 / su$1326;
           var u$1332 = _(max$1291, [ 0., _(min$1288, [ u1$1316, _(min$1288, [ u2$1317, u3$1318 ]) ]) - 4. ]);
           var v$1333 = _(max$1291, [ 0., _(min$1288, [ v1$1319, _(min$1288, [ v2$1320, v3$1321 ]) ]) - 4. ]);
           var u$27$1334 = _(min$1288, [ tw$1295, _(max$1291, [ u1$1316, _(max$1291, [ u2$1317, u3$1318 ]) ]) + 4. ]);
           var v$27$1335 = _(min$1288, [ th$1296, _(max$1291, [ v1$1319, _(max$1291, [ v2$1320, v3$1321 ]) ]) + 4. ]);
           var du$1336 = u$27$1334 - u$1332;
           var dv$1337 = v$27$1335 - v$1333;
           return $(u1$1316, v1$1319, du2$1331, dv2$1329, du3$1330, dv3$1328, u$1332, v$1333, du$1336, dv$1337);
         });
    var draw$1338 =
      _f(function (ctx$1339, img$1340, shd$1341, o$1342, uv$1343, normals$1344, face_info$1345, dir$1346) {
           return __(oc$Array$[13],
                     [
                       _f(function (i$1347, param$1531) {
                            var match$1535 = oc$$arefs(o$1342[0], param$1531[0]);
                            var y1$1352 = match$1535[1];
                            var x1$1351 = match$1535[0];
                            var match$1534 = oc$$arefs(o$1342[0], param$1531[1]);
                            var y2$1355 = match$1534[1];
                            var x2$1354 = match$1534[0];
                            var match$1533 = oc$$arefs(o$1342[0], param$1531[2]);
                            var y3$1358 = match$1533[1];
                            var x3$1357 = match$1533[0];
                            if (_(dot_product$1092, [ oc$$arefs(normals$1344, i$1347), dir$1346 ]) >= 0.) {
                              {
                                _m(ctx$1339.beginPath, ctx$1339, [  ]);
                                _m(ctx$1339.moveTo, ctx$1339, [ x1$1351, y1$1352 ]);
                                _m(ctx$1339.lineTo, ctx$1339, [ x2$1354, y2$1355 ]);
                                _m(ctx$1339.lineTo, ctx$1339, [ x3$1357, y3$1358 ]);
                                _m(ctx$1339.closePath, ctx$1339, [  ]);
                                _m(ctx$1339.save, ctx$1339, [  ]);
                                _m(ctx$1339.clip, ctx$1339, [  ]);
                                var match$1532 = oc$$arefs(face_info$1345, i$1347);
                                var dv$1369 = match$1532[9];
                                var du$1368 = match$1532[8];
                                var v$1367 = match$1532[7];
                                var u$1366 = match$1532[6];
                                var dv3$1365 = match$1532[5];
                                var du3$1364 = match$1532[4];
                                var dv2$1363 = match$1532[3];
                                var du2$1362 = match$1532[2];
                                var v1$1361 = match$1532[1];
                                var u1$1360 = match$1532[0];
                                var dx2$1370 = x2$1354 - x1$1351;
                                var dx3$1371 = x3$1357 - x1$1351;
                                var dy2$1372 = y2$1355 - y1$1352;
                                var dy3$1373 = y3$1358 - y1$1352;
                                var a$1374 = dx2$1370 * dv3$1365 - dx3$1371 * dv2$1363;
                                var b$1375 = dx2$1370 * du3$1364 - dx3$1371 * du2$1362;
                                var c$1376 = x1$1351 - a$1374 * u1$1360 - b$1375 * v1$1361;
                                var d$1377 = dy2$1372 * dv3$1365 - dy3$1373 * dv2$1363;
                                var e$1378 = dy2$1372 * du3$1364 - dy3$1373 * du2$1362;
                                var f$1379 = y1$1352 - d$1377 * u1$1360 - e$1378 * v1$1361;
                                _m(ctx$1339.transform, ctx$1339, [ a$1374, d$1377, b$1375, e$1378, c$1376, f$1379 ]);
                                _m(ctx$1339.drawImage, ctx$1339,
                                   [ shd$1341, u$1366, v$1367, du$1368, dv$1369, u$1366, v$1367, du$1368, dv$1369 ]);
                                return __m(ctx$1339.restore, ctx$1339, [  ]);
                              }
                            }
                            return 0;
                          }),
                       o$1342[1]
                     ]);
         });
    var $3E$3E$1380 = _f(function (x$1381, f$1382) { return __(f$1382, [ x$1381 ]); });
    var o$1383 = _(tesselate_sphere$1162, [ 12, 8 ]);
    var v$1384 = $(0., 0., 1.);
    var texture$1385 = "texture.jpg";
    var start$1386 =
      _f(function (param$1520) {
           return __(oc$Lwt$[16],
                     [
                       _($3E$3E$3D$1228,
                         [
                           _(load_image$1235, [ texture$1385 ]),
                           _f(function (texture$1387) {
                                var match$1529 = _(shadow$1239, [ texture$1387 ]);
                                var canvas$1391 = _(create_canvas$1224, [ width$1030, width$1030 ]);
                                var canvas$27$1392 = _(create_canvas$1224, [ width$1030, width$1030 ]);
                                (function () {
                                   var v$1584 = doc$1036.body;
                                   return _m(v$1584.appendChild, v$1584, [ canvas$1391 ]);
                                 }());
                                var ctx$1393 = _m(canvas$1391.getContext, canvas$1391, [ "2d" ]);
                                var ctx$27$1394 = _m(canvas$27$1392.getContext, canvas$27$1392, [ "2d" ]);
                                var r$1395 = width$1030 / 2.;
                                var tw$1396 = texture$1387.width;
                                var th$1397 = texture$1387.height;
                                var uv$1398 =
                                  _(oc$Array$[12],
                                    [ _f(function (v$1399) { return __(to_uv$1277, [ tw$1396, th$1397, v$1399 ]); }), o$1383[0] ]);
                                var normals$1400 =
                                  _(oc$Array$[12],
                                    [
                                      _f(function (param$1528) {
                                           var v1$1404 = oc$$arefs(o$1383[0], param$1528[0]);
                                           var v2$1405 = oc$$arefs(o$1383[0], param$1528[1]);
                                           var v3$1406 = oc$$arefs(o$1383[0], param$1528[2]);
                                           return __(cross_product$1085,
                                                     [ _(vect$1078, [ v1$1404, v2$1405 ]), _(vect$1078, [ v1$1404, v3$1406 ]) ]);
                                         }),
                                      o$1383[1]
                                    ]);
                                var face_info$1407 =
                                  _(oc$Array$[12],
                                    [
                                      _f(function (f$1408) {
                                           return __(precompute_mapping_info$1294, [ tw$1396, th$1397, uv$1398, f$1408 ]);
                                         }),
                                      o$1383[1]
                                    ]);
                                var paused$1409 = $(false);
                                var follow$1410 = $(false);
                                var lighting$1411 = $(true);
                                var clipped$1412 = $(true);
                                var obl$1413 = $(obliquity$1033);
                                var m_obliq$1414 = $(_(xy_rotation$1129, [ -obliquity$1033 ]));
                                var m$1415 = $(matrix_identity$1137);
                                var phi_rot$1416 = $(0.);
                                var rateText$1417 = _m(doc$1036.createTextNode, doc$1036, [ "" ]);
                                var add$1418 =
                                  _f(function (d$1419, e$1420) { _m(d$1419.appendChild, d$1419, [ e$1420 ]); return 0; });
                                var ctrl$1421 = _m(doc$1036.createElement, doc$1036, [ "div" ]);
                                ctrl$1421.className = "controls";
                                var d$1422 = _m(doc$1036.createElement, doc$1036, [ "div" ]);
                                _(add$1418,
                                  [ d$1422, _m(doc$1036.createTextNode, doc$1036, [ "Click and drag mouse to rotate." ]) ]);
                                _(add$1418, [ ctrl$1421, d$1422 ]);
                                var form$1423 = _m(doc$1036.createElement, doc$1036, [ "div" ]);
                                var br$1424 = _f(function (param$1527) { return __m(doc$1036.createElement, doc$1036, [ "br" ]); });
                                _(add$1418,
                                  [
                                    form$1423,
                                    _(toggle_button$1041,
                                      [ "Pause", "Resume", _f(function (p$1425) { return paused$1409[0] = p$1425; }) ])
                                  ]);
                                _(add$1418, [ form$1423, _(br$1424, [ 0 ]) ]);
                                _(add$1418,
                                  [
                                    form$1423,
                                    _(toggle_button$1041,
                                      [
                                        "Follow rotation",
                                        "Fixed position",
                                        _f(function (f$1426) { return follow$1410[0] = f$1426; })
                                      ])
                                  ]);
                                _(add$1418, [ form$1423, _(br$1424, [ 0 ]) ]);
                                _(add$1418,
                                  [
                                    form$1423,
                                    _(button$1037,
                                      [
                                        "Reset orientation",
                                        _f(function (param$1526) {
                                             m$1415[0] = matrix_identity$1137;
                                             phi_rot$1416[0] = 0.;
                                             return m_obliq$1414[0] = _(xy_rotation$1129, [ -obl$1413[0] ]);
                                           })
                                      ])
                                  ]);
                                _(add$1418, [ form$1423, _(br$1424, [ 0 ]) ]);
                                var lab$1427 = _m(doc$1036.createElement, doc$1036, [ "label" ]);
                                _(add$1418, [ lab$1427, _m(doc$1036.createTextNode, doc$1036, [ "Date:" ]) ]);
                                var s$1428 = _m(doc$1036.createElement, doc$1036, [ "select" ]);
                                _(oc$List$[9],
                                  [
                                    _f(function (txt$1429) {
                                         var o$1430 = _m(doc$1036.createElement, doc$1036, [ "option" ]);
                                         _(add$1418, [ o$1430, _m(doc$1036.createTextNode, doc$1036, [ txt$1429 ]) ]);
                                         return __m(s$1428.add, s$1428, [ o$1430, null ]);
                                       }),
                                    $("December solstice", $("Equinox", $("June solstice", 0)))
                                  ]);
                                s$1428.onchange =
                                  _f(function (param$1524) {
                                       var o$1431 =
                                         function () {
                                           var match$1525 = s$1428.selectedIndex;
                                           if (!!!match$1525) return obliquity$1033;
                                           if (match$1525 !== 1) return -obliquity$1033;
                                           return 0.;
                                         }();
                                       _(match$1529[1], [ o$1431 ]);
                                       return obl$1413[0] = o$1431;
                                     });
                                _(add$1418, [ lab$1427, s$1428 ]);
                                _(add$1418, [ form$1423, lab$1427 ]);
                                _m(ctrl$1421.appendChild, ctrl$1421, [ form$1423 ]);
                                var form$1432 = _m(doc$1036.createElement, doc$1036, [ "div" ]);
                                _(add$1418,
                                  [
                                    form$1432,
                                    _(checkbox$1047,
                                      [ "Lighting", true, _f(function (l$1433) { return lighting$1411[0] = l$1433; }) ])
                                  ]);
                                _(add$1418, [ form$1432, _(br$1424, [ 0 ]) ]);
                                _(add$1418,
                                  [
                                    form$1432,
                                    _(checkbox$1047, [ "Clip", true, _f(function (l$1434) { return clipped$1412[0] = l$1434; }) ])
                                  ]);
                                _(add$1418, [ form$1432, _(br$1424, [ 0 ]) ]);
                                _(add$1418, [ form$1432, _m(doc$1036.createTextNode, doc$1036, [ "Frames per second: " ]) ]);
                                _(add$1418, [ form$1432, rateText$1417 ]);
                                _(add$1418, [ ctrl$1421, form$1432 ]);
                                _(add$1418, [ doc$1036.body, ctrl$1421 ]);
                                var p$1435 = _m(doc$1036.createElement, doc$1036, [ "p" ]);
                                p$1435.innerHTML = "Credit: <a href=\'http://visibleearth.nasa.gov/\'>Visual Earth</a>, Nasa";
                                _(add$1418, [ doc$1036.body, p$1435 ]);
                                var mx$1436 = $(0);
                                var my$1437 = $(0);
                                canvas$1391.onmousedown =
                                  _f(function (ev$1438) {
                                       mx$1436[0] = ev$1438.clientX;
                                       my$1437[0] = ev$1438.clientY;
                                       var c1$1439 =
                                         _f(function (ev$1440) {
                                              var x$1441 = ev$1440.clientX;
                                              var y$1442 = ev$1440.clientY;
                                              var dx$1443 = x$1441 - mx$1436[0];
                                              var dy$1444 = y$1442 - my$1437[0];
                                              if (!!dy$1444)
                                                m$1415[0] =
                                                  _(matrix_mul$1115,
                                                    [ _(yz_rotation$1133, [ 2. * dy$1444 / width$1030 ]), m$1415[0] ]);
                                              else;
                                              if (!!dx$1443)
                                                m$1415[0] =
                                                  _(matrix_mul$1115,
                                                    [ _(xz_rotation$1125, [ 2. * dx$1443 / width$1030 ]), m$1415[0] ]);
                                              else;
                                              mx$1436[0] = x$1441;
                                              return my$1437[0] = y$1442;
                                            });
                                       _m(doc$1036.addEventListener, doc$1036, [ "mousemove", c1$1439, true ]);
                                       var c2$1445 =
                                         _f(function (param$1523) {
                                              _m(doc$1036.removeEventListener, doc$1036, [ "mousemove", c1$1439, true ]);
                                              return __m(doc$1036.removeEventListener, doc$1036, [ "mouseup", c2$1445, true ]);
                                            });
                                       _m(doc$1036.addEventListener, doc$1036, [ "mouseup", c2$1445, true ]);
                                       return false;
                                     });
                                var ti$1446 =
                                  $(function () {
                                      var v$1583 = _(oc$Javascript$[3], [ 0 ]);
                                      return _m(v$1583.getTime, v$1583, [  ]);
                                    }());
                                var fps$1447 = $(0.);
                                var loop$1448 =
                                  _f(function (t$1449, phi$1450) {
                                       var rotation$1451 = _(xz_rotation$1125, [ phi$1450 - phi_rot$1416[0] ]);
                                       _(match$1529[2], [ lighting$1411[0], phi$1450 ]);
                                       var m$1452 =
                                         _(matrix_mul$1115, [ m$1415[0], _(matrix_mul$1115, [ m_obliq$1414[0], rotation$1451 ]) ]);
                                       var o$27$1453 = _(rotate_object$1157, [ m$1452, o$1383 ]);
                                       var v$27$1454 = _(rotate_normal$1138, [ m$1452, v$1384 ]);
                                       _m(ctx$27$1394.clearRect, ctx$27$1394, [ 0., 0., width$1030, width$1030 ]);
                                       _m(ctx$27$1394.save, ctx$27$1394, [  ]);
                                       if (clipped$1412[0]) {
                                         {
                                           _m(ctx$27$1394.beginPath, ctx$27$1394, [  ]);
                                           _m(ctx$27$1394.arc, ctx$27$1394,
                                              [ r$1395, r$1395, r$1395 * 0.95, 0., -2. * pi$1032, true ]);
                                           _m(ctx$27$1394.clip, ctx$27$1394, [  ]);
                                         }
                                       }
                                       else;
                                       _m(ctx$27$1394.setTransform, ctx$27$1394,
                                          [ r$1395 - 2., 0., 0., r$1395 - 2., r$1395, r$1395 ]);
                                       ctx$27$1394.globalCompositeOperation = "lighter";
                                       _(draw$1338,
                                         [
                                           ctx$27$1394,
                                           texture$1387,
                                           match$1529[0],
                                           o$27$1453,
                                           uv$1398,
                                           normals$1400,
                                           face_info$1407,
                                           v$27$1454
                                         ]);
                                       _m(ctx$27$1394.restore, ctx$27$1394, [  ]);
                                       ctx$1393.globalCompositeOperation = "copy";
                                       _m(ctx$1393.drawImage, ctx$1393, [ canvas$27$1392, 0., 0. ]);
                                       try { _m(ctx$1393.getImageData, ctx$1393, [ 0., 0., 1., 1. ]); } catch (exn$1522) { }
                                       var t$27$1455 =
                                         function () {
                                           var v$1582 = _(oc$Javascript$[3], [ 0 ]);
                                           return _m(v$1582.getTime, v$1582, [  ]);
                                         }();
                                       fps$1447[0] =
                                         function () {
                                           var hz$1456 = 1000. / (t$27$1455 - ti$1446[0]);
                                           if (fps$1447[0] === 0.) return hz$1456;
                                           return 0.9 * fps$1447[0] + 0.1 * hz$1456;
                                         }();
                                       rateText$1417.data = _(oc$Printf$[4], [ "% 2.f", fps$1447[0] ]);
                                       ti$1446[0] = t$27$1455;
                                       return __($3E$3E$3D$1228,
                                                 [
                                                   _(oc$Lwt_dom$[0], [ 0.01 ]),
                                                   _f(function (param$1521) {
                                                        var t$27$1457 =
                                                          function () {
                                                            var v$1581 = _(oc$Javascript$[3], [ 0 ]);
                                                            return _m(v$1581.getTime, v$1581, [  ]);
                                                          }();
                                                        var dt$1458 = t$27$1457 - t$1449;
                                                        var dt$1459 = dt$1458 < 0. ? 0. : dt$1458 > 1000. ? 0. : dt$1458;
                                                        var angle$1460 = 2. * pi$1032 * dt$1459 / 1000. / 10.;
                                                        if (!paused$1409[0] && follow$1410[0])
                                                          phi_rot$1416[0] = phi_rot$1416[0] + angle$1460;
                                                        else;
                                                        return __(loop$1448,
                                                                  [ t$27$1457, paused$1409[0] ? phi$1450 : phi$1450 + angle$1460 ]);
                                                      })
                                                 ]);
                                     });
                                return __(loop$1448,
                                          [
                                            function () {
                                              var v$1580 = _(oc$Javascript$[3], [ 0 ]);
                                              return _m(v$1580.getTime, v$1580, [  ]);
                                            }(),
                                            0.
                                          ]);
                              })
                         ])
                     ]);
         });
    (oc$Dom$[0]).onload = start$1386;
    return $(width$1030, width$1030, pi$1032, obliquity$1033, gamma$1034, dark$1035, doc$1036, button$1037, toggle_button$1041,
             checkbox$1047, radio$1053, vertex$1067, vect$1078, cross_product$1085, dot_product$1092, matrix_vect_mul$1099,
             matrix_transp$1110, matrix_mul$1115, normalize$1119, xz_rotation$1125, xy_rotation$1129, yz_rotation$1133,
             matrix_identity$1137, rotate_normal$1138, face$1148, rotate_object$1157, octahedron$1161, tesselate_sphere$1162,
             divide$1181, create_canvas$1224, $3E$3E$3D$1228, lwt_wrap$1229, load_image$1235, shadow$1239, to_uv$1277, min$1288,
             max$1291, precompute_mapping_info$1294, draw$1338, $3E$3E$1380, o$1383, v$1384, texture$1385, start$1386);
  }();
var oc$Std_exit$ = (_(oc$Pervasives$[80], [ 0 ]), $());
return caml_named_value;
})();
