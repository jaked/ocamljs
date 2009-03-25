let sleep t =
  let res = Lwt.wait () in
  let timeout () = Lwt.wakeup res () in
  ignore (Dom.window#setTimeout (Ocamljs.jsfun timeout) t);
  res

let yield () = sleep 0.

let http_request ?(headers=[]) meth url =
  let res = Lwt.wait () in
  let meth, body =
    match meth with
      | `Get -> "GET", None
      | `Post body -> "POST", Some body in
  let r = Dom.new_XMLHttpRequest () in
  r#open_ meth url true;
  List.iter
    (fun (k, v) -> r#setRequestHeader k v)
    headers;
  let fired = ref false in
  let onreadystatechange () =
    if r#_get_readyState = 4
    then begin
      if not !fired then Lwt.wakeup res r;
      fired := true
    end in
  r#_set_onreadystatechange (Ocamljs.jsfun onreadystatechange);
  r#send (match body with Some body -> body | _ -> Ocamljs.null ());
  res
