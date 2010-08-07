open JQuery
open Dom

let () = jQuery_ready
  begin fun j ->

    (* Code for example A *)
    (j "input.buttonAsize")#click
      (fun _ ->
         let size = ((j "div.contentToChange")#find "p")#size in
         window#alert (string_of_int size);
         false);
    (* Show code example A *)
    (j "a.codeButtonA")#click (fun _ -> (j "pre.codeA")#toggle; false);

    (* Code for example B *)
    (j "input.buttonBslidedown")#click
      (fun _ ->
         ((j "div.contentToChange")#find "p.firstparagraph:hidden")
           #slideDown_speed_ "slow" ignore;
         false);
    (j "input.buttonBslideup")#click
      (fun _ ->
         ((j "div.contentToChange")#find "p.firstparagraph:visible")
           #slideUp_speed_ "slow" ignore;
         false);
    (* Show code example B *)
    (j "a.codeButtonB")#click (fun _ -> (j "pre.codeB")#toggle; false);

    (* Code for example C *)
    (j "input.buttonCAdd")#click
      (fun _ ->
         (((j "div.contentToChange")#find "p")#not ".alert")
           #append "<strong class=\"addedtext\">&nbsp;This text was just appended to this paragraph</strong>";
         false);
    (j "input.buttonCRemove")#click
      (fun _ ->
         (j "strong.addedtext")#remove;
         false);
    (* Show code example C *)
    (j "a.codeButtonC")#click (fun _ -> (j "pre.codeC")#toggle; false);

    (* Code for example D *)
    (j "input.buttonDhide")#click
      (fun _ ->
         ((j "div.contentToChange")#find "p.thirdparagraph")
           #hide_speed_ "slow" ignore;
         false);
    (* Show code example D *)
    (j "a.codeButtonD")#click (fun _ -> (j "pre.codeD")#toggle; false);

    (* Code for example E *)
    (j "input.buttonEitalics")#click
      (fun _ ->
         ((j "div.contentToChange")#find "em")#css_obj_
           (object
              method color = "#993300"
              method fontWeight = "bold"
            end);
         false);
    (* Show code example E *)
    (j "a.codeButtonE")#click (fun _ -> (j "pre.codeE")#toggle; false);

    (* Code for example F *)
    (j "input.buttonFaddclass")#click
      (fun _ -> (j "p.fifthparagraph")#addClass "changeP"; false);
    (j "input.buttonFremoveclass")#click
      (fun _ -> (j "p.fifthparagraph")#removeClass "changeP"; false);
    (* Show code example F *)
    (j "a.codeButtonF")#click (fun _ -> (j "pre.codeF")#toggle; false);

  end
