open JQuery
open Dom

let () = jQuery_ready
  begin fun (~$) ->

    (* Code for example A *)
    ~$"input.buttonAsize"#click
      (fun _ ->
         let size = ~$"div.contentToChange"#find "p"#size in
         window#alert (string_of_int size);
         false);
    (* Show code example A *)
    ~$"a.codeButtonA"#click (fun _ -> ~$"pre.codeA"#toggle; false);

    (* Code for example B *)
    ~$"input.buttonBslidedown"#click
      (fun _ ->
         ~$"div.contentToChange"#find "p.firstparagraph:hidden"
           #slideDown_speed_ "slow" ignore;
         false);
    ~$"input.buttonBslideup"#click
      (fun _ ->
         ~$"div.contentToChange"#find "p.firstparagraph:visible"
           #slideUp_speed_ "slow" ignore;
         false);
    (* Show code example B *)
    ~$"a.codeButtonB"#click (fun _ -> ~$"pre.codeB"#toggle; false);

    (* Code for example C *)
    ~$"input.buttonCAdd"#click
      (fun _ ->
         ~$"div.contentToChange"#find "p"#not ".alert"
           #append "<strong class=\"addedtext\">&nbsp;This text was just appended to this paragraph</strong>";
         false);
    ~$"input.buttonCRemove"#click
      (fun _ ->
         ~$"strong.addedtext"#remove;
         false);
    (* Show code example C *)
    ~$"a.codeButtonC"#click (fun _ -> ~$"pre.codeC"#toggle; false);

    (* Code for example D *)
    ~$"input.buttonDhide"#click
      (fun _ ->
         ~$"div.contentToChange"#find "p.thirdparagraph"
           #hide_speed_ "slow" ignore;
         false);
    (* Show code example D *)
    ~$"a.codeButtonD"#click (fun _ -> ~$"pre.codeD"#toggle; false);

    (* Code for example E *)
    ~$"input.buttonEitalics"#click
      (fun _ ->
         ~$"div.contentToChange"#find "em"#css_obj_
           (object
              method color = "#993300"
              method fontWeight = "bold"
            end);
         false);
    (* Show code example E *)
    ~$"a.codeButtonE"#click (fun _ -> ~$"pre.codeE"#toggle; false);

    (* Code for example F *)
    ~$"input.buttonFaddclass"#click
      (fun _ -> ~$"p.fifthparagraph"#addClass "changeP"; false);
    ~$"input.buttonFremoveclass"#click
      (fun _ -> ~$"p.fifthparagraph"#removeClass "changeP"; false);
    (* Show code example F *)
    ~$"a.codeButtonF"#click (fun _ -> ~$"pre.codeF"#toggle; false);

  end
