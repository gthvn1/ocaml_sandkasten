(*
 We want to be able to describe our HTML page like this:

     tag "html" [
       tag "body" [
         tag "h1" [text "Hello, Sailor!"];
         tag "p" [text "Welcome aboard..."];
       ]
     ]

  We will use a DSL...
*)
type html = Text of string | Tag of string * html list

let tag name children = Tag (name, children)
let text str = Text str

(*
 At this point we have everything to descript our page.
 Next step, render the HTML page. That is the evaluation.
*)

let rec render page =
  match page with
  | Text s -> s
  | Tag (b, h) ->
      Printf.sprintf "<%s>%s</%s>" b (String.concat "" (List.map render h)) b

let welcome_page =
  tag "html"
    [
      tag "body"
        [
          tag "h1" [ text "Hello, Sailor!" ];
          tag "p" [ text "Welcome aboard..." ];
        ];
    ]
