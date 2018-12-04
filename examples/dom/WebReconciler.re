/**
  * This is an implementation of a reconciler for DOM elements via js_of_ocaml :
  * http://ocsigen.org/js_of_ocaml/3.1.0/api/Dom_html
  *
  * This is just an example but you could use this to create interesting
  * CLI apps, with a react-like functional API!
*/
module Reconciler = {
  /*
     Step 1: Define primitives
   */
  type imageProps = {src: string};
  type buttonProps = {src: string};
  type primitives =
    | Div
    | Span(string)
    | Image(imageProps);

  /*
     Step 2: Define node type
   */
  type node =
    | Div(Js.t(Dom_html.divElement))
    | Span(Js.t(Dom_html.element))
    | Image(Js.t(Dom_html.imageElement))
    | Container(Js.t(Dom_html.element));
  let document = Dom_html.window##.document;

  /*
     Step 3: Implement a create function
   */
  let createInstance: primitives => node =
    primitive =>
      switch (primitive) {
      | Div => Div(Dom_html.createDiv(document))
      | Span(s) =>
        let e = Dom_html.createSpan(document);
        e##.innerHTML := Js.string(s);
        Span(e);
      | Image(p) =>
        let img = Dom_html.createImg(document);
        img##.src := Js.string(p.src);
        Image(img);
      };

  /*
      Step 4: Implement remaining primitives
   */

  let _getInnerNode = node =>
    switch (node) {
    | Div(e) => e |> Dom_html.element
    | Span(e) => e |> Dom_html.element
    | Image(e) => e |> Dom_html.element
    | Container(e) => e |> Dom_html.element
    };

  let updateInstance =
      (node: node, _oldPrimitive: primitives, newPrimitive: primitives) =>
    switch (newPrimitive, node) {
    /* The only update operation we handle today is updating src for an image! */
    | (Image({src}), Image(e)) => e##.src := Js.string(src)
    | _ => ()
    };

  let appendChild = (parentNode: node, childNode: node) => {
    let innerNode = _getInnerNode(childNode);
    switch (parentNode) {
    | Div(e) => Dom.appendChild(e, innerNode)
    | Span(e) => Dom.appendChild(e, innerNode)
    | Image(e) => Dom.appendChild(e, innerNode)
    | Container(e) => Dom.appendChild(e, innerNode)
    };
  };

  let removeChild = (parentNode: node, childNode: node) => {
    let innerNode = _getInnerNode(childNode);
    switch (parentNode) {
    | Div(e) => Dom.removeChild(e, innerNode)
    | Span(e) => Dom.removeChild(e, innerNode)
    | Image(e) => Dom.removeChild(e, innerNode)
    | Container(e) => Dom.removeChild(e, innerNode)
    };
  };

  let replaceChild = (parentNode: node, oldChild: node, newChild: node) => {
    let newInnerNode = _getInnerNode(newChild);
    let oldInnerNode = _getInnerNode(oldChild);
    switch (parentNode) {
    | Div(e) => Dom.replaceChild(e, newInnerNode, oldInnerNode)
    | Span(e) => Dom.replaceChild(e, newInnerNode, oldInnerNode)
    | Image(e) => Dom.replaceChild(e, newInnerNode, oldInnerNode)
    | Container(e) => Dom.replaceChild(e, newInnerNode, oldInnerNode)
    };
  };
};

/* Step 5: Hook it up! */
module JsooReact = Reactify.Make(Reconciler);

/* Define our primitive components */
let div = (~children, ()) => JsooReact.primitiveComponent(Div, ~children);

let span = (~text, ~children, ()) =>
  JsooReact.primitiveComponent(Span(text), ~children);

let image = (~children, ~src, ()) =>
  JsooReact.primitiveComponent(Image(src), ~children);

/* Create a container for our UI */
let container =
  JsooReact.createContainer(
    Reconciler.Container(Dom_html.getElementById_exn("app")),
  );

/* Let's finally put our UI to use! */
let render = () =>
  <span text="Hello World" />;

/* First render! */
JsooReact.updateContainer(container, render());