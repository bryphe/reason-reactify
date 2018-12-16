/**
  * This is an implementation of a reconciler for DOM elements via js_of_ocaml :
  * http://ocsigen.org/js_of_ocaml/3.1.0/api/Dom_html
  *
  * This is just an example but you could use this to create interesting
  * CLI apps, with a react-like functional API!
*/

exception InvalidNodePrimitiveMatchInUpdateInstance;

let str = string_of_int;

module Reconciler = {
  /*
     Step 1: Define primitives
   */
  type primitives =
    | View
    | Text(string)
    | Image(string) /* img src */
    | Button(unit => unit, string); /* onPress, title */

  /*
     Step 2: Define node type
   */
  type node =
    | Div(Js.t(Dom_html.divElement))
    | Span(Js.t(Dom_html.element))
    | Image(Js.t(Dom_html.imageElement))
    | Button(Js.t(Dom_html.buttonElement))
    | Container(Js.t(Dom_html.element));
  let document = Dom_html.window##.document;

  /*
     Step 3: Implement a create function
   */
  let createInstance: primitives => node =
    primitive =>
      switch (primitive) {
      | View => Div(Dom_html.createDiv(document))
      | Text(s) =>
        let e = Dom_html.createSpan(document);
        e##.innerHTML := Js.string(s);
        Span(e);
      | Image(p) =>
        let img = Dom_html.createImg(document);
        img##.src := Js.string(p);
        Image(img);
      | Button(onPress, title) =>
        let button =
          Dom_html.createButton(~_type=Js.string("button"), document);
        let t = Js.string(title);
        button##.title := t;
        button##.innerHTML := t;
        button##.onclick :=
          Dom_html.handler(_e => {
            onPress();
            Js.bool(false);
          });
        Button(button);
      };

  /*
      Step 4: Implement remaining primitives
   */

  let _getInnerNode = node =>
    switch (node) {
    | Div(e) => e |> Dom_html.element
    | Span(e) => e |> Dom_html.element
    | Image(e) => e |> Dom_html.element
    | Button(e) => e |> Dom_html.element
    | Container(e) => e |> Dom_html.element
    };

  let updateInstance =
      (node: node, _oldPrimitive: primitives, newPrimitive: primitives) =>
    switch (newPrimitive, node) {
    | (View, Div(_e)) => ()
    | (Text(s), Span(e)) => e##.innerHTML := Js.string(s)
    | (Image(src), Image(e)) => e##.src := Js.string(src)
    | (Button(onPress, title), Button(e)) =>
      let t = Js.string(title);
      e##.title := t;
      e##.innerHTML := t;
      e##.onclick :=
        Dom_html.handler(_e => {
          onPress();
          Js.bool(false);
        });
    | _ => raise(InvalidNodePrimitiveMatchInUpdateInstance)
    };

  let appendChild = (parentNode: node, childNode: node) => {
    let innerNode = _getInnerNode(childNode);
    switch (parentNode) {
    | Div(e) => Dom.appendChild(e, innerNode)
    | Span(e) => Dom.appendChild(e, innerNode)
    | Image(e) => Dom.appendChild(e, innerNode)
    | Button(e) => Dom.appendChild(e, innerNode)
    | Container(e) => Dom.appendChild(e, innerNode)
    };
  };

  let removeChild = (parentNode: node, childNode: node) => {
    let innerNode = _getInnerNode(childNode);
    switch (parentNode) {
    | Div(e) => Dom.removeChild(e, innerNode)
    | Span(e) => Dom.removeChild(e, innerNode)
    | Image(e) => Dom.removeChild(e, innerNode)
    | Button(e) => Dom.removeChild(e, innerNode)
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
    | Button(e) => Dom.replaceChild(e, newInnerNode, oldInnerNode)
    | Container(e) => Dom.replaceChild(e, newInnerNode, oldInnerNode)
    };
  };
};

/* Step 5: Hook it up! */
module JsooReact = Reactify.Make(Reconciler);
open JsooReact;

/* Define our primitive components */
let view = (~children, ()) => JsooReact.primitiveComponent(View, ~children);

let image = (~children, ~src="", ()) =>
  JsooReact.primitiveComponent(Image(src), ~children);

let text = (~children: list(string), ()) =>
  JsooReact.primitiveComponent(Text(List.hd(children)), ~children=[]);

let button = (~children, ~onPress, ~title, ()) =>
  JsooReact.primitiveComponent(Button(onPress, title), ~children);

type action =
  | Increment
  | Decrement;

let reducer = (state, action) =>
  switch (action) {
  | Increment => state + 1
  | Decrement => state - 1
  };

let renderCounter = () => {
  let (count, dispatch) = useReducer(reducer, 0);

  <view>
    <button title="Decrement" onPress={() => dispatch(Decrement)} />
    <text> {"Counter: " ++ str(count)} </text>
    <button title="Increment" onPress={() => dispatch(Increment)} />
  </view>;
};

module CounterButtons = (
  val component((render, ~children, ()) => render(renderCounter, ~children))
);

/* Create a container for our UI */
let container =
  JsooReact.createContainer(
    Reconciler.Container(Dom_html.getElementById_exn("app")),
  );

/* Let's finally put our UI to use! */
let render = () =>
  <view> <text> "Hello World" </text> <CounterButtons /> </view>;

/* First render! */
JsooReact.updateContainer(container, render());