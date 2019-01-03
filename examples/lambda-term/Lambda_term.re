open Lwt;
open LTerm_widget;

/**
  * This is an implementation of a reconciler for the Lambda_term widget library:
  * https://github.com/ocaml-community/lambda-term
  *
  * This is just an example but you could use this to create interesting
  * CLI apps, with a react-like functional API!
*/
module Reconciler = {
  open LTerm_widget;

  /*
     Step 1: Define primitives
   */
  type buttonProperties = {
    text: string,
    onClick: unit => unit,
  };

  type primitives =
    | Vbox
    | Hbox
    | Label(string)
    | Button(buttonProperties);

  /*
     Step 2: Define node type
   */

  /* For this, ideally we could just use the base widget class
   * There are some quirks with polymorphic typing, but this
   * could possibly be simplified.
   */
  class widget = class LTerm_widget.t;
  type node =
    | Label(LTerm_widget.label)
    | Button(LTerm_widget.button)
    | Container(LTerm_widget.box);

  /*
     Step 3: Implement a create function
   */

  let createInstance = prim =>
    switch (prim) {
    | Vbox => Container(new vbox)
    | Hbox => Container(new hbox)
    | Label(txt) =>
      let label = (new label)(txt);
      Label(label);
    | Button(props) =>
      let button = (new button)(props.text);
      button#on_click(props.onClick);
      Button(button);
    };

  /*
      Step 4: Implement remaining primitives
   */

  let _getInnerNode = node =>
    switch (node) {
    | Label(x) => (x :> widget)
    | Button(y) => (y :> widget)
    | Container(z) => (z :> widget)
    };

  let updateInstance =
      (node: node, _oldPrimitive: primitives, newPrimitive: primitives) =>
    switch (newPrimitive, node) {
    | (Label(txt), Label(n)) => n#set_text(txt)
    | (Button(buttonProps), Button(n)) =>
      n#set_label(buttonProps.text);
      n#on_click(buttonProps.onClick);
    | _ => ()
    };

  let appendChild = (parentNode: node, childNode: node) =>
    switch (parentNode) {
    | Container(n) => n#add(_getInnerNode(childNode))
    | _ => ()
    };

  let removeChild = (parentNode: node, childNode: node) =>
    switch (parentNode) {
    | Container(n) => n#remove(_getInnerNode(childNode))
    | _ => ()
    };

  let replaceChild = (parentNode: node, oldChild: node, newChild: node) => {
    removeChild(parentNode, oldChild);
    appendChild(parentNode, newChild);
  };
};

/* Step 5: Hook it up! */
module LambdaReact = Reactify.Make(Reconciler);
open LambdaReact;

/* Define our primitive components */
let hbox = (~children, ()) => primitiveComponent(Hbox, ~children);

let vbox = (~children, ()) => primitiveComponent(Vbox, ~children);

let label = (~children, ~text, ()) =>
  primitiveComponent(Label(text), ~children);

let button = (~children, ~text, ~onClick, ()) => {
  let buttonProps: Reconciler.buttonProperties = {text, onClick};

  primitiveComponent(Button(buttonProps), ~children);
};

/* And let's create some custom components! */
/*
    CounterButtons

    This shows how you can use reducers with a callback from a primitive.
 */

type action =
  | Increment
  | Decrement;

let reducer = (state, action) =>
  switch (action) {
  | Increment => state + 1
  | Decrement => state - 1
  };

let renderCounter = () =>
  useReducerExperimental(reducer, 0, ((count, dispatch)) =>
    <hbox>
      <button text="Decrement" onClick={() => dispatch(Decrement)} />
      <label text={"Counter: " ++ string_of_int(count)} />
      <button text="Increment" onClick={() => dispatch(Increment)} />
    </hbox>
  );

module CounterButtons = (
  val createComponent((render, ~children, ()) =>
        render(renderCounter, ~children)
      )
);
/*
    Clock

    Custom clock component to show the time. Demonstrates
    use of `useEffect` and `setState` together.
 */
module Clock = (
  val createComponent((render, ~children, ()) =>
        render(
          () =>
            useStateExperimental(0., ((time, setTime)) =>
              useEffectExperimental(
                () => {
                  let evt =
                    Lwt_engine.on_timer(1.0, true, _ => setTime(Unix.time()));

                  () => Lwt_engine.stop_event(evt);
                },
                () => <label text={"Time: " ++ string_of_float(time)} />,
              )
            ),
          ~children,
        )
      )
);

let main = () => {
  let (waiter, wakener) = wait();

  /* Create a container for our UI */
  let body = new vbox;
  let root = Reconciler.Container(body);
  let container = createContainer(root);

  let quit = () => wakeup(wakener, ());

  /* Let's finally put our UI to use! */
  let render = () =>
    <vbox>
      <label text="Hello from Reactify!" />
      <Clock />
      <CounterButtons />
      <button onClick=quit text="Quit" />
    </vbox>;

  /* First render! */
  LambdaReact.updateContainer(container, render());

  Lazy.force(LTerm.stdout)
  >>= (
    term =>
      LTerm.enable_mouse(term)
      >>= (
        () =>
          Lwt.finalize(
            () => run(term, body, waiter),
            () => LTerm.disable_mouse(term),
          )
      )
  );
};

let () = Lwt_main.run(main());