[![Build Status](https://bryphe.visualstudio.com/reason-reactify/_apis/build/status/bryphe.reason-reactify)](https://bryphe.visualstudio.com/reason-reactify/_build/latest?definitionId=3)
[![npm version](https://badge.fury.io/js/reason-reactify.svg)](https://badge.fury.io/js/reason-reactify)

# :rocket: Reactify

#### Transform a mutable tree into a functional React-like API, in native Reason!

## Why?

Reason is "react as originally intended", and the language provides excellent faculty for expressing a react-like function API, with built-in JSX support.

I often think of React purely in the context of web technologies / DOM, but the abstraction is really useful in other domains, too.

There are often cases where we either inherit some sort of mutable state tree (ie, the DOM), or when we create such a mutable structure for performance reasons (ie, a scene graph in a game engine). Having a functional API that always renders the entire tree is a way to reduce cognitive load, but for the DOM or a scene graph, it'd be too expensive to rebuild it all the time. So a react-like reconciler is useful for being able to express the tree in a stateless, purely functional way, but also reap the performance benefits of only mutating what needs to change.

## Let's build a reconciler!

### Quickstart

The way the library works is you implement a Module that implements some basic functionality:

This will be familiar if you've ever created a React reconciler before!

### Step 1: Tell us about your primitives

First, let's pretend we're building a reconciler for a familiar domain - the HTML DOM. (You wouldn't really want to do this for production - you're better off using ReasonReact in that case!). But it's a good example of how a reconciler works end-to-end.

We'll start with an empty module:
```diff
+ module WebReconciler {
+
+ }
```

And we'll create a type `t` that is a [__variant__](https://reasonml.github.io/docs/en/variant) specifying the different types of primitives. Primitives are the _core building blocks_ of your reconciler - these correspond to raw dom nodes, or whatever base type you need for your reconciler.

Let's start it up with a few simple tags:
```diff
module WebReconciler {
+    type imageProps = {
+       src: string;
+    };
+
+    type t =
+    | Div
+    | Span
+    | Image(imageProps);
}
```

### Step 2: Tell us your node type

The `node` is the type of the actual object we'll be working with in our mutable state tree. If we're using [`js_of_ocaml`](https://ocsigen.org/js_of_ocaml), we'd get access to the DOM elements via [`Dom_html.element`](https://ocsigen.org/js_of_ocaml/3.1.0/api/Dom_html) - that's the type we'll use for our node.

```diff
module WebReconciler {
    type imageProps = {
       src: string;
    };

    type t =
    | Div
    | Span
    | Image(imageProps);

+    module Html = Dom_html;
+    type node = Dom_html.element;
}
```

Not too bad so far!

### Step 3: Implement a create function

One of the most important jobs our reconciler has is to turn the _primitive_ objects into real, living _nodes_. Let's implement that now!

```diff
module WebReconciler {
    type imageProps = {
       src: string;
    };

    type t =
    | Div
    | Span
    | Image(imageProps);

    type node = Dom_html.element;

+    let document = Html.window##.document;
+
+    let createInstance: t => node = (primitive) => {
+        switch(primitive) {
+        | Div => Html.createDiv(document);
+        | Span => Html.createSpan(document);
+        | Image(p) => 
+           let img = Html.createImage(document);
+           img.src = p.src;
+           img;
+        };
+    };
}
```

Note how easy [pattern matching](https://reasonml.github.io/docs/en/pattern-matching) makes it to go from primitives to nodes.

### Step 4: Implement remaining tree operations 

For our reconciler to work, we also need to implement these operations:
- `updateInstance`
- `appendChild`
- `removeChild`
- `replaceChild`

Let's set those up!

```diff
module WebReconciler {
    type imageProps = {
       src: string;
    };

    type t =
    | Div
    | Span(string)
    | Image(imageProps);

    type node = Dom_html.element;

    let document = Html.window##.document;

    let createInstance: t => node = (primitive) => {
        switch(primitive) {
        | Div => Html.createDiv(document);
        | Span(t) => 
            let span = Html.createSpan(document);
            span##textContent = t;
        | Image(p) => 
           let img = Html.createImage(document);
           img##src = p.src;
           img;
        };
    };

+   let updateInstance = (node, oldPrimitive, newPrimitive) => {
+       switch ((oldState, newState)) => {
+       /* The only update operation we handle today is updating src for an image! */
+       | (Image(old), Image(new)) => node.src = new.src;
+       | _ => ();
+       };
+   };
+
+   let appendChild = (parentNode, childNode) => {
+       parentNode.appendChild(childNode);
+   };
+
+   let removeChild = (parentNode, childNode) => {
+       parentNode.removeChild(childNode);
+   };
+
+   let replaceChild = (parentNode, oldChild, newChild) => {
+       parentNode.replaceChild(oldChild, newChild);
+   };
}
```

Phew! That was a lot. Note that `createInstance` and `updateInstance` are operations that use _primitives_ to pass around some context. In this case, we carry around a `src` property for Image, and we set it on `createInstance` and `updateInstance`. The other operations - `appendChild`, `removeChild`, and `replaceChild` are purely _node_ operations. The internals of the reconciler handle the details of associating a `primitive` -> `node`.

### Step 5: Hook it up

`reactify` provides a [functor](https://reasonml.github.io/docs/en/module#module-functions-functors) for building a React API from your reconciler:

```diff
+ module MyReact = Reactify.Make(WebReconciler);
```

We'll also want to define some _primitive components_:

```diff
+ let div = (~children, ()) => primitiveComponent(Div, ~children);
+ let span = (~children, ~text, ()) => primitiveComponent(Span(text), ~children);
+ let image = (~children, ~src, ()) => primitiveComponent(Image(src), ~children);
```

These primitives are the building blocks that we can start composing to build interesting things.

Cool!

### Step 6: Use your API!

We have everything we need to start building things. Every Reactify'd API needs a container - this stores the current reconciliation state and allows us to do delta updates. We can create one like so:

#### Create / update a container

```diff
+ let container = MyReact.createContainer(Html.window##.document##body())
+ MyReact.updateContainer(container, <span text="Hello World" />):
```

#### Create custom components

## API

- Custom Components
    - Hooks
        - useState
        - useEffect
    - Context

### Examples

- [Lambda_term](examples/Lambda_term_reconciler.re)

### Usages

- [Revery](https://github.com/bryphe/revery)

## Development

### Install [esy](https://esy.sh/)

`esy` is like `npm` for native code. If you don't have it already, install it by running:
```
npm install -g esy
```

### Building

- `esy install`
- `esy build`

### Running Examples

- `esy b dune build @examples`

> You can then run `Lambda_term.exe` in `_build/default/examples`

### Running Tests

- `esy b dune runtest test`

## Limitations

- This project is not using a __fiber-based__ renderer. In particular, that means the following limitations:
    - Custom components are constrained to returning a single element, to simplify reconciliation.
    - Updates are not suspendable / resumable. This may not be necessary with native-performance, but it would be nice to have.

## License

This project is provided under the [MIT License](LICENSE).

Copyright 2018 Bryan Phelps.

## Additional Resources

- The API surface was inspired by the [react-reconciler](https://github.com/facebook/react/tree/master/packages/react-reconciler) package. If you've worked with that before, the `createContainer` and `updateContainer` will be familiar.
- Related / helpful projects:
    - [Didact: A DIY React Renderer](https://engineering.hexacta.com/didact-learning-how-react-works-by-building-it-from-scratch-51007984e5c5) - was really useful for learning!
    - [ReactMini](https://github.com/reasonml/reason-react/tree/master/ReactMini/src)
    - [BriskML](https://github.com/briskml/brisk) has an expanded implementation of ReactMini: https://github.com/briskml/brisk/blob/master/core/lib/ReactCore_Internal.re
