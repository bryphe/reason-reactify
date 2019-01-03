/** HooksUseReducer **/
open Rejest;

open TestReconciler;
open TestUtility;

module Event = Reactify.Event;

/* Use our Reconciler to create our own instance */
module TestReact = Reactify.Make(TestReconciler);
open TestReact;

let createRootNode = () => {children: ref([]), nodeId: 0, nodeType: Root};

let aComponent = (~testVal, ~children, ()) =>
  primitiveComponent(A(testVal), ~children);
let bComponent = (~children, ()) => primitiveComponent(B, ~children);
let cComponent = (~children, ()) => primitiveComponent(C, ~children);

type action =
  | Increase
  | Decrease;
let reducer = (state, action) =>
  switch (action) {
  | Increase => state + 1
  | Decrease => state - 1
  };

module ComponentWithState = (
  val createComponent((render, ~children, ()) =>
        render(
          () =>
            useReducerExperimental(reducer, 2, ((s, _dispatch)) =>
              <aComponent testVal=s />
            ),
          ~children,
        )
      )
);

type renderOption =
  /* | Nothing */
  | RenderAComponentWithState
  | RenderAComponent(int);

test("useReducer", () => {
  test("useReducer uses initial state", () => {
    let rootNode = createRootNode();
    let container = createContainer(rootNode);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(2))]);

    updateContainer(container, <ComponentWithState />);

    validateStructure(rootNode, expectedStructure);
  });

  module ComponentThatDispatchesIncreaseAction = (
    val createComponent(
          (render, ~children, ~event: Event.t(unit), ~initialValue: int, ()) =>
          render(
            () =>
              useReducerExperimental(reducer, initialValue, ((s, dispatch)) =>
                useEffectExperimental(
                  () => {
                    let unsubscribe =
                      Event.subscribe(event, () => dispatch(Increase));
                    () => unsubscribe();
                  },
                  () => <aComponent testVal=s />,
                )
              ),
            ~children,
          )
        )
  );

  test("useReducer updates state with dispatch function", () => {
    let rootNode = createRootNode();

    let container = createContainer(rootNode);

    let event: Event.t(unit) = Event.create();

    updateContainer(
      container,
      <ComponentThatDispatchesIncreaseAction event initialValue=2 />,
    );

    Event.dispatch(event, ());

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(3))]);
    validateStructure(rootNode, expectedStructure);
  });

  test("useReducer doesn't leak state between components", () => {
    let rootNode = createRootNode();

    let container = createContainer(rootNode);

    let event: Event.t(unit) = Event.create();

    updateContainer(
      container,
      <ComponentThatDispatchesIncreaseAction event initialValue=2 />,
    );

    Event.dispatch(event, ());

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(3))]);
    validateStructure(rootNode, expectedStructure);

    updateContainer(container, <ComponentWithState />);

    /* The 'componentWithState' should have its own state, so it should revert back to 2 - */
    /* and not pick up the state from the previous component */
    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(2))]);
    validateStructure(rootNode, expectedStructure);
  });

  test("useReducer dispatch state persists across renders", () => {
    let rootNode = createRootNode();

    let container = createContainer(rootNode);

    let event: Event.t(unit) = Event.create();

    updateContainer(
      container,
      <ComponentThatDispatchesIncreaseAction event initialValue=2 />,
    );

    Event.dispatch(event, ());

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(3))]);
    validateStructure(rootNode, expectedStructure);

    updateContainer(
      container,
      <ComponentThatDispatchesIncreaseAction event initialValue=2 />,
    );

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(3))]);
    validateStructure(rootNode, expectedStructure);
  });

  test("useReducer can update multiple times", () => {
    let rootNode = createRootNode();
    let container = createContainer(rootNode);

    let event: Event.t(unit) = Event.create();

    updateContainer(
      container,
      <ComponentThatDispatchesIncreaseAction event initialValue=2 />,
    );

    Event.dispatch(event, ());
    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(3))]);
    validateStructure(rootNode, expectedStructure);

    Event.dispatch(event, ());
    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(4))]);
    validateStructure(rootNode, expectedStructure);

    Event.dispatch(event, ());
    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(5))]);
    validateStructure(rootNode, expectedStructure);
  });

  module ComponentThatDispatchesIncreaseActionAndRendersChildren = (
    val createComponent(
          (render, ~children, ~event: Event.t(unit), ~initialValue: int, ()) =>
          render(
            () =>
              useReducerExperimental(reducer, initialValue, ((s, dispatch)) =>
                useEffectExperimental(
                  () => {
                    let unsubscribe =
                      Event.subscribe(event, () => dispatch(Increase));
                    () => unsubscribe();
                  },
                  () => <aComponent testVal=s> ...children </aComponent>,
                )
              ),
            ~children,
          )
        )
  );

  test("nested state works as expected", () => {
    let rootNode = createRootNode();
    let container = createContainer(rootNode);

    let outerEvent = Event.create();
    let innerEvent = Event.create();

    updateContainer(
      container,
      <ComponentThatDispatchesIncreaseActionAndRendersChildren
        event=outerEvent initialValue=2>
        <ComponentThatDispatchesIncreaseAction
          event=innerEvent
          initialValue=12
        />
      </ComponentThatDispatchesIncreaseActionAndRendersChildren>,
    );

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeNode(A(2), [TreeLeaf(A(12))])]);
    validateStructure(rootNode, expectedStructure);

    Event.dispatch(outerEvent, ());
    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeNode(A(3), [TreeLeaf(A(12))])]);
    validateStructure(rootNode, expectedStructure);

    Event.dispatch(innerEvent, ());
    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeNode(A(3), [TreeLeaf(A(13))])]);
    validateStructure(rootNode, expectedStructure);

    Event.dispatch(outerEvent, ());
    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeNode(A(4), [TreeLeaf(A(13))])]);
    validateStructure(rootNode, expectedStructure);
  });
});