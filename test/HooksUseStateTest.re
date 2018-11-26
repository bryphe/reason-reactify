/** HooksUseState **/
open Rejest;

open TestReconciler;
open TestUtility;

module Event = Reactify.Event;

/* Use our Reconciler to create our own instance */
module TestReact = Reactify.Make(TestReconciler);

let createRootNode = () => {children: ref([]), nodeId: 0, nodeType: Root};

let aComponent = (~testVal, ~children, ()) =>
  TestReact.primitiveComponent(A(testVal), ~children);
let bComponent = (~children, ()) =>
  TestReact.primitiveComponent(B, ~children);
let cComponent = (~children, ()) =>
  TestReact.primitiveComponent(C, ~children);

let componentWithState = (~children, ()) =>
  TestReact.component(
    () => {
      let (s, _setS) = TestReact.useState(2);
      <aComponent testVal=s />;
    },
    ~children,
  );

type renderOption =
  /* | Nothing */
  | ComponentWithState
  | AComponent(int);

test("useState", () => {
  test("useState uses initial state", () => {
    let rootNode = createRootNode();
    let container = TestReact.createContainer(rootNode);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(2))]);

    TestReact.updateContainer(container, <componentWithState />);

    validateStructure(rootNode, expectedStructure);
  });

  let componentThatUpdatesState = (~children, ~event: Event.t(int), ()) =>
    TestReact.component(
      () => {
        let (s, setS) = TestReact.useState(2);

        print_endline("Value: " ++ string_of_int(s));
        TestReact.useEffect(() => {
          let unsubscribe = Event.subscribe(event, v => setS(v));
          () => unsubscribe();
        });

        <aComponent testVal=s />;
      },
      ~children,
    );

  test("useState updates state with set function", () => {
    let rootNode = createRootNode();

    let container = TestReact.createContainer(rootNode);

    let event: Event.t(int) = Event.create();

    TestReact.updateContainer(container, <componentThatUpdatesState event />);

    Event.dispatch(event, 5);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(5))]);
    validateStructure(rootNode, expectedStructure);
  });

  test("useState doesn't leak state between components", () => {
    let rootNode = createRootNode();

    let container = TestReact.createContainer(rootNode);

    let event: Event.t(int) = Event.create();

    TestReact.updateContainer(container, <componentThatUpdatesState event />);

    Event.dispatch(event, 5);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(5))]);
    validateStructure(rootNode, expectedStructure);

    TestReact.updateContainer(container, <componentWithState />);

    /* The 'componentWithState' should have its own state, so it should revert back to 2 - */
    /* and not pick up the state from the previous component */
    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(2))]);
    validateStructure(rootNode, expectedStructure);
  });

  test("useState can update multiple times", () => {
    let rootNode = createRootNode();
    let container = TestReact.createContainer(rootNode);

    let event: Event.t(int) = Event.create();

    TestReact.updateContainer(container, <componentThatUpdatesState event />);

    Event.dispatch(event, 5);
    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(5))]);
    validateStructure(rootNode, expectedStructure);

    Event.dispatch(event, 6);
    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(6))]);
    validateStructure(rootNode, expectedStructure);

    Event.dispatch(event, 7);
    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(7))]);
    validateStructure(rootNode, expectedStructure);
  });

  let componentThatUpdatesStateAndRendersChildren =
      (~children, ~event: Event.t(int), ()) =>
    TestReact.component(
      () => {
        let (s, setS) = TestReact.useState(2);

        print_endline("Value: " ++ string_of_int(s));
        TestReact.useEffect(() => {
          let unsubscribe = Event.subscribe(event, v => setS(v));
          () => unsubscribe();
        });

        <aComponent testVal=s> ...children </aComponent>;
      },
      ~children,
    );

  test("nested state works as expected", () => {
    let rootNode = createRootNode();
    let container = TestReact.createContainer(rootNode);

    let outerEvent = Event.create();
    let innerEvent = Event.create();

    TestReact.updateContainer(
      container,
      <componentThatUpdatesStateAndRendersChildren event=outerEvent>
        <componentThatUpdatesState event=innerEvent />
      </componentThatUpdatesStateAndRendersChildren>,
    );

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeNode(A(2), [TreeLeaf(A(2))])]);
    validateStructure(rootNode, expectedStructure);

    Event.dispatch(outerEvent, 5);
    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeNode(A(5), [TreeLeaf(A(2))])]);
    validateStructure(rootNode, expectedStructure);

    Event.dispatch(innerEvent, 6);
    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeNode(A(5), [TreeLeaf(A(6))])]);
    validateStructure(rootNode, expectedStructure);

    Event.dispatch(outerEvent, 7);
    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeNode(A(7), [TreeLeaf(A(6))])]);
    validateStructure(rootNode, expectedStructure);
  });

  let componentThatWrapsEitherPrimitiveOrComponent =
      (~children, ~event: Event.t(renderOption), ()) =>
    TestReact.component(
      () => {
        let (s, setS) = TestReact.useState(ComponentWithState);

        TestReact.useEffect(() => {
          let unsubscribe = Event.subscribe(event, v => setS(v));
          () => unsubscribe();
        });

        switch (s) {
        /* | Nothing => () */
        | ComponentWithState => <componentWithState />
        | AComponent(x) => <aComponent testVal=x />
        };
      },
      ~children,
    );

  test("switching between a component to a primitive and back works", () => {
    let rootNode = createRootNode();
    let container = TestReact.createContainer(rootNode);

    let boolEvent: Event.t(renderOption) = Event.create();
    TestReact.updateContainer(
      container,
      <componentThatWrapsEitherPrimitiveOrComponent event=boolEvent />,
    );

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(2))]);
    validateStructure(rootNode, expectedStructure);

    Event.dispatch(boolEvent, AComponent(3));
    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(3))]);
    validateStructure(rootNode, expectedStructure);

    Event.dispatch(boolEvent, ComponentWithState);
    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(2))]);
    validateStructure(rootNode, expectedStructure);

    Event.dispatch(boolEvent, AComponent(5));
    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(5))]);
    validateStructure(rootNode, expectedStructure);
  });
});
