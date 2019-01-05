/** HooksUseState **/
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

module ComponentWithState = (
  val createComponent((render, ~children, ()) =>
        render(
          () => useStateExperimental(2, ((s, _setS)) => <aComponent testVal=s />),
          ~children,
        )
      )
);

type renderOption =
  /* | Nothing */
  | RenderAComponentWithState
  | RenderAComponent(int);

test("useState", () => {
  test("useState uses initial state", () => {
    let rootNode = createRootNode();
    let container = createContainer(rootNode);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(2))]);

    updateContainer(container, <ComponentWithState />);

    validateStructure(rootNode, expectedStructure);
  });

  module ComponentThatUpdatesState = (
    val createComponent((render, ~children, ~event: Event.t(int), ()) =>
          render(
            () =>
              useStateExperimental(2, ((s, setS)) =>
                useEffectExperimental(
                  () => {
                    let unsubscribe = Event.subscribe(event, v => setS(v));
                    () => unsubscribe();
                  },
                  () => <aComponent testVal=s />,
                )
              ),
            ~children,
          )
        )
  );

  module ComponentThatUpdatesMultipleStates = (
    val component((render, ~children, ~event: Event.t((int, int, int)), ()) =>
          render(
            () => {
              useStateExperimental(1, ((s1, setS1)) => {
              useStateExperimental(2, ((s2, setS2)) => {
              useStateExperimental(3, ((s3, setS3)) => {

              useEffectExperimental(() => {
                let unsubscribe = Event.subscribe(event, ((v1, v2, v3)) => {
                  setS1(v1);  
                  setS2(v2);
                  setS3(v3);
                });
                () => unsubscribe();
              }, () => {
                  <bComponent>
                      <aComponent testVal=s1 />
                      <aComponent testVal=s2 />
                      <aComponent testVal=s3 />
                  </bComponent>
              });

              })  ;
              });   
            });
            },
            ~children,
          )
        )
  );

  test("useState updates state with set function", () => {
    let rootNode = createRootNode();

    let container = createContainer(rootNode);

    let event: Event.t(int) = Event.create();

    updateContainer(container, <ComponentThatUpdatesState event />);

    Event.dispatch(event, 5);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(5))]);
    validateStructure(rootNode, expectedStructure);
  });

  test("useState doesn't leak state between components", () => {
    let rootNode = createRootNode();

    let container = createContainer(rootNode);

    let event: Event.t(int) = Event.create();

    updateContainer(container, <ComponentThatUpdatesState event />);

    Event.dispatch(event, 5);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(5))]);
    validateStructure(rootNode, expectedStructure);

    updateContainer(container, <ComponentWithState />);

    /* The 'componentWithState' should have its own state, so it should revert back to 2 - */
    /* and not pick up the state from the previous component */
    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(2))]);
    validateStructure(rootNode, expectedStructure);
  });

  test("useState set state persists across renders", () => {
    let rootNode = createRootNode();

    let container = createContainer(rootNode);

    let event: Event.t(int) = Event.create();

    updateContainer(container, <ComponentThatUpdatesState event />);

    Event.dispatch(event, 5);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(5))]);
    validateStructure(rootNode, expectedStructure);

    updateContainer(container, <ComponentThatUpdatesState event />);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(5))]);
    validateStructure(rootNode, expectedStructure);
  });

  test("multiple setState calls", () => {
    let rootNode = createRootNode();

    let container = createContainer(rootNode);

    let event: Event.t((int, int, int)) = Event.create();

    updateContainer(container, <ComponentThatUpdatesMultipleStates event />);
    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeNode(B, [TreeLeaf(A(1)), TreeLeaf(A(2)), TreeLeaf(A(3))])]);
    validateStructure(rootNode, expectedStructure);

    Event.dispatch(event, (10, 11, 12));

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeNode(B, [TreeLeaf(A(10)), TreeLeaf(A(11)), TreeLeaf(A(12))])]);
    validateStructure(rootNode, expectedStructure);

    updateContainer(container, <ComponentThatUpdatesMultipleStates event />);
    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeNode(B, [TreeLeaf(A(10)), TreeLeaf(A(11)), TreeLeaf(A(12))])]);
    validateStructure(rootNode, expectedStructure);
  });

  test("useState can update multiple times", () => {
    let rootNode = createRootNode();
    let container = createContainer(rootNode);

    let event: Event.t(int) = Event.create();

    updateContainer(container, <ComponentThatUpdatesState event />);

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

  module ComponentThatUpdatesStateAndRendersChildren = (
    val createComponent((render, ~children, ~event: Event.t(int), ()) =>
          render(
            () =>
              useStateExperimental(2, ((s, setS)) =>
                useEffectExperimental(
                  () => {
                    let unsubscribe = Event.subscribe(event, v => setS(v));
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
      <ComponentThatUpdatesStateAndRendersChildren event=outerEvent>
        <ComponentThatUpdatesState event=innerEvent />
      </ComponentThatUpdatesStateAndRendersChildren>,
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

  module ComponentThatWrapsEitherPrimitiveOrComponent = (
    val createComponent(
          (render, ~children, ~event: Event.t(renderOption), ()) =>
          render(
            () =>
              /* Hooks */
              useStateExperimental(RenderAComponentWithState, ((s, setS)) =>
                useEffectExperimental(
                  () => {
                    let unsubscribe = Event.subscribe(event, v => setS(v));
                    () => unsubscribe();
                  },
                  () =>
                    switch (s) {
                    /* | Nothing => () */
                    | RenderAComponentWithState => <ComponentWithState />
                    | RenderAComponent(x) => <aComponent testVal=x />
                    },
                )
              ),
            ~children,
          )
        )
  );

  test("switching between a component to a primitive and back works", () => {
    let rootNode = createRootNode();
    let container = createContainer(rootNode);

    let boolEvent: Event.t(renderOption) = Event.create();
    updateContainer(
      container,
      <ComponentThatWrapsEitherPrimitiveOrComponent event=boolEvent />,
    );

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(2))]);
    validateStructure(rootNode, expectedStructure);

    Event.dispatch(boolEvent, RenderAComponent(3));
    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(3))]);
    validateStructure(rootNode, expectedStructure);

    Event.dispatch(boolEvent, RenderAComponentWithState);
    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(2))]);
    validateStructure(rootNode, expectedStructure);

    Event.dispatch(boolEvent, RenderAComponent(5));
    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(5))]);
    validateStructure(rootNode, expectedStructure);
  });
});
