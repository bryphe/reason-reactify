/** Simple test cases */
open TestReconciler;
open TestUtility;

/* Use our Reconciler to create our own instance */
module TestReact = Reactify.Make(TestReconciler);

let createRootNode = () => {children: ref([]), nodeId: 0, nodeType: Root};

let aComponent = (~testVal, ~children, ()) =>
  TestReact.primitiveComponent(A(testVal), ~children);
let bComponent = (~children, ()) =>
  TestReact.primitiveComponent(B, ~children);
let cComponent = (~children, ()) =>
  TestReact.primitiveComponent(C, ~children);

test("State conversion works with ints", () => {
    let i = 1;
    let stateI = Reactify.State.to_state(i);
    let rehydratedI = Reactify.State.of_state(stateI);
    assert(i == rehydratedI);
});

test("State conversion works with tuples", () => {
    let p = (1, "a");
    let stateP = Reactify.State.to_state(p);
    let rehydratedP = Reactify.State.of_state(stateP);
    assert(p == rehydratedP);
});

test("State can be used to create list of different types", () => {
    let stateList: list(Reactify.State.t) = [];

    let stateList = [Reactify.State.to_state(1), ...stateList];
    let stateList = [Reactify.State.to_state(("a", "b")), ...stateList];

    let firstElement = List.nth(stateList, 0);
    assert(Reactify.State.of_state(firstElement) == ("a", "b"));

    let lastElement = List.nth(stateList, 1);
    assert(Reactify.State.of_state(lastElement) == 1);
});

let componentWrappingB = (~children, ()) => TestReact.component(() => {
        <bComponent />
}, ~children);

test("Rendering simple wrapped component", () => {
  let rootNode = createRootNode();
  let container = TestReact.createContainer(rootNode);

  let expectedStructure: tree(primitives) =
    TreeNode(
      Root,
      [TreeLeaf(B)],
    );

  TestReact.updateContainer(container, <componentWrappingB />);

  validateStructure(rootNode, expectedStructure);
});

 let componentWrappingAWithProps = (~children, ~v, ()) => TestReact.component(() => <aComponent testVal=v />, ~children);

test("Rendering wrapped component with prop", () => {
  let rootNode = createRootNode();
  let container = TestReact.createContainer(rootNode);

  let expectedStructure: tree(primitives) =
    TreeNode(
      Root,
      [TreeLeaf(A(7))],
    );

  TestReact.updateContainer(container, <componentWrappingAWithProps v=7 />);

  validateStructure(rootNode, expectedStructure);
});

test("Rendering wrapped component multiple times with prop", () => {
  let rootNode = createRootNode();
  let container = TestReact.createContainer(rootNode);

  let expectedStructure: tree(primitives) =
    TreeNode(
      Root,
      [TreeLeaf(A(7))],
    );

  TestReact.updateContainer(container, <componentWrappingAWithProps v=7 />);
  validateStructure(rootNode, expectedStructure);

  TestReact.updateContainer(container, <componentWrappingAWithProps v=7 />);
  validateStructure(rootNode, expectedStructure);

  let expectedStructure: tree(primitives) =
    TreeNode(
      Root,
      [TreeLeaf(A(8))],
    );

  TestReact.updateContainer(container, <componentWrappingAWithProps v=8 />);
  validateStructure(rootNode, expectedStructure);

  let expectedStructure: tree(primitives) =
    TreeNode(
      Root,
      [TreeLeaf(A(9))],
    );

  TestReact.updateContainer(container, <componentWrappingAWithProps v=9 />);
  validateStructure(rootNode, expectedStructure);
});


test("Replace primitive component to wrapped component", () => {
  let rootNode = createRootNode();
  let container = TestReact.createContainer(rootNode);

  let expectedStructure: tree(primitives) =
    TreeNode(
      Root,
      [TreeLeaf(B)],
    );

  TestReact.updateContainer(container, <bComponent />);
  validateStructure(rootNode, expectedStructure);

  TestReact.updateContainer(container, <componentWrappingB />);
  validateStructure(rootNode, expectedStructure);
});


test("Replace wrapped component with primitive component", () => {
  let rootNode = createRootNode();
  let container = TestReact.createContainer(rootNode);

  let expectedStructure: tree(primitives) =
    TreeNode(
      Root,
      [TreeLeaf(B)],
    );

  TestReact.updateContainer(container, <componentWrappingB />);
  validateStructure(rootNode, expectedStructure);

  TestReact.updateContainer(container, <bComponent />);
  validateStructure(rootNode, expectedStructure);
});

let componentWithWrappedComponents = (~children, ()) => TestReact.component(() => {
    <aComponent testVal=1>
        <componentWrappingB />
      </aComponent>
}, ~children);

test("Rendering wrapped component with wrappedComponent as child prop", () => {
  let rootNode = createRootNode();
  let container = TestReact.createContainer(rootNode);

  let expectedStructure: tree(primitives) =
    TreeNode(
      Root,
      [TreeNode(A(1), [TreeLeaf(B)])]
    );

  TestReact.updateContainer(container, <componentWithWrappedComponents />);

  validateStructure(rootNode, expectedStructure);
});

let componentThatRendersChildren = (~children, ()) => TestReact.component(() => {
    <aComponent testVal=1>
        ...children
    </aComponent>
}, ~children);

test("Rendering component that renders primitive child", () => {
  let rootNode = createRootNode();
  let container = TestReact.createContainer(rootNode);

  let expectedStructure: tree(primitives) =
    TreeNode(
      Root,
      [TreeNode(A(1), [TreeLeaf(B)])]
    );

  TestReact.updateContainer(container, <componentThatRendersChildren><bComponent /></componentThatRendersChildren>);

  validateStructure(rootNode, expectedStructure);
});

test("Rendering component that renders component child", () => {
  let rootNode = createRootNode();
  let container = TestReact.createContainer(rootNode);

  let expectedStructure: tree(primitives) =
    TreeNode(
      Root,
      [TreeNode(A(1), [TreeLeaf(B)])]
    );

  TestReact.updateContainer(container, <componentThatRendersChildren><componentWrappingB /></componentThatRendersChildren>);

  validateStructure(rootNode, expectedStructure);
});
