/** Simple test cases */
open TestReconciler;
open TestUtility;

open Rejest;

/* Use our Reconciler to create our own instance */
module TestReact = Reactify.Make(TestReconciler);

let createRootNode = () => {children: ref([]), nodeId: 0, nodeType: Root};

let aComponent = (~testVal, ~children, ()) =>
  TestReact.primitiveComponent(A(testVal), ~children);
let bComponent = (~children, ()) =>
  TestReact.primitiveComponent(B, ~children);
let cComponent = (~children, ()) =>
  TestReact.primitiveComponent(C, ~children);

let componentWrappingB = (~children, ()) =>
  TestReact.component(() => <bComponent />, 
  ~uniqueId="componentWrappingB",
  ~children);

test("StatelessComponentTest", () => {
  test("Rendering simple wrapped component", () => {
    let rootNode = createRootNode();
    let container = TestReact.createContainer(rootNode);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(B)]);

    TestReact.updateContainer(container, <componentWrappingB />);

    validateStructure(rootNode, expectedStructure);
  });

  let componentWrappingAWithProps = (~children, ~v, ()) =>
    TestReact.component(() => <aComponent testVal=v />, ~uniqueId="componentWrappingAWithProps", ~children);

  test("Rendering wrapped component with prop", () => {
    let rootNode = createRootNode();
    let container = TestReact.createContainer(rootNode);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(7))]);

    TestReact.updateContainer(container, <componentWrappingAWithProps v=7 />);

    validateStructure(rootNode, expectedStructure);
  });

  test("Rendering wrapped component multiple times with prop", () => {
    let rootNode = createRootNode();
    let container = TestReact.createContainer(rootNode);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(7))]);

    TestReact.updateContainer(container, <componentWrappingAWithProps v=7 />);
    validateStructure(rootNode, expectedStructure);

    TestReact.updateContainer(container, <componentWrappingAWithProps v=7 />);
    validateStructure(rootNode, expectedStructure);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(8))]);

    TestReact.updateContainer(container, <componentWrappingAWithProps v=8 />);
    validateStructure(rootNode, expectedStructure);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(9))]);

    TestReact.updateContainer(container, <componentWrappingAWithProps v=9 />);
    validateStructure(rootNode, expectedStructure);
  });

  test("Replace primitive component to wrapped component", () => {
    let rootNode = createRootNode();
    let container = TestReact.createContainer(rootNode);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(B)]);

    TestReact.updateContainer(container, <bComponent />);
    validateStructure(rootNode, expectedStructure);

    TestReact.updateContainer(container, <componentWrappingB />);
    validateStructure(rootNode, expectedStructure);
  });

  test("Replace wrapped component with primitive component", () => {
    let rootNode = createRootNode();
    let container = TestReact.createContainer(rootNode);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(B)]);

    TestReact.updateContainer(container, <componentWrappingB />);
    validateStructure(rootNode, expectedStructure);

    TestReact.updateContainer(container, <bComponent />);
    validateStructure(rootNode, expectedStructure);
  });

  let componentWithWrappedComponents = (~children, ()) =>
    TestReact.component(
      () => <aComponent testVal=1> <componentWrappingB /> </aComponent>,
      ~uniqueId="componentWithWrappedComponents",
      ~children,
    );

  test("Rendering wrapped component with wrappedComponent as child prop", () => {
    let rootNode = createRootNode();
    let container = TestReact.createContainer(rootNode);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeNode(A(1), [TreeLeaf(B)])]);

    TestReact.updateContainer(container, <componentWithWrappedComponents />);

    validateStructure(rootNode, expectedStructure);
  });

  let componentThatRendersChildren = (~children, ()) =>
    TestReact.component(~uniqueId="componentThatRendersChildren",
      () => <aComponent testVal=1> ...children </aComponent>,
      ~children,
    );

  test("Rendering component that renders primitive child", () => {
    let rootNode = createRootNode();
    let container = TestReact.createContainer(rootNode);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeNode(A(1), [TreeLeaf(B)])]);

    TestReact.updateContainer(
      container,
      <componentThatRendersChildren>
        <bComponent />
      </componentThatRendersChildren>,
    );

    validateStructure(rootNode, expectedStructure);
  });

  test("Rendering component that renders component child", () => {
    let rootNode = createRootNode();
    let container = TestReact.createContainer(rootNode);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeNode(A(1), [TreeLeaf(B)])]);

    TestReact.updateContainer(
      container,
      <componentThatRendersChildren>
        <componentWrappingB />
      </componentThatRendersChildren>,
    );

    validateStructure(rootNode, expectedStructure);
  });

  let componentWithVisibilityToggle = (~children, ~visible=true, ()) =>
    TestReact.component(~uniqueId="componentWithVisibilityToggle", () =>
      visible ?
        <aComponent testVal=1> ...children </aComponent> : TestReact.empty
    );

  test("Test toggling visibility", () => {
    let rootNode = createRootNode();
    let container = TestReact.createContainer(rootNode);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeNode(A(1), [TreeLeaf(B)])]);

    TestReact.updateContainer(
      container,
      <componentWithVisibilityToggle visible=true>
        <componentWrappingB />
      </componentWithVisibilityToggle>,
    );
    validateStructure(rootNode, expectedStructure);

    TestReact.updateContainer(
      container,
      <componentWithVisibilityToggle visible=false>
        <componentWrappingB />
      </componentWithVisibilityToggle>,
    );
    let expectedStructure: tree(primitives) = TreeLeaf(Root);
    validateStructure(rootNode, expectedStructure);
  });
});
