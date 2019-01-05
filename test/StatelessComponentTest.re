/** Simple test cases */
open TestReconciler;
open TestUtility;

open Rejest;

/* Use our Reconciler to create our own instance */
module TestReact = Reactify.Make(TestReconciler);
open TestReact;

let createRootNode = () => {children: ref([]), nodeId: 0, nodeType: Root};

let aComponent = (~testVal, ~children, ()) =>
  primitiveComponent(A(testVal), ~children);
let bComponent = (~children, ()) => primitiveComponent(B, ~children);
let cComponent = (~children, ()) => primitiveComponent(C, ~children);

module ComponentWrappingB = (
  val createComponent((render, ~children, ()) =>
        render(() => <bComponent />, ~children)
      )
);

test("StatelessComponentTest", () => {
  test("Rendering simple wrapped component", () => {
    let rootNode = createRootNode();
    let container = createContainer(rootNode);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(B)]);

    updateContainer(container, <ComponentWrappingB />);

    validateStructure(rootNode, expectedStructure);
  });

  module ComponentWrappingAWithProps = (
    val createComponent((render, ~children, ~v, ()) =>
          render(() => <aComponent testVal=v />, ~children)
        )
  );

  test("Rendering wrapped component with prop", () => {
    let rootNode = createRootNode();
    let container = createContainer(rootNode);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(7))]);

    updateContainer(container, <ComponentWrappingAWithProps v=7 />);

    validateStructure(rootNode, expectedStructure);
  });

  test("Rendering wrapped component multiple times with prop", () => {
    let rootNode = createRootNode();
    let container = createContainer(rootNode);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(7))]);

    updateContainer(container, <ComponentWrappingAWithProps v=7 />);
    validateStructure(rootNode, expectedStructure);

    updateContainer(container, <ComponentWrappingAWithProps v=7 />);
    validateStructure(rootNode, expectedStructure);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(8))]);

    updateContainer(container, <ComponentWrappingAWithProps v=8 />);
    validateStructure(rootNode, expectedStructure);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(9))]);

    updateContainer(container, <ComponentWrappingAWithProps v=9 />);
    validateStructure(rootNode, expectedStructure);
  });

  test("Replace primitive component to wrapped component", () => {
    let rootNode = createRootNode();
    let container = createContainer(rootNode);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(B)]);

    updateContainer(container, <bComponent />);
    validateStructure(rootNode, expectedStructure);

    updateContainer(container, <ComponentWrappingB />);
    validateStructure(rootNode, expectedStructure);
  });

  test("Replace wrapped component with primitive component", () => {
    let rootNode = createRootNode();
    let container = createContainer(rootNode);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(B)]);

    updateContainer(container, <ComponentWrappingB />);
    validateStructure(rootNode, expectedStructure);

    updateContainer(container, <bComponent />);
    validateStructure(rootNode, expectedStructure);
  });

  module ComponentWithWrappedComponents = (
    val createComponent((render, ~children, ()) =>
          render(
            () => <aComponent testVal=1> <ComponentWrappingB /> </aComponent>,
            ~children,
          )
        )
  );

  test("Rendering wrapped component with wrappedComponent as child prop", () => {
    let rootNode = createRootNode();
    let container = createContainer(rootNode);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeNode(A(1), [TreeLeaf(B)])]);

    updateContainer(container, <ComponentWithWrappedComponents />);

    validateStructure(rootNode, expectedStructure);
  });

  module ComponentThatRendersChildren = (
    val createComponent((render, ~children, ()) =>
          render(
            () => <aComponent testVal=1> ...children </aComponent>,
            ~children,
          )
        )
  );

  test("Rendering component that renders primitive child", () => {
    let rootNode = createRootNode();
    let container = createContainer(rootNode);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeNode(A(1), [TreeLeaf(B)])]);

    updateContainer(
      container,
      <ComponentThatRendersChildren>
        <bComponent />
      </ComponentThatRendersChildren>,
    );

    validateStructure(rootNode, expectedStructure);
  });

  test("Rendering component that renders component child", () => {
    let rootNode = createRootNode();
    let container = createContainer(rootNode);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeNode(A(1), [TreeLeaf(B)])]);

    updateContainer(
      container,
      <ComponentThatRendersChildren>
        <ComponentWrappingB />
      </ComponentThatRendersChildren>,
    );

    validateStructure(rootNode, expectedStructure);
  });

  module ComponentWithVisibilityToggle = (
    val createComponent((render, ~visible=true, ~children, ()) =>
          render(
            () =>
              visible ?
                <aComponent testVal=1> ...children </aComponent> :
                TestReact.empty,
            ~children,
          )
        )
  );

  test("Test toggling visibility", () => {
    let rootNode = createRootNode();
    let container = createContainer(rootNode);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeNode(A(1), [TreeLeaf(B)])]);

    updateContainer(
      container,
      <ComponentWithVisibilityToggle visible=true>
        <ComponentWrappingB />
      </ComponentWithVisibilityToggle>,
    );
    validateStructure(rootNode, expectedStructure);

    updateContainer(
      container,
      <ComponentWithVisibilityToggle visible=false>
        <ComponentWrappingB />
      </ComponentWithVisibilityToggle>,
    );
    let expectedStructure: tree(primitives) = TreeLeaf(Root);
    validateStructure(rootNode, expectedStructure);
  });
});