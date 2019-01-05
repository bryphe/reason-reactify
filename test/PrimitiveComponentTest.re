/** Simple test cases */
open Rejest;

open TestReconciler;
open TestUtility;

/* Use our Reconciler to create our own instance */
module TestReact = Reactify.Make(TestReconciler);
open TestReact;

let createRootNode = () => {children: ref([]), nodeId: 0, nodeType: Root};

let aComponent = (~testVal, ~children, ()) =>
  primitiveComponent(A(testVal), ~children);
let bComponent = (~children, ()) => primitiveComponent(B, ~children);
let cComponent = (~children, ()) => primitiveComponent(C, ~children);

module CustomComponent = (
  val createComponent((render, ~children, ()) =>
        render(
          () =>
            <aComponent testVal=1> <bComponent /> <bComponent /> </aComponent>,
          ~children,
        )
      )
);

test("PrimitiveComponent", () => {
  test("BasicRenderTest", () => {
    let rootNode = createRootNode();
    let container = createContainer(rootNode);

    updateContainer(container, <bComponent />);

    let expectedStructure = TreeNode(Root, [TreeLeaf(B)]);
    validateStructure(rootNode, expectedStructure);
  });

  test("BasicRenderTest - multiple updates", () => {
    let rootNode = createRootNode();
    let container = createContainer(rootNode);

    updateContainer(container, <bComponent />);
    updateContainer(container, <bComponent />);

    let expectedStructure = TreeNode(Root, [TreeLeaf(B)]);
    validateStructure(rootNode, expectedStructure);
  });

  test("UpdateNodeTest", () => {
    let rootNode = createRootNode();
    let container = createContainer(rootNode);

    updateContainer(container, <aComponent testVal=1 />);

    let expectedStructure = TreeNode(Root, [TreeLeaf(A(1))]);
    validateStructure(rootNode, expectedStructure);

    /* Now, we'll update the tree */
    updateContainer(container, <aComponent testVal=2 />);

    let expectedStructure = TreeNode(Root, [TreeLeaf(A(2))]);
    validateStructure(rootNode, expectedStructure);
  });

  test("UpdateChildNodeTest", () => {
    let rootNode = createRootNode();
    let container = createContainer(rootNode);

    updateContainer(
      container,
      <aComponent testVal=1> <aComponent testVal=2 /> </aComponent>,
    );

    let expectedStructure =
      TreeNode(Root, [TreeNode(A(1), [TreeLeaf(A(2))])]);
    validateStructure(rootNode, expectedStructure);

    /* Now, we'll update the tree */
    updateContainer(
      container,
      <aComponent testVal=1> <aComponent testVal=3 /> </aComponent>,
    );

    let expectedStructure =
      TreeNode(Root, [TreeNode(A(1), [TreeLeaf(A(3))])]);
    validateStructure(rootNode, expectedStructure);
  });

  test("ReplaceChildNodeTest", () => {
    let rootNode = createRootNode();
    let container = createContainer(rootNode);

    updateContainer(
      container,
      <aComponent testVal=1> <aComponent testVal=2 /> </aComponent>,
    );

    let expectedStructure =
      TreeNode(Root, [TreeNode(A(1), [TreeLeaf(A(2))])]);
    validateStructure(rootNode, expectedStructure);

    /* Now, we'll update the tree */
    updateContainer(
      container,
      <aComponent testVal=1> <bComponent /> </aComponent>,
    );

    show(rootNode);
    let expectedStructure =
      TreeNode(Root, [TreeNode(A(1), [TreeLeaf(B)])]);
    validateStructure(rootNode, expectedStructure);
  });

  test("ReplaceChildrenWithLessChildrenTest", () => {
    let rootNode = createRootNode();
    let container = createContainer(rootNode);

    updateContainer(
      container,
      <aComponent testVal=1>
        <bComponent />
        <bComponent />
        <bComponent />
      </aComponent>,
    );

    let expectedStructure =
      TreeNode(
        Root,
        [TreeNode(A(1), [TreeLeaf(B), TreeLeaf(B), TreeLeaf(B)])],
      );
    validateStructure(rootNode, expectedStructure);

    /* Now, we'll update the tree */
    print_endline("going for the update....");
    updateContainer(
      container,
      <aComponent testVal=1> <bComponent /> </aComponent>,
    );

    let expectedStructure =
      TreeNode(Root, [TreeNode(A(1), [TreeLeaf(B)])]);
    validateStructure(rootNode, expectedStructure);
  });

  test("ReplaceNodeTest", () => {
    let rootNode = createRootNode();
    let container = createContainer(rootNode);

    updateContainer(container, <aComponent testVal=1 />);

    let expectedStructure = TreeNode(Root, [TreeLeaf(A(1))]);
    validateStructure(rootNode, expectedStructure);

    /* Now, we'll update the tree */
    updateContainer(container, <bComponent />);

    let expectedStructure = TreeNode(Root, [TreeLeaf(B)]);
    validateStructure(rootNode, expectedStructure);
  });

  test("RenderingChildrenTest", () => {
    let rootNode = createRootNode();
    let container = createContainer(rootNode);

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeNode(A(1), [TreeLeaf(B), TreeLeaf(B)])]);

    updateContainer(container, <CustomComponent />);

    validateStructure(rootNode, expectedStructure);
  });

  test("Regression Test - update, revert does not re-render node", () => {
    let rootNode = createRootNode();
    let container = createContainer(rootNode);

    updateContainer(container, <aComponent testVal=0 />);
    updateContainer(container, <aComponent testVal=1 />);
    updateContainer(container, <aComponent testVal=0 />);

    let expectedStructure = TreeNode(Root, [TreeLeaf(A(0))]);
    validateStructure(rootNode, expectedStructure);
  });
});
