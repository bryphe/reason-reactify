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

test("HooksUseContext", () => {
  test("uses default value", () => {
    let rootNode = createRootNode();
    let container = TestReact.createContainer(rootNode);

    let testContext = TestReact.createContext(2);

    let componentThatUsesContext = (~children, ()) =>
      TestReact.component(
        () => {
          let ctx = TestReact.useContext(testContext);

          <aComponent testVal=ctx />;
        },
        ~children,
      );

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(2))]);

    TestReact.updateContainer(container, <componentThatUsesContext />);
    validateStructure(rootNode, expectedStructure);
  });

  test("uses provider value", () => {
    let rootNode = createRootNode();
    let container = TestReact.createContainer(rootNode);

    let testContext = TestReact.createContext(2);
    let provider = TestReact.getProvider(testContext);

    let componentThatUsesContext = (~children, ()) =>
      TestReact.component(
        () => {
          let ctx = TestReact.useContext(testContext);

          <provider value=9> <aComponent testVal=ctx /> </provider>;
        },
        ~children,
      );

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(2))]);

    TestReact.updateContainer(container, <componentThatUsesContext />);
    validateStructure(rootNode, expectedStructure);
  });

  test("uses nested provider value", () => {
    let rootNode = createRootNode();
    let container = TestReact.createContainer(rootNode);

    let testContext = TestReact.createContext(2);
    let provider = TestReact.getProvider(testContext);

    let componentThatUsesContext = (~children, ()) =>
      TestReact.component(
        () => {
          let ctx = TestReact.useContext(testContext);
          <aComponent testVal=ctx />;
        },
        ~children,
      );

    TestReact.updateContainer(
      container,
      <provider value=9>
        <provider value=10> <componentThatUsesContext /> </provider>
      </provider>,
    );

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(10))]);
    validateStructure(rootNode, expectedStructure);
  });
});
