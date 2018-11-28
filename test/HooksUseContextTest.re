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

test("HooksUseContext", () => {
  test("uses default value", () => {
    let rootNode = createRootNode();
    let container = createContainer(rootNode);

    let testContext = createContext(2);

    module ComponentThatUsesContext = (
      val component((render, ~children, ()) =>
            render(
              () => {
                let ctx = useContext(testContext);

                <aComponent testVal=ctx />;
              },
              ~children,
            )
          )
    );

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(2))]);

    updateContainer(container, <ComponentThatUsesContext />);
    validateStructure(rootNode, expectedStructure);
  });

  test("uses provider value", () => {
    let rootNode = createRootNode();
    let container = createContainer(rootNode);

    let testContext = createContext(2);
    let provider = getProvider(testContext);

    module ComponentThatUsesContext = (
      val component((render, ~children, ()) =>
            render(
              () => {
                let ctx = useContext(testContext);

                <provider value=9> <aComponent testVal=ctx /> </provider>;
              },
              ~children,
            )
          )
    );

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(2))]);

    updateContainer(container, <ComponentThatUsesContext />);
    validateStructure(rootNode, expectedStructure);
  });

  test("uses nested provider value", () => {
    let rootNode = createRootNode();
    let container = createContainer(rootNode);

    let testContext = createContext(2);
    let provider = getProvider(testContext);

    module ComponentThatUsesContext = (
      val component((render, ~children, ()) =>
            render(
              () => {
                let ctx = useContext(testContext);
                <aComponent testVal=ctx />;
              },
              ~children,
            )
          )
    );

    updateContainer(
      container,
      <provider value=9>
        <provider value=10> <ComponentThatUsesContext /> </provider>
      </provider>,
    );

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(A(10))]);
    validateStructure(rootNode, expectedStructure);
  });
});
