/** HooksUseState **/
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

let noop = () => ();

let componentWithState =
    (~children, ()) =>

      let (s, setS) = TestReact.useState(2)

      <aComponent testVal=s />;
    },
    ~children,
  );

test("useState uses initial state", () => {
  let rootNode = createRootNode();
  let container = TestReact.createContainer(rootNode);

  let expectedStructure: tree(primitives) = TreeNode(Root, [TreeLeaf(A(2))]);

  TestReact.updateContainer(
    container,
    <componentWithState />,
  );

  validateStructure(rootNode, expectedStructure);
});
