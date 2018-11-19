/** HooksUseEffectTest **/
open Rejest;

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

let componentWithEffectOnMount =
    (~children, ~functionToCallOnMount, ~functionToCallOnUnmount, ()) =>
  TestReact.component(
    () => {
      TestReact.useEffect(() => {
        functionToCallOnMount();
        () => functionToCallOnUnmount();
      });

      <bComponent />;
    },
    ~children,
  );

test("useEffect", () => {
  test("useEffect is called on render", () => {
    let rootNode = createRootNode();
    let container = TestReact.createContainer(rootNode);

    let v = ref(0);
    let mutate = () => v := v^ + 1;

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(B)]);

    TestReact.updateContainer(
      container,
      <componentWithEffectOnMount
        functionToCallOnMount=mutate
        functionToCallOnUnmount=noop
      />,
    );

    validateStructure(rootNode, expectedStructure);
    assert(v^ == 1);
  });

  test("useEffect handles case when component is removed", () => {
    let rootNode = createRootNode();
    let container = TestReact.createContainer(rootNode);

    let v = ref(0);
    let r = ref(0);
    let mount = () => v := v^ + 1;
    let unmount = () => r := r^ + 1;

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(B)]);

    TestReact.updateContainer(
      container,
      <componentWithEffectOnMount
        functionToCallOnMount=mount
        functionToCallOnUnmount=unmount
      />,
    );
    validateStructure(rootNode, expectedStructure);
    assert(v^ == 1);

    TestReact.updateContainer(container, <bComponent />);
    validateStructure(rootNode, expectedStructure);
    assert(v^ == 1);
    assert(r^ == 1);
  });
});
