/** HooksUseEffectTest **/
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

let noop = () => ();

module ComponentWithEffectOnMount = (
  val createComponent(
        (
          render,
          ~children,
          ~functionToCallOnMount,
          ~functionToCallOnUnmount,
          (),
        ) =>
        render(
          () =>
            useEffectExperimental(
              () => {
                functionToCallOnMount();
                () => functionToCallOnUnmount();
              },
              () => <bComponent />,
            ),
          ~children,
        )
      )
);

module ComponentWithEmptyConditionalEffect = (
  val createComponent(
        (
          render,
          ~children,
          ~functionToCallOnMount,
          ~functionToCallOnUnmount,
          (),
        ) =>
        render(
          () =>
            /* Hooks */
            useEffectExperimental(
              ~condition=MountUnmount,
              () => {
                functionToCallOnMount();
                () => functionToCallOnUnmount();
              },
              () => <bComponent />,
            ),
          ~children,
        )
      )
);

test("useEffect", () => {
  test("useEffect is called on render", () => {
    let rootNode = createRootNode();
    let container = createContainer(rootNode);

    let v = ref(0);
    let mutate = () => v := v^ + 1;

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(B)]);

    updateContainer(
      container,
      <ComponentWithEffectOnMount
        functionToCallOnMount=mutate
        functionToCallOnUnmount=noop
      />,
    );

    validateStructure(rootNode, expectedStructure);
    assert(v^ == 1);
  });

  test("useEffect handles case when component is removed", () => {
    let rootNode = createRootNode();
    let container = createContainer(rootNode);

    let v = ref(0);
    let r = ref(0);
    let mount = () => v := v^ + 1;
    let unmount = () => r := r^ + 1;

    let expectedStructure: tree(primitives) =
      TreeNode(Root, [TreeLeaf(B)]);

    updateContainer(
      container,
      <ComponentWithEffectOnMount
        functionToCallOnMount=mount
        functionToCallOnUnmount=unmount
      />,
    );
    validateStructure(rootNode, expectedStructure);
    assert(v^ == 1);

    updateContainer(container, <bComponent />);
    validateStructure(rootNode, expectedStructure);
    assert(v^ == 1);
    assert(r^ == 1);
  });

  test("useEffect without a condition is called for each render", () => {
    let rootNode = createRootNode();
    let container = createContainer(rootNode);

    let v = ref(0);
    let mutate = () => v := v^ + 1;

    updateContainer(
      container,
      <ComponentWithEffectOnMount
        functionToCallOnMount=mutate
        functionToCallOnUnmount=noop
      />,
    );
    updateContainer(
      container,
      <ComponentWithEffectOnMount
        functionToCallOnMount=mutate
        functionToCallOnUnmount=noop
      />,
    );

    assert(v^ == 2);
  });

  test("useEffect with an empty condition is called only once", () => {
    let rootNode = createRootNode();
    let container = createContainer(rootNode);

    let v = ref(0);
    let mutate = () => v := v^ + 1;

    updateContainer(
      container,
      <ComponentWithEmptyConditionalEffect
        functionToCallOnMount=mutate
        functionToCallOnUnmount=noop
      />,
    );
    updateContainer(
      container,
      <ComponentWithEmptyConditionalEffect
        functionToCallOnMount=mutate
        functionToCallOnUnmount=noop
      />,
    );

    expect(v^).toBe(1);
  });
});