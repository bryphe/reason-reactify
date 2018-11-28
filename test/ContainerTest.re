/** Simple test cases */
open Rejest;

open TestReconciler;

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

test("Container", () => {
  test("beginReconcile / endReconcile are called for updateContainer", () => {
    let rootNode = createRootNode();

    let beginCount = ref(0);
    let endCount = ref(0);

    let onBeginReconcile = _node => beginCount := beginCount^ + 1;

    let onEndReconcile = _node => endCount := endCount^ + 1;

    let container =
      TestReact.createContainer(~onBeginReconcile, ~onEndReconcile, rootNode);

    TestReact.updateContainer(container, <bComponent />);

    assert(beginCount^ == 1);
    assert(endCount^ == 1);
  });

  let componentThatUpdatesState = (~children, ~event: Event.t(int), ()) =>
    TestReact.component(
      () => {
        let (s, setS) = TestReact.useState(2);

        print_endline("Value: " ++ string_of_int(s));
        TestReact.useEffect(() => {
          let unsubscribe = Event.subscribe(event, v => setS(v));
          () => unsubscribe();
        });

        <aComponent testVal=s />;
      },
      ~uniqueId="componentThatUpdatesState",
      ~children,
    );

  test("beginReconcile / endReconcile are called when updating state", () => {
    let rootNode = createRootNode();

    let beginCount = ref(0);
    let endCount = ref(0);

    let onBeginReconcile = _node => beginCount := beginCount^ + 1;

    let onEndReconcile = _node => endCount := endCount^ + 1;

    let container =
      TestReact.createContainer(~onBeginReconcile, ~onEndReconcile, rootNode);

    let event: Event.t(int) = Event.create();
    TestReact.updateContainer(container, <componentThatUpdatesState event />);

    beginCount := 0;
    endCount := 0;
    Event.dispatch(event, 5);

    assert(beginCount^ == 1);
    assert(endCount^ == 1);
  });
});
