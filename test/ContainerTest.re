/** Simple test cases */
open Rejest;

open TestReconciler;

module Event = Reactify.Event;

/* Use our Reconciler to create our own instance */
module TestReact = Reactify.Make(TestReconciler);
open TestReact;

let createRootNode = () => {children: ref([]), nodeId: 0, nodeType: Root};

let aComponent = (~testVal, ~children, ()) =>
  primitiveComponent(A(testVal), ~children);
let bComponent = (~children, ()) => primitiveComponent(B, ~children);
let cComponent = (~children, ()) => primitiveComponent(C, ~children);

test("Container", () => {
  test("beginReconcile / endReconcile are called for updateContainer", () => {
    let rootNode = createRootNode();

    let beginCount = ref(0);
    let endCount = ref(0);

    let onBeginReconcile = _node => beginCount := beginCount^ + 1;

    let onEndReconcile = _node => endCount := endCount^ + 1;

    let container =
      createContainer(~onBeginReconcile, ~onEndReconcile, rootNode);

    updateContainer(container, <bComponent />);

    assert(beginCount^ == 1);
    assert(endCount^ == 1);
  });

  module ComponentThatUpdatesState = (
    val createComponent((render, ~children, ~event: Event.t(int), ()) =>
          render(
            () =>
              useStateExperimental(
                2,
                ((s, setS)) => {
                  print_endline("Value: " ++ string_of_int(s));
                  useEffectExperimental(
                    () => {
                      let unsubscribe = Event.subscribe(event, v => setS(v));
                      () => unsubscribe();
                    },
                    () => <aComponent testVal=s />,
                  );
                },
              ),
            ~children,
          )
        )
  );

  test("beginReconcile / endReconcile are called when updating state", () => {
    let rootNode = createRootNode();

    let beginCount = ref(0);
    let endCount = ref(0);

    let onBeginReconcile = _node => beginCount := beginCount^ + 1;

    let onEndReconcile = _node => endCount := endCount^ + 1;

    let container =
      createContainer(~onBeginReconcile, ~onEndReconcile, rootNode);

    let event: Event.t(int) = Event.create();
    updateContainer(container, <ComponentThatUpdatesState event />);

    beginCount := 0;
    endCount := 0;
    Event.dispatch(event, 5);

    assert(beginCount^ == 1);
    assert(endCount^ == 1);
  });
});