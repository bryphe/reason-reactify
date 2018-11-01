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

module Event = {
  type cb('a) = 'a => unit;

  type t('a) = ref(list(cb('a)));

  let create = () => ref([]);

  let subscribe = (evt: t('a), f: cb('a)) =>
    evt := List.append(evt^, [f]);

  let dispatch = (evt: t('a), v: 'a) => List.iter(c => c(v), evt^);
};

let componentWithState = (~children, ()) =>
  TestReact.component(
    () => {
      let (s, _setS) = TestReact.useState(2);
      <aComponent testVal=s />;
    },
    ~children,
  );

test("useState uses initial state", () => {
  let rootNode = createRootNode();
  let container = TestReact.createContainer(rootNode);

  let expectedStructure: tree(primitives) =
    TreeNode(Root, [TreeLeaf(A(2))]);

  TestReact.updateContainer(container, <componentWithState />);

  validateStructure(rootNode, expectedStructure);
});

let componentThatUpdatesState = (~children, ~event: Event.t(int), ()) =>
  TestReact.component(
    () => {
      let (s, setS) = TestReact.useState(2);

      print_endline("Value: " ++ string_of_int(s));
      TestReact.useEffect(() => {
        Event.subscribe(event, v => setS(v));
        noop;
      });

      <aComponent testVal=s />;
    },
    ~children,
  );

test("useState updates state with set function", () => {
  let rootNode = createRootNode();

  let container = TestReact.createContainer(rootNode);

  let event: Event.t(int) = Event.create();

  TestReact.updateContainer(container, <componentThatUpdatesState event />);

  Event.dispatch(event, 5);

  let expectedStructure: tree(primitives) =
    TreeNode(Root, [TreeLeaf(A(5))]);
  validateStructure(rootNode, expectedStructure);
});

/* test("useState can update multiple times", () => { */
/*   let rootNode = createRootNode(); */

/*   let container = TestReact.createContainer(rootNode); */

/*   let event: Event.t(int) = Event.create(); */

/*   TestReact.updateContainer(container, <componentThatUpdatesState event />); */

/*   Event.dispatch(event, 5); */
/*   let expectedStructure: tree(primitives) = */
/*     TreeNode(Root, [TreeLeaf(A(5))]); */
/*   validateStructure(rootNode, expectedStructure); */

/*   Event.dispatch(event, 6); */
/*   let expectedStructure: tree(primitives) = */
/*     TreeNode(Root, [TreeLeaf(A(6))]); */
/*   validateStructure(rootNode, expectedStructure); */

/*   Event.dispatch(event, 7); */
/*   let expectedStructure: tree(primitives) = */
/*     TreeNode(Root, [TreeLeaf(A(7))]); */
/*   validateStructure(rootNode, expectedStructure); */
/* }); */
