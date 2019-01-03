open Reactify_Types;

module Make = (ReconcilerImpl: Reconciler) => {
  /* Module to give us unique IDs for components */
  type renderedElement =
    | RenderedPrimitive(ReconcilerImpl.node)
  and elementWithChildren = (list(element), Effects.effects, Context.t)
  and render = unit => elementWithChildren
  and element =
    | Primitive(ReconcilerImpl.primitives, render)
    | Component(ComponentId.t, render)
    | Provider(render)
    | Empty(render)
  /*
      An instance is a component that has been rendered.
      We store some additional context for it, like the state,
      effects that need to be run, and corresponding nodes.
   */
  and instance = {
    mutable element,
    children: list(element),
    node: option(ReconcilerImpl.node),
    rootNode: ReconcilerImpl.node,
    mutable childInstances,
    mutable effectInstances: Effects.effectInstances,
    state: State.HeterogenousMutableList.t,
    context: Context.HeterogenousHashtbl.t,
    container: t,
  }
  and container = {
    onBeginReconcile: Event.t(ReconcilerImpl.node),
    onEndReconcile: Event.t(ReconcilerImpl.node),
    rootInstance: ref(option(instance)),
    containerNode: ReconcilerImpl.node,
  }
  and t = container
  and childInstances = list(instance)
  and hook('t) =
    | Hook(element, 't)
  and state('s) =
    | State('s)
  and reducer('r) =
    | Reducer('r)
  and effect =
    | Effect
  and context('t) =
    | Context('t)
  and emptyHook = hook(unit);

  type node = ReconcilerImpl.node;
  type primitives = ReconcilerImpl.primitives;

  /*
     Internal helpers to aid the hooks type representation,
     the wrapping variants could be optimized away in the future as they're not used at runtime
   */
  let addHook = (newHook, Hook(element, h)) => Hook(element, (h, newHook));
  let addState = (~state as s) => addHook(State(s));
  let addEffect = element => addHook(Effect, element);
  let addReducer = (~reducer as r) => addHook(Reducer(r));
  let addContext = (~value as v) => addHook(Context(v));
  let elementToHook = x => Hook(x, ());
  let elementFromHook = (Hook(x, _)) => x;

  /*
     A global, non-pure container to hold effects
     during the course of a render operation.
   */
  let __globalEffects = Effects.create();

  let _uniqueIdScope = ComponentId.createScope();

  /*
      A global, non-pure container to hold current
      context during hte course of a render operation.
   */
  let noContext = Context.create();
  let __globalContext = ref(noContext);

  /*
     State management for reconciliation
   */
  module ComponentStateContext = {
    type t = instance;
  };
  module ComponentState = State.Make(ComponentStateContext);
  let noState = ComponentState.create([]);
  let __globalState = ref(noState);

  /*
     Container API
   */
  type reconcileNotification = node => unit;

  let createContainer =
      (
        ~onBeginReconcile: option(reconcileNotification)=?,
        ~onEndReconcile: option(reconcileNotification)=?,
        rootNode: ReconcilerImpl.node,
      ) => {
    let be = Event.create();
    let ee = Event.create();

    switch (onBeginReconcile) {
    | Some(x) =>
      let _ = Event.subscribe(be, x);
      ();
    | _ => ()
    };

    switch (onEndReconcile) {
    | Some(x) =>
      let _ = Event.subscribe(ee, x);
      ();
    | _ => ()
    };

    let ret: container = {
      onBeginReconcile: be,
      onEndReconcile: ee,
      containerNode: rootNode,
      rootInstance: ref(None),
    };

    ret;
  };

  let empty: emptyHook =
    elementToHook(Empty(() => ([], [], __globalContext^)));

  let render = (id: ComponentId.t, lazyElement, ~children) => {
    ignore(children);
    let ret: emptyHook =
      elementToHook(
        Component(
          id,
          () => {
            Effects.resetEffects(__globalEffects);
            let childElement = lazyElement() |> elementFromHook;
            let children = [childElement];
            let effects = Effects.getEffects(__globalEffects);
            let renderResult: elementWithChildren = (
              children,
              effects,
              __globalContext^,
            );
            renderResult;
          },
        ),
      );
    ret;
  };

  module type Component = {
    type hooks;
    type createElement;
    let createElement: createElement;
  };

  let createComponent =
      (
        type c,
        type h,
        create:
          ((unit => hook(h), ~children: list(emptyHook)) => emptyHook) => c,
      )
      : (module Component with type createElement = c and type hooks = h) => {
    let id = ComponentId.newId(_uniqueIdScope);
    let boundFunc = create(render(id));
    (module
     {
       type hooks = h;
       type createElement = c;
       let createElement = boundFunc;
     });
  };
  let component = createComponent;

  let primitiveComponent = (~children, prim) => {
    let comp: emptyHook =
      elementToHook(
        Primitive(
          prim,
          () => (List.map(elementFromHook, children), [], __globalContext^),
        ),
      );
    comp;
  };

  /* Context */
  let __contextId = ref(0);
  type providerConstructor('t) =
    (~children: list(emptyHook), ~value: 't, unit) => emptyHook;
  type contextValue('t) = {
    initialValue: 't,
    id: int,
  };

  let createContext = (initialValue: 't) => {
    let contextId = __contextId^;
    __contextId := __contextId^ + 1;
    let ret: contextValue('t) = {initialValue, id: contextId};
    ret;
  };

  let getProvider = ctx => {
    let provider = (~children, ~value, ()) => {
      let ret: emptyHook =
        elementToHook(
          Provider(
            () => {
              let contextId = ctx.id;
              let context = Context.clone(__globalContext^);
              Context.set(context, contextId, Object.to_object(value));
              (List.map(elementFromHook, children), [], context);
            },
          ),
        );
      ret;
    };
    provider;
  };

  let useContext = (ctx: contextValue('t)) => {
    let value =
      switch (Context.get(__globalContext^, ctx.id)) {
      | Some(x) => Object.of_object(x)
      | None => ctx.initialValue
      };
    value;
  };

  let useContextExperimental = (ctx: contextValue('t), continuation) => {
    let value = useContext(ctx);
    continuation(value) |> addContext(~value);
  };

  let useEffect =
      (
        ~condition: Effects.effectCondition=Effects.Always,
        e: Effects.effectFunction,
      ) =>
    Effects.addEffect(~condition, __globalEffects, e);

  let useEffectExperimental =
      (
        ~condition: Effects.effectCondition=Effects.Always,
        e: Effects.effectFunction,
        continuation,
      ) => {
    useEffect(~condition, e);
    continuation() |> addEffect;
  };

  let _getEffectsFromInstance = (instance: option(instance)) =>
    switch (instance) {
    | None => []
    | Some(i) => i.effectInstances
    };

  let _getPreviousChildInstances = (instance: option(instance)) =>
    switch (instance) {
    | None => []
    | Some(i) => i.childInstances
    };

  let _getCurrentStateFromInstance = (instance: option(instance)) =>
    switch (instance) {
    | None => []
    | Some(i) => i.state
    };

  let rec getFirstNode = (node: instance) =>
    switch (node.node) {
    | Some(n) => Some(n)
    | None =>
      switch (node.childInstances) {
      | [] => None
      | [c] => getFirstNode(c)
      | _ => None
      }
    };

  let isInstanceOfComponent = (instance: option(instance), element: element) =>
    switch (instance) {
    | None => false
    | Some(x) =>
      switch (x.element, element) {
      | (Primitive(a, _), Primitive(b, _)) =>
        Utility.areConstructorsEqual(a, b)
      | (Component(a, _), Component(b, _)) => a === b
      | _ => x.element === element
      }
    };

  /*
   * Instantiate turns a component function into a live instance,
   * and asks the reconciler to append it to the root node.
   */
  let rec instantiate =
          (
            rootNode,
            previousInstance: option(instance),
            element: element,
            context: Context.t,
            container: t,
          ) => {
    let previousState = ref([]);

    /* Recycle any previous effect instances */
    let previousEffectInstances = _getEffectsFromInstance(previousInstance);

    let isSameInstanceAsBefore =
      isInstanceOfComponent(previousInstance, element);

    if (isSameInstanceAsBefore) {
      /* Set up state for the component */
      previousState := _getCurrentStateFromInstance(previousInstance);
    };

    let state = ComponentState.create(previousState^);
    /* We hold onto a reference to the component instance - we need to set this _after_ the component is instantiated */
    let stateContext = ComponentState.getCurrentContext(state);

    /*
         This is dirty, but we set the 'global' state so that the 'useState'
         can have access to it, without additional binding or passing. This is
         necessary to preserve the semantics of the React-style API
     */
    __globalState := state;
    __globalContext := context;
    let (children, effects, newContext) =
      switch (element) {
      | Primitive(_, render)
      | Component(_, render)
      | Provider(render)
      | Empty(render) => render()
      };
    /* Once rendering is complete, we don't need this anymore */
    __globalContext := noContext;
    __globalState := noState;
    let newState = ComponentState.getNewState(state);

    let newEffectCount = List.length(effects);

    let newEffectInstances =
      isSameInstanceAsBefore ?
        Effects.runEffects(
          ~previousInstances=previousEffectInstances,
          effects,
        ) :
        {
          Effects.drainEffects(previousEffectInstances);
          let emptyInstances =
            Effects.createEmptyEffectInstances(newEffectCount);
          Effects.runEffects(~previousInstances=emptyInstances, effects);
        };

    let primitiveInstance =
      switch (element) {
      | Primitive(p, _render) => Some(ReconcilerImpl.createInstance(p))
      | _ => None
      };

    let nextRootPrimitiveInstance =
      switch (primitiveInstance) {
      | Some(i) => i
      | None => rootNode
      };

    let previousChildInstances = _getPreviousChildInstances(previousInstance);
    let childInstances =
      reconcileChildren(
        nextRootPrimitiveInstance,
        previousChildInstances,
        children,
        newContext,
        container,
      );

    let instance: instance = {
      element,
      node: primitiveInstance,
      rootNode: nextRootPrimitiveInstance,
      children,
      childInstances,
      effectInstances: newEffectInstances,
      state: newState,
      context: newContext,
      container,
    };

    /*
         'context' is the instance that state needs when 'setState' is called
         We set it here, after the instance is fully realized, so that the 'setState'
         callback has the latest state for the component instance.
     */
    stateContext := Some(instance);

    instance;
  }
  and reconcile = (rootNode, instance, component, context, container) => {
    let newInstance =
      instantiate(rootNode, instance, component, context, container);

    let r =
      switch (instance) {
      | None =>
        switch (newInstance.node) {
        | Some(n) => ReconcilerImpl.appendChild(rootNode, n)
        | None => ()
        };

        newInstance;
      | Some(i) =>
        let ret =
          switch (newInstance.node, i.node) {
          | (Some(a), Some(b)) =>
            /* Only both replacing node if the primitives are different */
            switch (newInstance.element, i.element) {
            | (Primitive(newPrim, _), Primitive(oldPrim, _)) =>
              if (oldPrim !== newPrim) {
                /* Check if the primitive type is the same - if it is, we can simply update the node */
                /* If not, we'll replace the node */
                if (Utility.areConstructorsEqual(oldPrim, newPrim)) {
                  ReconcilerImpl.updateInstance(b, oldPrim, newPrim);
                  i.element = newInstance.element;
                  i.effectInstances = newInstance.effectInstances;
                  i.childInstances =
                    reconcileChildren(
                      b,
                      i.childInstances,
                      newInstance.children,
                      context,
                      container,
                    );
                  i;
                } else {
                  ReconcilerImpl.replaceChild(rootNode, a, b);
                  newInstance;
                };
              } else {
                /* The node itself is unchanged, so we'll just reconcile the children */
                i.effectInstances = newInstance.effectInstances;
                i.childInstances =
                  reconcileChildren(
                    b,
                    i.childInstances,
                    newInstance.children,
                    context,
                    container,
                  );
                i;
              }
            | _ =>
              print_endline(
                "ERROR: Should only be nodes if there are primitives!",
              );
              newInstance;
            }
          | (Some(a), None) =>
            /* If there was a non-primitive instance, we need to get the top-level node - */
            /* and then remove it */
            let currentNode = getFirstNode(i);
            switch (currentNode) {
            | Some(c) => ReconcilerImpl.removeChild(rootNode, c)
            | _ => ()
            };
            ReconcilerImpl.appendChild(rootNode, a);
            newInstance;
          | (None, Some(b)) =>
            ReconcilerImpl.removeChild(rootNode, b);
            newInstance;
          | (None, None) => newInstance
          };

        ret;
      };
    r;
  }
  and reconcileChildren =
      (
        root: node,
        currentChildInstances: childInstances,
        newChildren: list(element),
        context: Context.t,
        container: t,
      ) => {
    let currentChildInstances: array(instance) =
      Array.of_list(currentChildInstances);
    let newChildren = Array.of_list(newChildren);

    let newChildInstances: ref(childInstances) = ref([]);

    for (i in 0 to Array.length(newChildren) - 1) {
      let childInstance =
        i >= Array.length(currentChildInstances) ?
          None : Some(currentChildInstances[i]);
      let childComponent = newChildren[i];
      let newChildInstance =
        reconcile(root, childInstance, childComponent, context, container);
      newChildInstances :=
        List.append(newChildInstances^, [newChildInstance]);
    };

    /* Clean up existing children */
    for (i in
         Array.length(newChildren) to
         Array.length(currentChildInstances) - 1) {
      switch (currentChildInstances[i].node) {
      | Some(n) => ReconcilerImpl.removeChild(root, n)
      | _ => ()
      };
    };

    newChildInstances^;
  };

  let useReducer =
      (reducer: ('state, 'action) => 'state, initialState: 'state) => {
    let globalState = __globalState^;
    let componentState =
      ComponentState.popOldState(globalState, initialState);

    let (getState, updateState) =
      ComponentState.pushNewState(globalState, componentState);

    let currentContext = ComponentState.getCurrentContext(globalState);

    let dispatch = (context: ref(option(instance)), action: 'action) => {
      let newVal = reducer(getState(), action);
      updateState(newVal);
      switch (context^) {
      | Some(i) =>
        let {rootNode, element, _} = i;
        Event.dispatch(i.container.onBeginReconcile, rootNode);
        let _ =
          reconcile(rootNode, Some(i), element, i.context, i.container);
        Event.dispatch(i.container.onEndReconcile, rootNode);
        ();
      | _ => print_endline("WARNING: Skipping reconcile!")
      };
    };

    (componentState, dispatch(currentContext));
  };

  let _useReducerExperimental =
      (
        reducer: ('state, 'action) => 'state,
        initialState: 'state,
        continuation,
      ) =>
    continuation(useReducer(reducer, initialState));

  /*
     There's an internal and a public version of `useReducer`. The internal version,
     which has no "hooks types propagation", allows to keep the types in `useState`
     (which reuses `useReducer`) simpler
   */
  let useReducerExperimental = (reducer, initialState, continuation) =>
    _useReducerExperimental(reducer, initialState, continuation)
    |> addReducer(~reducer);

  type useStateAction('a) =
    | SetState('a);
  let useStateReducer = (_state, action) =>
    switch (action) {
    | SetState(newState) => newState
    };

  let useState = initialState => {
    let (componentState, dispatch) =
      useReducer(useStateReducer, initialState);
    let setState = newState => dispatch(SetState(newState));
    (componentState, setState);
  };

  let useStateExperimental = (initialState, continuation) =>
    _useReducerExperimental(
      useStateReducer,
      initialState,
      ((componentState, dispatch)) => {
        let setState = newState => dispatch(SetState(newState));
        continuation((componentState, setState))
        |> addState(~state=initialState);
      },
    );

  let updateContainer = (container, hook) => {
    let {containerNode, rootInstance, onBeginReconcile, onEndReconcile} = container;
    let prevInstance = rootInstance^;
    Event.dispatch(onBeginReconcile, containerNode);
    let nextInstance =
      reconcile(
        containerNode,
        prevInstance,
        elementFromHook(hook),
        noContext,
        container,
      );
    rootInstance := Some(nextInstance);
    Event.dispatch(onEndReconcile, containerNode);
  };
};

module State = State;
module Event = Event;
module Utility = Utility;
module Object = Object;