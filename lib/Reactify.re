open Reactify_Types;

module Make = (ReconcilerImpl: Reconciler) => {
  type element =
    | Primitive(ReconcilerImpl.primitives)
    | Component
  and renderedElement =
    | RenderedPrimitive(ReconcilerImpl.node)
  and elementWithChildren = (element, childComponents, list(effect))
  /*
     A component is our JSX primitive element - just an object
     with a render method.
     TODO: Can we clean this interface up and just make component
     a function of type unit => elementWithChildren ?
   */
  and component = {render: unit => elementWithChildren}
  and childComponents = list(component)
  /*
      An instance is a component that has been rendered.
      We store some additional context for it, like the state,
      effects that need to be run, and corresponding nodes.
   */
  and instance = {
    component,
    element,
    children: childComponents,
    node: option(ReconcilerImpl.node),
    rootNode: ReconcilerImpl.node,
    mutable childInstances,
    mutable effectInstances: Effects.effectInstances,
    state: State.HeterogenousMutableList.t,
  }
  and childInstances = list(instance)
  /*
     An effectInstance is an effect that was already instantiated -
     it's an effect we'll have to run when the element is unmounted
   */
  and effectInstance = unit => unit
  and effectInstances = list(effectInstance)
  /* An effect is a function sent to useEffect. We haven't run it yet, */
  /* But we will once the element is mounted */
  and effect = unit => effectInstance;

  type stateUpdateFunction('t) = 't => unit;
  type stateResult('t) = ('t, stateUpdateFunction('t));

  type node = ReconcilerImpl.node;
  type primitives = ReconcilerImpl.primitives;

  /*
     A global, non-pure container to hold effects
     during the course of a render operation.
   */
  let __globalEffects = Effects.create();

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
  type container = {
    rootInstance: ref(option(instance)),
    rootNode: ReconcilerImpl.node,
  };
  type t = container;

  let createContainer = (rootNode: ReconcilerImpl.node) => {
    let ret: container = {rootNode, rootInstance: ref(None)};
    ret;
  };

  type componentFunction = unit => component;

  let empty: component = {
    render: () => {
        (Component, [], [])
    },
  };

  let component = (~children: childComponents=[], c: componentFunction) => {
    let ret: component = {
      render: () => {
        Effects.resetEffects(__globalEffects);
        let children: list(component) = [c()];
        let effects = Effects.getEffects(__globalEffects);
        let renderResult: elementWithChildren = (
          Component,
          children,
          effects,
        );
        renderResult;
      },
    };
    ret;
  };

  let primitiveComponent = (~children, prim) => {
    let comp: component = {render: () => (Primitive(prim), children, [])};
    comp;
  };

  let useEffect = (e: effect) => Effects.addEffect(__globalEffects, e);

  exception TodoException;

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

  /*
   * Instantiate turns a component function into a live instance,
   * and asks the reconciler to append it to the root node.
   */
  let rec instantiate =
          (
            rootNode,
            previousInstance: option(instance),
            component: component,
          ) => {
    /* Recycle any previous effect instances */
    let previousEffectInstances = _getEffectsFromInstance(previousInstance);
    Effects.runEffectInstances(previousEffectInstances);

    /* Set up state for the component */
    let previousState = _getCurrentStateFromInstance(previousInstance);
    let stateInstance = ref(previousInstance);
    let state = ComponentState.create(previousState);
    /* We hold onto a reference to the component instance - we need to set this _after_ the component is instantiated */
    let context = ComponentState.getCurrentContext(state);

    /*
         This is dirty, but we set the 'global' state so that the 'useState'
         can have access to it, without additional binding or passing. This is
         necessary to preserve the semantics of the React-style API
     */
    __globalState := state;
    let (element, children, effects) = component.render();
    /* Once rendering is complete, we don't need this anymore */
    __globalState := noState;
    let newState = ComponentState.getNewState(state);

    /* TODO: Should this be deferred until we actually mount the component? */
    let effectInstances = Effects.runEffects(effects);

    let primitiveInstance =
      switch (element) {
      | Primitive(p) => Some(ReconcilerImpl.createInstance(p))
      | _ => None
      };

    let nextRootPrimitiveInstance =
      switch (primitiveInstance) {
      | Some(i) => i
      | None => rootNode
      };

    let previousChildInstances = _getPreviousChildInstances(previousInstance);
    let childInstances = reconcileChildren(nextRootPrimitiveInstance, previousChildInstances, children);

    let instance: instance = {
      component,
      element,
      node: primitiveInstance,
      rootNode: nextRootPrimitiveInstance,
      children,
      childInstances: childInstances,
      effectInstances,
      state: newState,
    };

    /*
         'context' is the instance that state needs when 'setState' is called
         We set it here, after the instance is fully realized, so that the 'setState'
         callback has the latest state for the component instance.
     */
    context := Some(instance);

    instance;
  }
  and reconcile = (rootNode, instance, component) => {
    let newInstance = instantiate(rootNode, instance, component);

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
            | (Primitive(oldPrim), Primitive(newPrim)) =>
              if (oldPrim != newPrim) {
                /* Check if the primitive type is the same - if it is, we can simply update the node */
                /* If not, we'll replace the node */
                if (Utility.areConstructorsEqual(oldPrim, newPrim)) {
                  switch (newInstance.element) {
                  | Primitive(o) =>
                    ReconcilerImpl.updateInstance(b, o);
                    i.childInstances = reconcileChildren(b, i.childInstances, newInstance.children);
                    i;
                  | _ =>
                    print_endline(
                      "ERROR: We shouldn't hit this condition! If there is a node, there should be a related primitive element.",
                    );
                    newInstance;
                  };
                } else {
                  ReconcilerImpl.replaceChild(rootNode, a, b);
                  newInstance;
                };
              } else {
                /* The node itself is unchanged, so we'll just reconcile the children */
                i.childInstances = reconcileChildren(b, i.childInstances, newInstance.children);
                i;
              };
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
          | (None, None) =>
            newInstance;
          };

        ret;
      };
    r;
  }
  and reconcileChildren = (root: node, currentChildInstances: childInstances, newChildren: list(component)) => {
    let currentChildInstances: array(instance) = Array.of_list(currentChildInstances);
    let newChildren = Array.of_list(newChildren);

    let newChildInstances: ref(childInstances) = ref([]);

    for (i in 0 to Array.length(newChildren) - 1) {
      let childInstance =
        i >= Array.length(currentChildInstances) ?
          None : Some(currentChildInstances[i]);
      let childComponent = newChildren[i];
      let newChildInstance = reconcile(root, childInstance, childComponent);
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

  let useState = (v: 't) => {
    let state = __globalState^;
    let n = ComponentState.popOldState(state, v);

    let updateFunction = ComponentState.pushNewState(state, n);

    /* let updateFunction = (_n) => { (); }; */

    let currentContext = ComponentState.getCurrentContext(state);

    let setState = (context: ref(option(instance)), newVal: 't) => {
      updateFunction(newVal);
      switch (context^) {
      | Some(i) =>
        let {rootNode, component} = i;
        reconcile(rootNode, Some(i), component);
        ();
      | _ => print_endline("WARNING: Skipping reconcile!")
      };
    };

    (n, setState(currentContext));
  };

  let updateContainer = (container, component) => {
    let {rootNode, rootInstance} = container;
    let prevInstance = rootInstance^;
    let nextInstance = reconcile(rootNode, prevInstance, component);
    rootInstance := Some(nextInstance);
  };
};

module State = State;
module Event = Event;
module Utility = Utility;
