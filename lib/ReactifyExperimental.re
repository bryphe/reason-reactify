open Reactify_TypesExperimental;

exception InvalidOpaqueInstance(string);

module Make = (ReconcilerImpl: Reconciler) => {
  /* Module to give us unique IDs for components */
  type elementWithChildren = (list(element), Effects.effects, Context.t)
  and render = unit => elementWithChildren
  and component('slot, 'nextSlots) = {
    initSlots: unit => Slots.t('slot, 'nextSlots),
    render:
      (
        (
          Slots.t('slot, 'nextSlots),
          option(instance),
          node,
          element,
          Context.t,
          container,
        )
      ) =>
      elementWithChildren,
  }
  and opaqueComponent =
    | OpaqueComponent(component('slot, 'nextSlots)): opaqueComponent
  and componentInstance('slot, 'nextSlots) = {
    slots: Slots.t('slot, 'nextSlots),
    component: component('slot, 'nextSlots),
  }
  and opaqueInstance =
    | OpaqueInstance(componentInstance('slot, 'nextSlots)): opaqueInstance
  and element =
    | Primitive(ReconcilerImpl.primitives, render)
    | Component(ComponentId.t, opaqueComponent)
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
    opaqueInstance: option(opaqueInstance), /* Only has value for instances of `Component()` elements. */
    node: option(ReconcilerImpl.node),
    rootNode: ReconcilerImpl.node,
    mutable childInstances,
    mutable effectInstances: Effects.effectInstances,
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
  and node = ReconcilerImpl.node;

  type primitives = ReconcilerImpl.primitives;

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

  let empty = Empty(_slots => ([], [], __globalContext^));

  let render = (id: ComponentId.t, userRender, ~children) => {
    ignore(children);
    let ret =
      Component(
        id,
        OpaqueComponent({
          initSlots: () => Slots.create(),
          render: slots => {
            Effects.resetEffects(__globalEffects);
            let childElement = userRender(slots);
            let children = [childElement];
            let effects = Effects.getEffects(__globalEffects);
            let renderResult: elementWithChildren = (
              children,
              effects,
              __globalContext^,
            );
            renderResult;
          },
        }),
      );
    ret;
  };

  module type Component = {
    type slots;
    type createElement;
    let createElement: createElement;
  };

  let createComponent =
      (
        type c,
        type s,
        create:
          (
            (
              (
                (
                  Slots.t(s, _),
                  option(instance),
                  node,
                  element,
                  Context.t,
                  container,
                )
              ) =>
              element,
              ~children: list(element)
            ) =>
            element
          ) =>
          c,
      )
      : (module Component with type createElement = c and type slots = s) => {
    let id = ComponentId.newId(_uniqueIdScope);
    let boundFunc = create(render(id));
    (module
     {
       type slots = s;
       type createElement = c;
       let createElement = boundFunc;
     });
  };

  let primitiveComponent = (~children, prim) =>
    Primitive(prim, _slots => (children, [], __globalContext^));

  /* Context */
  let __contextId = ref(0);
  type providerConstructor('t) =
    (~children: list(element), ~value: 't, unit) => element;
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
    let provider = (~children, ~value, ()) =>
      Provider(
        _slots => {
          let contextId = ctx.id;
          let context = Context.clone(__globalContext^);
          Context.set(context, contextId, Object.to_object(value));
          (children, [], context);
        },
      );
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

  let useEffect =
      (
        ~condition: Effects.effectCondition=Effects.Always,
        e: Effects.effectFunction,
      ) =>
    Effects.addEffect(~condition, __globalEffects, e);

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

  let isInstanceOfElement = (instance: option(instance), element: element) =>
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
    /* Recycle any previous effect instances */
    let previousEffectInstances = _getEffectsFromInstance(previousInstance);

    let isSameInstanceAsBefore =
      isInstanceOfElement(previousInstance, element);

    __globalContext := context;
    let ((children, effects, newContext), opaqueInstance) =
      switch (element) {
      | Primitive(_, render)
      | Provider(render)
      | Empty(render) => (render(), None)
      | Component({id, _}, OpaqueComponent(component)) =>
        switch (isSameInstanceAsBefore, previousInstance) {
        | (false, _) =>
          let innerInstance = {slots: component.initSlots(), component};
          let instance = OpaqueInstance(innerInstance);
          (
            innerInstance.component.render((
              innerInstance.slots,
              previousInstance,
              rootNode,
              element,
              context,
              container,
            )),
            Some(instance),
          );
        | (
            true,
            Some({
              opaqueInstance:
                Some(OpaqueInstance(innerInstance)) as prevOpaqueInstance,
              _,
            }),
          ) => (
            innerInstance.component.render((
              innerInstance.slots,
              previousInstance,
              rootNode,
              element,
              context,
              container,
            )),
            prevOpaqueInstance,
          )
        | (true, _) =>
          raise(
            InvalidOpaqueInstance(
              "`isSameInstanceAsBefore` is true but could not find opaque instance when rendering component with id "
              ++ string_of_int(id),
            ),
          )
        }
      };
    /* Once rendering is complete, we don't need this anymore */
    __globalContext := noContext;

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
      opaqueInstance,
      context: newContext,
      container,
    };

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

  let useReducer:
    (
      ('state, 'action) => 'state,
      'state,
      (
        Slots.t('state, Slots.t('slot, 'nextSlots)),
        option(instance),
        node,
        element,
        Context.t,
        container,
      )
    ) =>
    (('state, 'action => unit), Slots.t('slot, 'nextSlots)) =
    (
      reducer,
      initialState,
      (slots, previousInstance, rootNode, element, context, container),
    ) => {
      let ((state, setState), nextSlots) =
        Slots.use(~default=initialState, slots);

      let dispatch = (action: 'action) => {
        let newVal = reducer(state, action);
        setState(newVal);
        Event.dispatch(container.onBeginReconcile, rootNode);
        let _ =
          reconcile(rootNode, previousInstance, element, context, container);
        Event.dispatch(container.onEndReconcile, rootNode);
        ();
      };

      ((state, dispatch), nextSlots);
    };

  type useStateAction('a) =
    | SetState('a);
  let useStateReducer = (_state, action) =>
    switch (action) {
    | SetState(newState) => newState
    };

  let useState = (initialState, slots) => {
    let ((componentState, dispatch), nextSlots) =
      useReducer(useStateReducer, initialState, slots);
    let setState = newState => dispatch(SetState(newState));
    ((componentState, setState), nextSlots);
  };

  let updateContainer = (container, element) => {
    let {containerNode, rootInstance, onBeginReconcile, onEndReconcile} = container;
    let prevInstance = rootInstance^;
    Event.dispatch(onBeginReconcile, containerNode);
    let nextInstance =
      reconcile(containerNode, prevInstance, element, noContext, container);
    rootInstance := Some(nextInstance);
    Event.dispatch(onEndReconcile, containerNode);
  };
};

module State = State;
module Event = Event;
module Utility = Utility;
module Object = Object;