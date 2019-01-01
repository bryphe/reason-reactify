/*
 * Interface to bridge a React-style functional API
 * with a mutable back-end. Similiar in spirit
 * to the reconciler interface provided via 'react-conciler'.
 */

module type Reconciler = {
  /*
      Primitives is a variant type describing metadata needed
      to instantiate something in the mutable world.
   */
  type primitives;

  /*
      A node is a live instance representing a mutable object
   */
  type node;

  let appendChild: (node, node) => unit;

  let createInstance: primitives => node;

  let replaceChild: (node, node, node) => unit;

  let removeChild: (node, node) => unit;

  let updateInstance: (node, primitives, primitives) => unit;
};

module type React = {
  type primitives;
  type node;
  type state('s);
  type reducer('r);
  type effect;
  type context('t);

  type renderedElement =
    | RenderedPrimitive(node)
  and elementWithChildren = (list(element), Effects.effects, Context.t)
  and render = unit => elementWithChildren
  and element =
    | Primitive(primitives, render)
    | Component(ComponentId.t, render)
    | Provider(render)
    | Empty(render)
  and hook('t) =
    | Hook(element, 't)
  and emptyHook = hook(unit);

  type t;

  /*
       Container API
   */
  type reconcileNotification = node => unit;
  let createContainer:
    (
      ~onBeginReconcile: reconcileNotification=?,
      ~onEndReconcile: reconcileNotification=?,
      node
    ) =>
    t;
  let updateContainer: (t, emptyHook) => unit;

  /*
       Component creation API
   */
  let primitiveComponent:
    (~children: list(emptyHook), primitives) => emptyHook;

  module type Component = {
    type hooks;
    type createElement;
    let createElement: createElement;
  };

  let createComponent:
    (
      (
        (unit => hook('h), ~children: list(emptyHook)) =>
        emptyHook
      ) =>
      'c
    ) =>
    (module Component with type createElement = 'c and type hooks = 'h);

  /*
       Component API
   */

  type providerConstructor('t) =
    (~children: list(emptyHook), ~value: 't, unit) =>
    emptyHook;
  type contextValue('t);

  let getProvider: contextValue('t) => providerConstructor('t);
  let createContext: 't => contextValue('t);
  let useContext:
    (contextValue('t), 't => hook('a)) =>
    (hook(('a, context('t))));

  let empty: emptyHook;

  let useEffect:
    (
      ~condition: Effects.effectCondition=?,
      Effects.effectFunction,
      unit => hook('a)
    ) =>
    hook(('a, effect));

  let useState:
    ('state, (('state, 'state => unit)) => hook('a)) =>
    hook(('a, state('state)));

  let useReducer:
    (
      ('state, 'action) => 'state,
      'state,
      (('state, 'action => unit)) => hook('a)
    ) =>
    hook(('a, reducer(('state, 'action) => 'state)));
};