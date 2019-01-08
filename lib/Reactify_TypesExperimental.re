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

  type elementWithChildren = (list(element), Effects.effects, Context.t)
  and render = unit => elementWithChildren
  and opaqueComponent
  and element =
    | Primitive(primitives, render)
    | Component(ComponentId.t, opaqueComponent)
    | Provider(render)
    | Empty(render)
  and hook('t) =
    | Hook(element, 't)
  and instance /* TODO: Remove */
  and container; /* TODO: Remove */

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
  let updateContainer: (t, element) => unit;

  /*
       Component creation API
   */
  let primitiveComponent:
    (~children: list(element), primitives) => element;

  module type Component = {
    type slots;
    type createElement;
    let createElement: createElement;
  };

  let createComponent:
    (
      (
        (
          (
            (
              Slots.t('s, _),
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
      'c
    ) =>
    (module Component with type createElement = 'c and type slots = 's);

  /*
       Component API
   */

  type providerConstructor('t) =
    (~children: list(element), ~value: 't, unit) => element;
  type contextValue('t);

  let getProvider: contextValue('t) => providerConstructor('t);
  let createContext: 't => contextValue('t);
  let useContext: contextValue('t) => 't;

  let empty: element;

  let useEffect:
    (~condition: Effects.effectCondition=?, Effects.effectFunction) => unit;

  let useState:
    (
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
    (('state, 'state => unit), Slots.t('slot, 'nextSlots));

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
    (('state, 'action => unit), Slots.t('slot, 'nextSlots));
};