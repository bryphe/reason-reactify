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

  type element =
    | Primitive(primitives)
    | Component(string)
    | Provider
    | Empty
  and renderedElement =
    | RenderedPrimitive(node)
  and elementWithChildren = (element, childComponents, Effects.effects, Context.t)
  /*
     A component is our JSX primitive element - just an object
     with a render method.
     TODO: Can we clean this interface up and just make component
     a function of type unit => elementWithChildren ?
   */
  and component = {
      element,
      render: unit => elementWithChildren
  }
  and componentFunction = unit => component
  and childComponents = list(component);

  type t;

  /*
       Container API
   */
  type reconcileNotification = node => unit;
  let createContainer: (~onBeginReconcile:reconcileNotification=?, ~onEndReconcile:reconcileNotification=?, node) => t;
  let updateContainer: (t, component) => unit;

  /*
       Component creation API
   */
  let primitiveComponent:
    (~children: childComponents, primitives) => component;
  let component:
    (~children: childComponents=?, ~uniqueId:string, componentFunction) => component;

  module type COMPONENT = {
    type t;
    let createElement: t;
  };

  type renderFunction = (~children:childComponents=?, componentFunction) => component;
  type func('a) = renderFunction => 'a;

  let component2: (func('a)) => (module COMPONENT with type t = 'a);

  /*
       Component API
   */

  type providerConstructor('t) = (~children: childComponents, ~value: 't, unit) => component;
  type context('t);

  let getProvider: context('t) => providerConstructor('t);
  let createContext: 't => context('t);
  let useContext: context('t) => 't;

  let empty: component;

  let useEffect: (~condition:Effects.effectCondition=?, Effects.effectFunction) => unit;

  type stateUpdateFunction('t) = 't => unit;
  type stateResult('t) = ('t, stateUpdateFunction('t));
  let useState: 't => stateResult('t);
};
