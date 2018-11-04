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

  let updateInstance: (node, primitives) => unit;
};

module type React = {
  type primitives;
  type node;

  type element =
    | Primitive(primitives)
    | Component
  and renderedElement =
    | RenderedPrimitive(node)
  and elementWithChildren = (element, childComponents, list(effect))
  /*
     A component is our JSX primitive element - just an object
     with a render method.
     TODO: Can we clean this interface up and just make component
     a function of type unit => elementWithChildren ?
   */
  and component = {render: unit => elementWithChildren}
  and childComponents = list(component)
  and effectInstance = unit => unit
  and effectInstances = list(effectInstance)
  /* An effect is a function sent to useEffect. We haven't run it yet, */
  /* But we will once the element is mounted */
  and effect = unit => effectInstance;

  type componentFunction = unit => component;

  type t;

  /*
       Container API
   */
  let createContainer: node => t;
  let updateContainer: (t, component) => unit;

  /*
       Component creation API
   */
  let primitiveComponent:
    (~children: childComponents, primitives) => component;
  let component:
    (~children: childComponents=?, componentFunction) => component;

  /*
       Component API
   */
  let empty: component;

  let useEffect: effect => unit;

  type stateUpdateFunction('t) = 't => unit;
  type stateResult('t) = ('t, stateUpdateFunction('t));
  let useState: 't => stateResult('t);
};
