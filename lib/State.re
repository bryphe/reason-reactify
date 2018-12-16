/*
  State - helper to convert any type of object to an opaque 'State.t'

  This is _not ideal_ - we should be leveraging the magic of the OCaml type system,
  to help us here. We use this to assist in creating a heterogenous list of state,
  since `useState` can be called with any type, and we're preserving the semantics
  of the React hooks model.

  There are other potential options - but they would require API changes.

  Definitely open to alternatives!
 */

module type StateContext = {type t;};

module HeterogenousMutableList = {
  type t = list(ref(Object.t));

  let create = () => [];
};

module Make = (StateContextImpl: StateContext) => {
  type t = {
    context: ref(ref(option(StateContextImpl.t))),
    mutable currentState: HeterogenousMutableList.t,
    mutable newState: HeterogenousMutableList.t,
  };

  type getterAndUpdater('a) = (unit => 'a, 'a => unit);

  let noneContext = () => ref(None);

  let create = (previousState: HeterogenousMutableList.t) => {
    let ret: t = {
      context: ref(noneContext()),
      currentState: previousState,
      newState: HeterogenousMutableList.create(),
    };
    ret;
  };

  let popOldState: (t, 'a) => 'a =
    (state: t, defaultValue: 'a) => {
      let curr =
        switch (state.currentState) {
        | [] => defaultValue
        | [hd, ...tail] =>
          state.currentState = tail;
          Object.of_object(hd^);
        };
      curr;
    };

  let pushNewState: (t, 'a) => getterAndUpdater('a) =
    (state: t, currentVal: 'a) => {
      let updatedVal: ref(Object.t) = ref(Object.to_object(currentVal));
      state.newState = List.append(state.newState, [updatedVal]);
      let ret: getterAndUpdater('a) = (
        () => Object.of_object(updatedVal^),
        (newVal: 'a) => {
          updatedVal := Object.to_object(newVal);
          ();
        },
      );
      ret;
    };

  let getCurrentContext = (state: t) => state.context^;

  let getNewState: t => HeterogenousMutableList.t =
    (state: t) => state.newState;
};