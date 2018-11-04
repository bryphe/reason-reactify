open Reactify_Types;

module Make:
  (ReconcilerImpl: Reconciler) =>

    React with
      type node = ReconcilerImpl.node and
      type primitives = ReconcilerImpl.primitives;

module State = State;
module Event = Event;
module Utility = Utility;
