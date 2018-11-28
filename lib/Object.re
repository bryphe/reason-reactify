/*

  This module is _not ideal_ - we should be leveraging the magic of the OCaml type system -
  not `Obj.magic` - to help us here. We use this to assist in creating a heterogenous list of state,
  since `useState` can be called with any type, and we're preserving the semantics
  of the React hooks model.

  There are other potential options - but they would require API changes.

  Definitely open to alternatives!
 */

type t;

let to_object: 'a => t = (v: 'a) => Obj.magic(v);
let of_object: t => 'a = (v: t) => Obj.magic(v);
