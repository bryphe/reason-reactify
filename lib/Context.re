/*
  Context
 */

module HeterogenousHashtbl = {
  type t = Hashtbl.t(int, Object.t);

  let create = () => Hashtbl.create(16);
};

type t = HeterogenousHashtbl.t;

let create = () => HeterogenousHashtbl.create();

let clone = (original: t) => Hashtbl.copy(original);

let set = (context: t, key: int, obj: Object.t) =>
  Hashtbl.replace(context, key, obj);

let get = (context: t, key: int) => Hashtbl.find_opt(context, key);
