/* Support methods for the library */

/*
    areConstructorsEqual

    This is a helper method to check if two objects came from the same constructor.
*/

let areConstructorsEqual = (x: 'a, y: 'a) => {
  let r = Obj.repr(x)
  and s = Obj.repr(y);
  if (Obj.is_int(r) && Obj.is_int(s)) {
    (Obj.obj(r): int) == (Obj.obj(s): int);
  } else if (Obj.is_block(r) && Obj.is_block(s)) {
    Obj.tag(r) == Obj.tag(s);
  } else {
    false;
  };
};
