/* Support methods for the library */

/*
     areConstructorsEqual

     This is a helper method to check if two objects came from the same constructor.
     The use of Obj.Magic is _not_ good and should be avoided, as it relies on internal
     representation.

     It looks like there are some smarter things we can do with the type system to handle this,
     https://github.com/reasonml/reason-react/blob/a46fcda65f0847246a493bb2743f216010679b86/ReactMini/src/React.re#L422
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
