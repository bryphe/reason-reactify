type t('slot, 'nextSlots) = ref(option(('slot, 'nextSlots)));
let create = () => ref(None);
let use = (~default, slots: t(_)) =>
  switch (slots^) {
  | None =>
    let slot = default;
    let nextSlots = create();
    slots := Some((slot, nextSlots));
    let set = slot => slots := Some((slot, nextSlots));
    ((slot, set), nextSlots);
  | Some((slot, nextSlots)) =>
    let set = slot => slots := Some((slot, nextSlots));
    ((slot, set), nextSlots);
  };