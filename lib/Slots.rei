type t('slot, 'nextSlots);
let create: unit => t('slot, 'nextSlots);
let use:
  (~default: 'slot, t('slot, t('slot2, 'nextSlots))) =>
  (('slot, 'slot => unit), t('slot2, 'nextSlots));