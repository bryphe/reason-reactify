/*
    Effects.re

    Module encapsulating some simple effect manipulation
*/

type t = ref(list(effect));

type effectInstance = unit => unit
and effectInstances = list(effectInstance)
/* An effect is a function sent to useEffect. We haven't run it yet, */
/* But we will once the element is mounted */
and effect = unit => effectInstance;

let create: unit => t = () => {
    ref([]);
};

let resetEffects: t => unit = (effects: t) => {
    effects := [];
};

let addEffect: (t, effect) => unit = (effects, effect) {
    effects := List.append(effects^, [effect]);
};

let getEffects: (t) => list(effect) = (effects) => {
    effects^;
};

let runEffects: (list(effect)) => list(effectInstances) = (effects) => {
    List.map(e => e(), effects);
};

let runEffectInstances: (effectInstances) => unit = (effectInstances) => {
    List.iter(ei => ei(), effectInstances);
};
