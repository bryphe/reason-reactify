/*
  Effects.re

  Module encapsulating some simple effect manipulation
*/

type effectCondition = option(Object.t)
and effectInstanceFunction = unit => unit
and effectFunction = unit => effectInstanceFunction
and effectInstance = {
    fn: effectInstanceFunction,
    condition: effectCondition
}
and effectInstances = list(effectInstance)
/* An effect is a function sent to useEffect. We haven't run it yet, */
/* But we will once the element is mounted */
and effect = {
    effectFn: unit => effectInstanceFunction,
    effectCondition: effectCondition,
}
and effects = list(effect);

let noop = () => ();

/*
  Core type for the effects module
*/
type t = ref(effects);

let create: unit => t = () => {
    ref([]);
};

let resetEffects: t => unit = (effects: t) => {
    effects := [];
};

let addEffect = (~condition:'a=?, effects: ref(effects), effectFunction: effectFunction) => {
    let boxedCondition = switch(condition) {
    | None => None
    | Some(x) => Some(Object.to_object(x))
    };

    let effect: effect = {
      effectFn: effectFunction,
      effectCondition: boxedCondition,
    };

    effects := List.append(effects^, [effect]);
};

let getEffects: (t) => list(effect) = (effects) => {
    effects^;
};

let rec createEmptyEffectInstances = (x: int) => {
    switch (x > 0) {
    | true => [{ fn: noop, condition: None}, ...createEmptyEffectInstances(x - 1)]
    | false => []
    };
};

let runEffects: (effectInstances, effects) => effectInstances = (previousInstances, effects) => {

    let fn = (acc: effectInstances, previousEffectInstance: effectInstance, currentEffect: effect) => {
        previousEffectInstance.fn(); 
        let effectInstanceFn = currentEffect.effectFn();
        let effectInstance: effectInstance = {
            condition: currentEffect.effectCondition,
            fn: effectInstanceFn,
        };
        [effectInstance, ...acc]
    };

    let initial: effectInstances = [];

    List.fold_left2(fn, initial, previousInstances, effects);
    /* List.rev(l); */
};

/* let runEffectInstances: (effectInstances) => unit = (effectInstances) => { */
/*     List.iter(ei => ei(), effectInstances); */
/* }; */
