/*
   Effects.re

   Module encapsulating some simple effect manipulation
 */

type effectCondition =
  | Always
  | MountUnmount
and effectInstanceFunction = unit => unit
and effectFunction = unit => effectInstanceFunction
and effectInstance = {
  fn: effectInstanceFunction,
  condition: effectCondition,
}
and effectInstances = list(effectInstance)
/* An effect is a function sent to useEffect. We haven't run it yet, */
/* But we will once the element is mounted */
and effect = {
  effectFn: unit => effectInstanceFunction,
  effectCondition,
}
and effects = list(effect);

let noop = () => ();

/*
   Core type for the effects module
 */
type t = ref(effects);

let create: unit => t = () => ref([]);

let resetEffects: t => unit = (effects: t) => effects := [];

let addEffect =
    (
      ~condition: effectCondition,
      effects: ref(effects),
      effectFunction: effectFunction,
    ) => {
  let effect: effect = {effectFn: effectFunction, effectCondition: condition};

  effects := List.append(effects^, [effect]);
};

let getEffects: t => list(effect) = effects => effects^;

let rec createEmptyEffectInstances = (x: int) =>
  x > 0 ?
    [{fn: noop, condition: Always}, ...createEmptyEffectInstances(x - 1)] :
    [];

let runEffects:
  (~previousInstances: effectInstances=?, effects) => effectInstances =
  (~previousInstances: option(effectInstances)=?, effects) => {
    let previousInstances =
      switch (previousInstances) {
      | None => createEmptyEffectInstances(List.length(effects))
      | Some(x) => x
      };

    let fn =
        (
          acc: effectInstances,
          previousEffectInstance: effectInstance,
          currentEffect: effect,
        ) => {
      let pc = previousEffectInstance.condition;
      let nc = currentEffect.effectCondition;
      let newInstance =
        pc === nc && pc === MountUnmount ?
          previousEffectInstance :
          {
            previousEffectInstance.fn();
            let effectInstanceFn = currentEffect.effectFn();
            let ret: effectInstance = {
              condition: currentEffect.effectCondition,
              fn: effectInstanceFn,
            };
            ret;
          };

      [newInstance, ...acc];
    };

    let initial: effectInstances = [];

    let l = List.fold_left2(fn, initial, previousInstances, effects);
    List.rev(l);
  };

let drainEffects: effectInstances => unit =
  (effects: effectInstances) => {
    let fn = ei => ei.fn();
    List.iter(fn, effects);
  };

/* let runEffectInstances: (effectInstances) => unit = (effectInstances) => { */
/*     List.iter(ei => ei(), effectInstances); */
/* }; */
