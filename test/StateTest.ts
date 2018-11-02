test("State conversion works with ints", () => {
    let i = 1;
    let stateI = Reactify.State.to_state(i);
    let rehydratedI = Reactify.State.of_state(stateI);
    assert(i == rehydratedI);
});

test("State conversion works with tuples", () => {
    let p = (1, "a");
    let stateP = Reactify.State.to_state(p);
    let rehydratedP = Reactify.State.of_state(stateP);
    assert(p == rehydratedP);
});

test("State can be used to create list of different types", () => {
    let stateList: list(Reactify.State.t) = [];

    let stateList = [Reactify.State.to_state(1), ...stateList];
    let stateList = [Reactify.State.to_state(("a", "b")), ...stateList];

    let firstElement = List.nth(stateList, 0);
    assert(Reactify.State.of_state(firstElement) == ("a", "b"));

    let lastElement = List.nth(stateList, 1);
    assert(Reactify.State.of_state(lastElement) == 1);
});
