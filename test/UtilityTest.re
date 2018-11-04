
open TestUtility;

type testRecord = {
    testVal: int
};

type testType = 
| A(int)
| B
| C
| D(testRecord);

let a11 = A(1);
let a12 = A(1);
let a2 = A(2);

let b = B;
let c = C;

let d11 = D({testVal: 1});
let d12 = D({testVal: 1});
let d2 = D({testVal: 2});

test("Same constructors should be equal", () => {
    assert(Utility.areConstructorsEqual(a11,a12) == true);
});

test("Constructors with different parameters should not be equal", () => {
    assert(Utility.areConstructorsEqual(a11,a2) == false);
});

test("Different constructors should not be equal", () => {
    assert(Utility.areConstructorsEqual(b, c) == false);
});

test("Constructors with same record types should be equal", () => {
    assert(Utility.areConstructorsEqual(d11, d12) == true);
});

test("Constructors with different record types should not be equal", () => {
    assert(Utility.areConstructorsEqual(d11, d2) == false);
});
