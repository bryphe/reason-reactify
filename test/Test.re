/* TODO: Is there a better way for tests to be picked up automatically? */
/* We could potentially create a lib/bin pair, and use `library_flags -linkall` */

module ContainerTest = ContainerTest;
module StateTest = StateTest;
module PrimitiveComponentTest = PrimitiveComponentTest;
module StatelessComponentTest = StatelessComponentTest;
module HooksUseEffectTest = HooksUseEffectTest;
module HooksUseReducerTest = HooksUseReducerTest;
module HooksUseStateTest = HooksUseStateTest;
module HooksUseContextTest = HooksUseContextTest;
module UtilityTest = UtilityTest;

Rejest.run();
