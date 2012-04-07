package Ycombinator;

public class Ycombinator<Input, Output> {

	public interface InterfaceType<InterfaceInput, InterfaceOutput> {
		InterfaceOutput compute(InterfaceInput input);
	}

	public interface HighOrderCombinatorFor<Interface> {
		Interface combine(Interface calculator);
	}

	private interface RecursiveInvocationFor<Interface> {
		Interface invokeWithSelfRecursion(RecursiveInvocationFor<Interface> self);
	}

	private interface FromHighOrderCombinatorTo<Interface> {
		Interface map(HighOrderCombinatorFor<Interface> highOrder);
	}

	public InterfaceType<Input, Output> recursion(
			HighOrderCombinatorFor<InterfaceType<Input, Output>> highOrderObject) {

		return (new FromHighOrderCombinatorTo<InterfaceType<Input, Output>>() {

			@Override
			public InterfaceType<Input, Output> map(
					final HighOrderCombinatorFor<InterfaceType<Input, Output>> highOrder) {

				return (new RecursiveInvocationFor<InterfaceType<Input, Output>>() {

					@Override
					public InterfaceType<Input, Output> invokeWithSelfRecursion(
							RecursiveInvocationFor<InterfaceType<Input, Output>> self) {

						return self.invokeWithSelfRecursion(self);

					}
				}).invokeWithSelfRecursion(new RecursiveInvocationFor<InterfaceType<Input, Output>>() {

					@Override
					public InterfaceType<Input, Output> invokeWithSelfRecursion(
							final RecursiveInvocationFor<InterfaceType<Input, Output>> self) {

						return highOrder
								.combine(new InterfaceType<Input, Output>() {

									@Override
									public Output compute(Input input) {
										return self.invokeWithSelfRecursion(self)
												.compute(input);
									}
								});
					}
				});
			}
		}).map(highOrderObject);
	}
}
