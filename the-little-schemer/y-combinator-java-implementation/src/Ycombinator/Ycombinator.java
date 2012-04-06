package Ycombinator;

public class Ycombinator<Input, Output> {

	public interface InterfaceType<InterfaceInput, InterfaceOutput> {
		InterfaceOutput compute(InterfaceInput input);
	}

	private static interface RecursiveInvocationFor<Interface> {
		Interface invokeWithRecursion(RecursiveInvocationFor<Interface> self);
	}

	public static interface HighOrderCombinatorFor<Interface> {

		Interface combine(Interface calculator);
	}

	private static interface FromHighOrderCombinatorTo<Interface> {
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
					public InterfaceType<Input, Output> invokeWithRecursion(
							RecursiveInvocationFor<InterfaceType<Input, Output>> self) {

						return self.invokeWithRecursion(self);

					}
				}).invokeWithRecursion(new RecursiveInvocationFor<InterfaceType<Input, Output>>() {

					@Override
					public InterfaceType<Input, Output> invokeWithRecursion(
							final RecursiveInvocationFor<InterfaceType<Input, Output>> self) {

						return highOrder
								.combine(new InterfaceType<Input, Output>() {

									@Override
									public Output compute(Input input) {
										return self.invokeWithRecursion(self)
												.compute(input);
									}
								});
					}
				});
			}
		}).map(highOrderObject);
	}
}
