package Ycombinator;

public class Ycombinator<Input, Output> {

	public interface InterfaceType<InterfaceInput, InterfaceOutput> {
		InterfaceOutput compute(InterfaceInput input);
	}

	public static interface ListLengthCalculatorRecursiveInvocation<Interface> {
		Interface invokeWithRecursion(
				ListLengthCalculatorRecursiveInvocation<Interface> self);
	}

	public static interface ListLengthCalculatorHighOrder<Interface> {

		Interface make(Interface calculator);
	}

	public static interface ListLengthCalculatorMaker<Interface> {
		Interface use(ListLengthCalculatorHighOrder<Interface> highOrder);
	}

	public Output recursion(
			ListLengthCalculatorHighOrder<InterfaceType<Input, Output>> highOrderObject,
			Input input) {

		return (new ListLengthCalculatorMaker<InterfaceType<Input, Output>>() {

			@Override
			public InterfaceType<Input, Output> use(
					final ListLengthCalculatorHighOrder<InterfaceType<Input, Output>> highOrder) {

				return (new ListLengthCalculatorRecursiveInvocation<InterfaceType<Input, Output>>() {

					@Override
					public InterfaceType<Input, Output> invokeWithRecursion(
							ListLengthCalculatorRecursiveInvocation<InterfaceType<Input, Output>> self) {

						return self.invokeWithRecursion(self);

					}
				}).invokeWithRecursion(new ListLengthCalculatorRecursiveInvocation<InterfaceType<Input, Output>>() {

					@Override
					public InterfaceType<Input, Output> invokeWithRecursion(
							final ListLengthCalculatorRecursiveInvocation<InterfaceType<Input, Output>> self) {

						return highOrder
								.make(new InterfaceType<Input, Output>() {

									@Override
									public Output compute(Input input) {
										return self.invokeWithRecursion(self)
												.compute(input);
									}
								});
					}
				});
			}
		}).use(highOrderObject).compute(input);
	}
}
