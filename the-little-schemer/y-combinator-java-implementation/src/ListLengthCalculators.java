/**
 * @author massimo
 * 
 */
public class ListLengthCalculators {

	/**
	 * This is a decider, always halts.
	 */
	public static ListLengthCalculator NormalRecursion = new ListLengthCalculator() {

		@Override
		public int length(ListModule list) {
			return list.size() == 0 ? 0 : 1 + this.length(list.cdr());
		}
	};

	/**
	 * This is the most partial function that we can write: it doesn't halt on
	 * any input list.
	 */
	public static ListLengthCalculator Eternity = new ListLengthCalculator() {

		@Override
		public int length(ListModule list) {
			return this.length(list);
		}
	};

	public static ListLengthCalculator DecideOnlyEmptyLists = new ListLengthCalculator() {

		@Override
		public int length(ListModule list) {
			return list.size() == 0 ? 0 : 1 + Eternity.length(list.cdr());
		}
	};

	public static ListLengthCalculator DecideOnlyListWithAtMostOneElement = new ListLengthCalculator() {

		@Override
		public int length(ListModule list) {
			return list.size() == 0 ? 0 : 1 + (new ListLengthCalculator() {

				@Override
				public int length(ListModule list) {
					return list.size() == 0 ? 0 : 1 + Eternity.length(list
							.cdr());
				}
			}).length(list.cdr());

		}
	};

	public static ListLengthCalculator DecideOnlyListWithAtMostTwoElements = new ListLengthCalculator() {

		@Override
		public int length(ListModule list) {
			return list.size() == 0 ? 0 : 1 + (new ListLengthCalculator() {

				@Override
				public int length(ListModule list) {
					return list.size() == 0 ? 0
							: 1 + (new ListLengthCalculator() {

								@Override
								public int length(ListModule list) {
									return list.size() == 0 ? 0 : 1 + Eternity
											.length(list.cdr());
								}
							}).length(list.cdr());
				}
			}).length(list.cdr());

		}
	};

	public static ListLengthCalculator DecideOnlyEmptyListsUsingHighOrder = new ListLengthCalculator() {

		@Override
		public int length(ListModule list) {

			return (new ListLengthCalculatorHighOrder() {

				@Override
				public ListLengthCalculator make(
						final ListLengthCalculator calculator) {

					return new ListLengthCalculator() {

						@Override
						public int length(ListModule list) {
							return list.size() == 0 ? 0 : 1 + calculator
									.length(list.cdr());
						}
					};
				}
			}).make(Eternity).length(list);

		}
	};

	public static ListLengthCalculator DecideOnlyListWithAtMostOneElementUsingHighOrder = new ListLengthCalculator() {

		@Override
		public int length(ListModule list) {

			return (new ListLengthCalculatorHighOrder() {

				@Override
				public ListLengthCalculator make(
						final ListLengthCalculator calculator) {

					return new ListLengthCalculator() {

						@Override
						public int length(ListModule list) {
							return list.size() == 0 ? 0 : 1 + calculator
									.length(list.cdr());
						}
					};
				}
			}).make((new ListLengthCalculatorHighOrder() {

				@Override
				public ListLengthCalculator make(
						final ListLengthCalculator calculator) {

					return new ListLengthCalculator() {

						@Override
						public int length(ListModule list) {
							return list.size() == 0 ? 0 : 1 + calculator
									.length(list.cdr());
						}
					};
				}
			}).make(Eternity)).length(list);

		}
	};

	public static ListLengthCalculator DecideOnlyListWithAtMostTwoElementUsingHighOrder = new ListLengthCalculator() {

		@Override
		public int length(ListModule list) {

			return (new ListLengthCalculatorHighOrder() {

				@Override
				public ListLengthCalculator make(
						final ListLengthCalculator calculator) {

					return new ListLengthCalculator() {

						@Override
						public int length(ListModule list) {
							return list.size() == 0 ? 0 : 1 + calculator
									.length(list.cdr());
						}
					};
				}
			}).make((new ListLengthCalculatorHighOrder() {

				@Override
				public ListLengthCalculator make(
						final ListLengthCalculator calculator) {

					return new ListLengthCalculator() {

						@Override
						public int length(ListModule list) {
							return list.size() == 0 ? 0 : 1 + calculator
									.length(list.cdr());
						}
					};
				}
			}).make((new ListLengthCalculatorHighOrder() {

				@Override
				public ListLengthCalculator make(
						final ListLengthCalculator calculator) {

					return new ListLengthCalculator() {

						@Override
						public int length(ListModule list) {
							return list.size() == 0 ? 0 : 1 + calculator
									.length(list.cdr());
						}
					};
				}
			}).make(Eternity))).length(list);

		}
	};

}
