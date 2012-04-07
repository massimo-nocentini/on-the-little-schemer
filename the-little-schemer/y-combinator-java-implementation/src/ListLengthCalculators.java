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

			// in this definition we recognize a common structure: we ask
			// something about the size of the list and then delegate to a
			// function (constructed dynamically) to do so work. This function
			// asks the same question about the size of the list and then
			// delegate to a new function...
			return list.size() == 0 ? 0 : 1 + (new ListLengthCalculator() {
				@Override
				public int length(ListModule list) { // label:*

					return list.size() == 0 ? 0
							: 1 + (new ListLengthCalculator() {

								@Override
								public int length(ListModule list) { // label:**

									// observe that here we fix the finish of
									// the chain because we hard-code the use of
									// Eternity. In this way if we want to
									// extend the chain, we're forced to make a
									// copy of the same code because this is a
									// fixed structure (is something like
									// enumerating all our knowledge)
									return list.size() == 0 ? 0 : 1 + Eternity
											.length(list.cdr());
								}
							}).length(list.cdr()); // this invocation executes
													// label:**
				}
			}).length(list.cdr()); // this invocation executes label:*

		}
	};

	public static ListLengthCalculator DecideOnlyEmptyListsUsingHighOrder = new ListLengthCalculator() {

		@Override
		public int length(ListModule list) {

			// for extracting the similarities saw in the previous calculators,
			// we follow the Ninth Commandment, introducing the
			// ListLengthCalculatorHighOrder interface
			return (new ListLengthCalculatorHighOrder() {

				@Override
				public ListLengthCalculator combine(
						final ListLengthCalculator calculator) {

					return new ListLengthCalculator() {

						@Override
						public int length(ListModule list) {
							return list.size() == 0 ? 0 : 1 + calculator
									.length(list.cdr());
						}
					};
				}
			}).combine(Eternity).length(list);

		}
	};

	public static ListLengthCalculator DecideOnlyListWithAtMostOneElementUsingHighOrder = new ListLengthCalculator() {

		@Override
		public int length(ListModule list) {

			return (new ListLengthCalculatorHighOrder() {

				@Override
				public ListLengthCalculator combine(
						final ListLengthCalculator calculator) {

					return new ListLengthCalculator() {

						@Override
						public int length(ListModule list) {
							return list.size() == 0 ? 0 : 1 + calculator
									.length(list.cdr());
						}
					};
				}
			}).combine((new ListLengthCalculatorHighOrder() {

				@Override
				public ListLengthCalculator combine(
						final ListLengthCalculator calculator) {

					return new ListLengthCalculator() {

						@Override
						public int length(ListModule list) {
							return list.size() == 0 ? 0 : 1 + calculator
									.length(list.cdr());
						}
					};
				}
			}).combine(Eternity)).length(list);

		}
	};

	public static ListLengthCalculator DecideOnlyListWithAtMostTwoElementUsingHighOrder = new ListLengthCalculator() {

		@Override
		public int length(ListModule list) {

			// here we see the devastating effect of having introduced the high
			// order object: the structure of the code is the same as before
			// with the same nested levels, with the difference that, using the
			// high-order object we have the SAME code like a truly duplication
			// (before we have the same PATTERN, not the same CODE). What is
			// duplicated is the "common similarities" part, but couldn't be
			// otherwise because that part is the PATTERN in common in the
			// previous code. Now we can observe that the creation of the
			// high-order object isn't necessary to be replicated, but we can
			// use only one of them. This is of course a more truly duplication,
			// hence we are toward another refactoring...
			return (new ListLengthCalculatorHighOrder() {

				@Override
				public ListLengthCalculator combine(
						final ListLengthCalculator calculator) {

					return new ListLengthCalculator() {

						@Override
						public int length(ListModule list) {
							return list.size() == 0 ? 0 : 1 + calculator
									.length(list.cdr());
						}
					};
				}
			}).combine((new ListLengthCalculatorHighOrder() {

				@Override
				public ListLengthCalculator combine(
						final ListLengthCalculator calculator) {

					return new ListLengthCalculator() {

						@Override
						public int length(ListModule list) {
							return list.size() == 0 ? 0 : 1 + calculator
									.length(list.cdr());
						}
					};
				}
			}).combine((new ListLengthCalculatorHighOrder() {

				@Override
				public ListLengthCalculator combine(
						final ListLengthCalculator calculator) {

					return new ListLengthCalculator() {

						@Override
						public int length(ListModule list) {
							return list.size() == 0 ? 0 : 1 + calculator
									.length(list.cdr());
						}
					};
				}
			}).combine(Eternity))).length(list);

		}
	};

	public static ListLengthCalculator DecideOnlyEmptyListsUsingHighOrder_mklength = new ListLengthCalculator() {

		@Override
		public int length(ListModule list) {

			return (new ListLengthCalculatorMaker() {

				@Override
				public ListLengthCalculator map(
						ListLengthCalculatorHighOrder highOrder) {

					return highOrder.combine(Eternity);
				}
			}).map(new ListLengthCalculatorHighOrder() {

				@Override
				public ListLengthCalculator combine(
						final ListLengthCalculator calculator) {

					return new ListLengthCalculator() {

						@Override
						public int length(ListModule list) {
							return list.size() == 0 ? 0 : 1 + calculator
									.length(list.cdr());
						}
					};
				}
			}).length(list);

		}
	};

	public static ListLengthCalculator DecideOnlyListWithAtMostOneElementUsingHighOrder_mklength = new ListLengthCalculator() {

		@Override
		public int length(ListModule list) {

			return (new ListLengthCalculatorMaker() {

				@Override
				public ListLengthCalculator map(
						ListLengthCalculatorHighOrder highOrder) {

					return highOrder.combine(highOrder.combine(Eternity));
				}
			}).map(new ListLengthCalculatorHighOrder() {

				@Override
				public ListLengthCalculator combine(
						final ListLengthCalculator calculator) {

					return new ListLengthCalculator() {

						@Override
						public int length(ListModule list) {
							return list.size() == 0 ? 0 : 1 + calculator
									.length(list.cdr());
						}
					};
				}
			}).length(list);

		}
	};

	public static ListLengthCalculator DecideOnlyListWithAtMostTwoElementUsingHighOrder_mklength = new ListLengthCalculator() {

		@Override
		public int length(ListModule list) {

			// using this algebra-style manipulation, instead of building and
			// invoking three times the high-order object, we build it only once
			// and pass the object to a function that invoke it three times. In
			// this way we divide when we build the object from when we use it.
			// Observe that the nested structure of the invocations of the
			// high-order object is preserved during this transformation.
			return (new ListLengthCalculatorMaker() {

				@Override
				public ListLengthCalculator map(
						ListLengthCalculatorHighOrder highOrder) {

					return highOrder.combine(highOrder.combine(highOrder
							.combine(Eternity)));
				}
			}).map(new ListLengthCalculatorHighOrder() {

				@Override
				public ListLengthCalculator combine(
						final ListLengthCalculator calculator) {

					return new ListLengthCalculator() {

						@Override
						public int length(ListModule list) {
							return list.size() == 0 ? 0 : 1 + calculator
									.length(list.cdr());
						}
					};
				}
			}).length(list);

		}
	};

	public static ListLengthCalculator DecideOnlyListWithAtMostThreeElementUsingHighOrder_mklength = new ListLengthCalculator() {

		@Override
		public int length(ListModule list) {

			return (new ListLengthCalculatorMaker() {

				@Override
				public ListLengthCalculator map(
						ListLengthCalculatorHighOrder highOrder) {

					return highOrder.combine(highOrder.combine(highOrder
							.combine(highOrder.combine(Eternity))));
				}
			}).map(new ListLengthCalculatorHighOrder() {

				@Override
				public ListLengthCalculator combine(
						final ListLengthCalculator calculator) {

					return new ListLengthCalculator() {

						@Override
						public int length(ListModule list) {
							return list.size() == 0 ? 0 : 1 + calculator
									.length(list.cdr());
						}
					};
				}
			}).length(list);

		}
	};

	/**
	 * First version equivalent to the normal length implementation.
	 */
	public static ListLengthCalculator DecideListLengthUsingSelfRecursion = new ListLengthCalculator() {

		@Override
		public int length(ListModule list) {

			// introducing the self invocation allow us to add a little brick
			// every time we need in order to match the length of the list.
			return (new ListLengthCalculatorRecursiveInvocation() {

				@Override
				public ListLengthCalculator invokeWithSelfRecursion( // label:*
						ListLengthCalculatorRecursiveInvocation self) {

					return self.invokeWithSelfRecursion(self); // invokes
																// label:**

				}
			}).invokeWithSelfRecursion( // invokes label:*
					new ListLengthCalculatorRecursiveInvocation() {

						// using the self recursion instead of the high-order
						// object we loose the syntactic equivalence of the code
						// that compute the length. Instead of using the
						// calculator as done in the previous version here we
						// use the self invocation
						// self.invokeWithSelfRecursion(self) which build a
						// calculator, but this is far different than using a
						// collector available from the high-order object.

						@Override
						public ListLengthCalculator invokeWithSelfRecursion( // label:**
								final ListLengthCalculatorRecursiveInvocation self) {

							return new ListLengthCalculator() {

								@Override
								public int length(ListModule list) {

									// the invocation
									// self.invokeWithSelfRecursion(self)
									// doesn't produce a loop because it build
									// an object of type ListLengthCalculator,
									// without calling the length method (this
									// method!) which, if invoked, will
									// produce a loop instead.
									return list.size() == 0 ? 0 : 1 + self
											.invokeWithSelfRecursion(self)
											.length(list.cdr());
								}
							};
						}
					}).length(list);

		}
	};

	public static ListLengthCalculator UndecideListLengthExtractingSelfRecursion = new ListLengthCalculator() {

		@Override
		public int length(ListModule list) {

			return (new ListLengthCalculatorRecursiveInvocation() {

				@Override
				public ListLengthCalculator invokeWithSelfRecursion(
						ListLengthCalculatorRecursiveInvocation self) {

					return self.invokeWithSelfRecursion(self);

				}
			}).invokeWithSelfRecursion(
					new ListLengthCalculatorRecursiveInvocation() {

						@Override
						public ListLengthCalculator invokeWithSelfRecursion(
								final ListLengthCalculatorRecursiveInvocation self) {

							// in this way we return back using the high-order
							// object and this structure is the same as the
							// firsts, where the code that asks on the lists use
							// the passed calculator. Here we've used the same
							// algebra-style manipulation: build the necessary
							// object (in this case the construction is done in
							// the invocation
							// self.invokeWithSelfRecursion(self)) pass it to an
							// high-order object and from the inside invoke it.
							// This produce an error not in this algebra
							// manipulation, but in the self invocation which
							// produce an infinite loop.
							return (new ListLengthCalculatorHighOrder() {

								@Override
								public ListLengthCalculator combine(
										final ListLengthCalculator calculator) {

									return new ListLengthCalculator() {

										@Override
										public int length(ListModule list) {

											return list.size() == 0 ? 0
													: 1 + calculator
															.length(list.cdr());

										}
									};

								}
							}).combine(self.invokeWithSelfRecursion(self));

						}
					}).length(list);

		}
	};

	public static ListLengthCalculator DecideListLengthUsingSelfRecursionTowardYCombinatorFirstStep = new ListLengthCalculator() {

		@Override
		public int length(ListModule list) {

			return (new ListLengthCalculatorRecursiveInvocation() {

				@Override
				public ListLengthCalculator invokeWithSelfRecursion(
						ListLengthCalculatorRecursiveInvocation self) {

					return self.invokeWithSelfRecursion(self);

				}
			}).invokeWithSelfRecursion(
					new ListLengthCalculatorRecursiveInvocation() {

						// in this version we haven't the high-order object, but
						// we make a little step toward its re-introduction

						@Override
						public ListLengthCalculator invokeWithSelfRecursion(
								final ListLengthCalculatorRecursiveInvocation self) {

							return new ListLengthCalculator() {

								@Override
								public int length(ListModule list) {
									return list.size() == 0 ? 0 : 1 +
									// building here a calculator object and
									// invoking it allow us to "reintroduce" the
									// concept of a calculator (like the first
									// versions) which hides the use of the self
									// recursion.
											(new ListLengthCalculator() {

												@Override
												public int length(
														ListModule list) {
													return self
															.invokeWithSelfRecursion(
																	self)
															.length(list);
												}
											}).length(list.cdr());
								}
							};
						}
					}).length(list);

		}
	};

	public static ListLengthCalculator DecideListLengthUsingSelfRecursionTowardYCombinatorSecondStep = new ListLengthCalculator() {

		@Override
		public int length(ListModule list) {

			return (new ListLengthCalculatorRecursiveInvocation() {

				@Override
				public ListLengthCalculator invokeWithSelfRecursion(
						ListLengthCalculatorRecursiveInvocation self) {

					return self.invokeWithSelfRecursion(self);

				}
			}).invokeWithSelfRecursion(
					new ListLengthCalculatorRecursiveInvocation() {

						@Override
						public ListLengthCalculator invokeWithSelfRecursion(
								final ListLengthCalculatorRecursiveInvocation self) {

							// here we reintroduce the high-order object just
							// with the same algebra manipulation: the moral
							// behind this re-introduction is that when we are
							// building an object dynamically, don't define it
							// where needed and use it immediately, instead
							// build it and pass it to an high-order object
							// which in turn will invoke it. We observe that
							// doing this, the object self is no more used in
							// the definition of the high-order object, hence we
							// have another refactor to do! (before, where we
							// build dynamically the calculator directly, we
							// have the dependency from self in the compound
							// where we did the definition.
							return (new ListLengthCalculatorHighOrder() {

								@Override
								public ListLengthCalculator combine(
										final ListLengthCalculator calculator) {

									return new ListLengthCalculator() {

										@Override
										public int length(ListModule list) {
											return list.size() == 0 ? 0
													: 1 + calculator
															.length(list.cdr());
										}
									};
								}
							}).combine(new ListLengthCalculator() {

								@Override
								public int length(ListModule list) {
									return self.invokeWithSelfRecursion(self)
											.length(list);
								}
							});
						}
					}).length(list);

		}
	};

	public static ListLengthCalculator DecideListLengthUsingSelfRecursionTowardYCombinatorThirdStep = new ListLengthCalculator() {

		@Override
		public int length(ListModule list) {

			// another application of the algebra-style manipulation: build the
			// high-order object outside the place where you need it, pass it to
			// another high-order object and then use it through its reference
			// parameter.
			return (new ListLengthCalculatorMaker() {

				@Override
				public ListLengthCalculator map(
						final ListLengthCalculatorHighOrder highOrder) {

					return (new ListLengthCalculatorRecursiveInvocation() {

						@Override
						public ListLengthCalculator invokeWithSelfRecursion(
								ListLengthCalculatorRecursiveInvocation self) {

							return self.invokeWithSelfRecursion(self);

						}
					}).invokeWithSelfRecursion(new ListLengthCalculatorRecursiveInvocation() {

						@Override
						public ListLengthCalculator invokeWithSelfRecursion(
								final ListLengthCalculatorRecursiveInvocation self) {

							return highOrder
									.combine(new ListLengthCalculator() {

										@Override
										public int length(ListModule list) {
											return self
													.invokeWithSelfRecursion(
															self).length(list);
										}
									});
						}
					});
				}
			}).map(new ListLengthCalculatorHighOrder() {

				@Override
				public ListLengthCalculator combine(
						final ListLengthCalculator calculator) {

					return new ListLengthCalculator() {

						@Override
						public int length(ListModule list) {
							return list.size() == 0 ? 0 : 1 + calculator
									.length(list.cdr());
						}
					};
				}
			}).length(list);

		}
	};

}
