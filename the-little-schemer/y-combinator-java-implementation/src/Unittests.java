import org.junit.Assert;
import org.junit.Test;

import Ycombinator.Ycombinator;
import Ycombinator.Ycombinator.HighOrderCombinatorFor;
import Ycombinator.Ycombinator.InterfaceType;

public class Unittests {

	private static ListModule empty_list = ListModule.nil();
	private static ListModule list_with_one_element = ListModule.nil().cons(
			new Object());
	private static ListModule list_with_two_elements = ListModule.nil()
			.cons(new Object()).cons(new Object());
	private static ListModule list_with_three_elements = ListModule.nil()
			.cons(new Object()).cons(new Object()).cons(new Object());
	private static ListModule list_with_four_elements = ListModule.nil()
			.cons(new Object()).cons(new Object()).cons(new Object())
			.cons(new Object());
	private static ListModule list_with_five_elements = ListModule.nil()
			.cons(new Object()).cons(new Object()).cons(new Object())
			.cons(new Object()).cons(new Object());

	@Test
	public void checking_lists_for_tests_execution() {

		Assert.assertEquals(0, empty_list.size());
		Assert.assertEquals(1, list_with_one_element.size());
		Assert.assertEquals(2, list_with_two_elements.size());
		Assert.assertEquals(3, list_with_three_elements.size());
		Assert.assertEquals(4, list_with_four_elements.size());
		Assert.assertEquals(5, list_with_five_elements.size());
	}

	@Test
	public void checking_lists_for_requested_cardinality() {

		int cardinality = 54;
		Assert.assertEquals(cardinality, ListModule
				.makeWithRequestedCardinality(cardinality).size());

	}

	@Test
	public void default_list_length_calculator_should_compute_correctly() {

		ListLengthCalculator calculator = ListLengthCalculators.NormalRecursion;

		Assert.assertEquals(0, calculator.length(empty_list));

		Assert.assertEquals(3, calculator.length(list_with_three_elements));
		Assert.assertEquals(2, calculator.length(list_with_two_elements));
		Assert.assertEquals(1, calculator.length(list_with_one_element));
	}

	@Test(expected = StackOverflowError.class)
	public void run_eternity_calculator_on_empty_list_should_produce_stack_overflow() {

		Assert.assertEquals(-1,
				ListLengthCalculators.Eternity.length(empty_list));
	}

	@Test
	public void run_DecideOnlyEmptyLists_with_empty_list() {

		Assert.assertEquals(0,
				ListLengthCalculators.DecideOnlyEmptyLists.length(empty_list));
	}

	@Test(expected = StackOverflowError.class)
	public void run_DecideOnlyEmptyLists_with_non_empty_list_should_produce_stack_overflow() {

		Assert.assertEquals(-1, ListLengthCalculators.DecideOnlyEmptyLists
				.length(list_with_one_element));
	}

	@Test
	public void run_DecideOnlyListsWithAtMostOneElement_with_empty_list() {

		Assert.assertEquals(0,
				ListLengthCalculators.DecideOnlyListWithAtMostOneElement
						.length(empty_list));
	}

	@Test
	public void run_DecideOnlyListsWithAtMostOneElement_with_one_element_in_list() {

		Assert.assertEquals(1,
				ListLengthCalculators.DecideOnlyListWithAtMostOneElement
						.length(list_with_one_element));
	}

	@Test(expected = StackOverflowError.class)
	public void run_DecideOnlyListsWithAtMostOneElement_with_two_element_in_list_should_produce_stack_overflow() {

		Assert.assertEquals(-1,
				ListLengthCalculators.DecideOnlyListWithAtMostOneElement
						.length(list_with_two_elements));
	}

	@Test
	public void run_DecideOnlyListsWithAtMostTwoElement_with_empty_list() {

		Assert.assertEquals(0,
				ListLengthCalculators.DecideOnlyListWithAtMostTwoElements
						.length(empty_list));
	}

	@Test
	public void run_DecideOnlyListsWithAtMostTwoElement_with_one_element_in_list() {

		Assert.assertEquals(1,
				ListLengthCalculators.DecideOnlyListWithAtMostTwoElements
						.length(list_with_one_element));
	}

	@Test
	public void run_DecideOnlyListsWithAtMostTwoElement_with_two_element_in_list() {

		Assert.assertEquals(2,
				ListLengthCalculators.DecideOnlyListWithAtMostTwoElements
						.length(list_with_two_elements));
	}

	@Test(expected = StackOverflowError.class)
	public void run_DecideOnlyListsWithAtMostTwoElement_with_two_element_in_list_should_produce_stack_overflow() {

		Assert.assertEquals(-1,
				ListLengthCalculators.DecideOnlyListWithAtMostTwoElements
						.length(list_with_three_elements));
	}

	@Test
	public void run_DecideOnlyEmptyLists_using_high_order_creation_with_empty_list() {

		Assert.assertEquals(0,
				ListLengthCalculators.DecideOnlyEmptyListsUsingHighOrder
						.length(empty_list));
	}

	@Test(expected = StackOverflowError.class)
	public void run_DecideOnlyEmptyLists_using_high_order_creation_with_non_empty_list_should_produce_stack_overflow() {

		Assert.assertEquals(0,
				ListLengthCalculators.DecideOnlyEmptyListsUsingHighOrder
						.length(list_with_one_element));
	}

	@Test
	public void run_DecideOnlyListsWithAtMostOneElement_using_high_order_creation_with_empty_list() {

		Assert.assertEquals(
				0,
				ListLengthCalculators.DecideOnlyListWithAtMostOneElementUsingHighOrder
						.length(empty_list));
	}

	@Test
	public void run_DecideOnlyListsWithAtMostOneElement_using_high_order_creation_with_one_element_in_list() {

		Assert.assertEquals(
				1,
				ListLengthCalculators.DecideOnlyListWithAtMostOneElementUsingHighOrder
						.length(list_with_one_element));
	}

	@Test(expected = StackOverflowError.class)
	public void run_DecideOnlyListsWithAtMostOneElement_using_high_order_creation_with_two_element_in_list_should_produce_stack_overflow() {

		Assert.assertEquals(
				-1,
				ListLengthCalculators.DecideOnlyListWithAtMostOneElementUsingHighOrder
						.length(list_with_two_elements));
	}

	@Test
	public void run_DecideOnlyListsWithAtMostTwoElement_using_high_order_with_empty_list() {

		Assert.assertEquals(
				0,
				ListLengthCalculators.DecideOnlyListWithAtMostTwoElementUsingHighOrder
						.length(empty_list));
	}

	@Test
	public void run_DecideOnlyListsWithAtMostTwoElement_using_high_order_with_one_element_in_list() {

		Assert.assertEquals(
				1,
				ListLengthCalculators.DecideOnlyListWithAtMostTwoElementUsingHighOrder
						.length(list_with_one_element));
	}

	@Test
	public void run_DecideOnlyListsWithAtMostTwoElement_using_high_order_with_two_element_in_list() {

		Assert.assertEquals(
				2,
				ListLengthCalculators.DecideOnlyListWithAtMostTwoElementUsingHighOrder
						.length(list_with_two_elements));
	}

	@Test(expected = StackOverflowError.class)
	public void run_DecideOnlyListsWithAtMostTwoElement_using_high_order_with_two_element_in_list_should_produce_stack_overflow() {

		Assert.assertEquals(
				-1,
				ListLengthCalculators.DecideOnlyListWithAtMostTwoElementUsingHighOrder
						.length(list_with_three_elements));
	}

	@Test
	public void run_DecideOnlyEmptyLists_using_mklength_creation_with_empty_list() {

		Assert.assertEquals(
				0,
				ListLengthCalculators.DecideOnlyEmptyListsUsingHighOrder_mklength
						.length(empty_list));
	}

	@Test(expected = StackOverflowError.class)
	public void run_DecideOnlyEmptyLists_using_mklength_creation_with_non_empty_list_should_produce_stack_overflow() {

		Assert.assertEquals(
				0,
				ListLengthCalculators.DecideOnlyEmptyListsUsingHighOrder_mklength
						.length(list_with_one_element));
	}

	@Test
	public void run_DecideOnlyListsWithAtMostOneElement_using_mklength_creation_with_empty_list() {

		Assert.assertEquals(
				0,
				ListLengthCalculators.DecideOnlyListWithAtMostOneElementUsingHighOrder_mklength
						.length(empty_list));
	}

	@Test
	public void run_DecideOnlyListsWithAtMostOneElement_using_mklength_creation_with_one_element_in_list() {

		Assert.assertEquals(
				1,
				ListLengthCalculators.DecideOnlyListWithAtMostOneElementUsingHighOrder_mklength
						.length(list_with_one_element));
	}

	@Test(expected = StackOverflowError.class)
	public void run_DecideOnlyListsWithAtMostOneElement_using_mklength_creation_with_two_element_in_list_should_produce_stack_overflow() {

		Assert.assertEquals(
				-1,
				ListLengthCalculators.DecideOnlyListWithAtMostOneElementUsingHighOrder_mklength
						.length(list_with_two_elements));
	}

	@Test
	public void run_DecideOnlyListsWithAtMostTwoElement_using_mklength_with_empty_list() {

		Assert.assertEquals(
				0,
				ListLengthCalculators.DecideOnlyListWithAtMostTwoElementUsingHighOrder_mklength
						.length(empty_list));
	}

	@Test
	public void run_DecideOnlyListsWithAtMostTwoElement_using_mklength_with_one_element_in_list() {

		Assert.assertEquals(
				1,
				ListLengthCalculators.DecideOnlyListWithAtMostTwoElementUsingHighOrder_mklength
						.length(list_with_one_element));
	}

	@Test
	public void run_DecideOnlyListsWithAtMostTwoElement_using_mklength_with_two_element_in_list() {

		Assert.assertEquals(
				2,
				ListLengthCalculators.DecideOnlyListWithAtMostTwoElementUsingHighOrder_mklength
						.length(list_with_two_elements));
	}

	@Test(expected = StackOverflowError.class)
	public void run_DecideOnlyListsWithAtMostTwoElement_using_mklength_with_two_element_in_list_should_produce_stack_overflow() {

		Assert.assertEquals(
				-1,
				ListLengthCalculators.DecideOnlyListWithAtMostTwoElementUsingHighOrder_mklength
						.length(list_with_three_elements));
	}

	@Test
	public void run_DecideOnlyListsWithAtMostThreeElement_using_mklength_with_empty_list() {

		Assert.assertEquals(
				0,
				ListLengthCalculators.DecideOnlyListWithAtMostThreeElementUsingHighOrder_mklength
						.length(empty_list));
	}

	@Test
	public void run_DecideOnlyListsWithAtMostThreeElement_using_mklength_with_one_element_in_list() {

		Assert.assertEquals(
				1,
				ListLengthCalculators.DecideOnlyListWithAtMostThreeElementUsingHighOrder_mklength
						.length(list_with_one_element));
	}

	@Test
	public void run_DecideOnlyListsWithAtMostThreeElement_using_mklength_with_two_element_in_list() {

		Assert.assertEquals(
				2,
				ListLengthCalculators.DecideOnlyListWithAtMostThreeElementUsingHighOrder_mklength
						.length(list_with_two_elements));
	}

	@Test
	public void run_DecideOnlyListsWithAtMostThreeElement_using_mklength_with_three_element_in_list() {

		Assert.assertEquals(
				3,
				ListLengthCalculators.DecideOnlyListWithAtMostThreeElementUsingHighOrder_mklength
						.length(list_with_three_elements));
	}

	@Test(expected = StackOverflowError.class)
	public void run_DecideOnlyListsWithAtMostThreeElement_using_mklength_with_four_element_in_list_should_produce_stack_overflow() {

		Assert.assertEquals(
				-1,
				ListLengthCalculators.DecideOnlyListWithAtMostThreeElementUsingHighOrder_mklength
						.length(list_with_four_elements));
	}

	@Test
	public void run_DecideListLengthUsingSelfRecursion_with_all_list() {

		assert_correct_computation_for_all_testing_lists(ListLengthCalculators.DecideListLengthUsingSelfRecursion);

	}

	@Test
	public void run_DecideListLengthUsingSelfRecursionTowardYCombinatorFirstStep_with_all_list() {

		assert_correct_computation_for_all_testing_lists(ListLengthCalculators.DecideListLengthUsingSelfRecursionTowardYCombinatorFirstStep);

	}

	@Test
	public void run_DecideListLengthUsingSelfRecursionTowardYCombinatorSecondStep_with_all_list() {

		assert_correct_computation_for_all_testing_lists(ListLengthCalculators.DecideListLengthUsingSelfRecursionTowardYCombinatorSecondStep);

	}

	@Test
	public void run_DecideListLengthUsingSelfRecursionTowardYCombinatorThirsStep_with_all_list() {

		assert_correct_computation_for_all_testing_lists(ListLengthCalculators.DecideListLengthUsingSelfRecursionTowardYCombinatorThirdStep);

	}

	private void assert_correct_computation_for_all_testing_lists(
			ListLengthCalculator calculator) {

		Assert.assertEquals(0, calculator.length(empty_list));

		Assert.assertEquals(1, calculator.length(list_with_one_element));

		Assert.assertEquals(2, calculator.length(list_with_two_elements));

		Assert.assertEquals(3, calculator.length(list_with_three_elements));

		Assert.assertEquals(4, calculator.length(list_with_four_elements));

		Assert.assertEquals(5, calculator.length(list_with_five_elements));

	}

	@Test(expected = StackOverflowError.class)
	public void run_UndecideListLengthExtractingSelfRecursion_with_empty_list() {

		Assert.assertEquals(0,
				ListLengthCalculators.UndecideListLengthExtractingSelfRecursion
						.length(empty_list));
	}

	@Test
	public void YCombinator_examples() {
		Ycombinator<ListModule, Integer> y_combinator = new Ycombinator<ListModule, Integer>();

		InterfaceType<ListModule, Integer> entire_computation = y_combinator
				.recursion(new HighOrderCombinatorFor<Ycombinator.InterfaceType<ListModule, Integer>>() {

					@Override
					public InterfaceType<ListModule, Integer> combine(
							final InterfaceType<ListModule, Integer> calculator) {

						return new InterfaceType<ListModule, Integer>() {

							@Override
							public Integer compute(ListModule input) {

								return input.size() == 0 ? 0 : 1 + calculator
										.compute(input.cdr());
							}
						};
					}
				});

		Assert.assertEquals((Integer) 0, entire_computation.compute(empty_list));

		Assert.assertEquals((Integer) 1,
				entire_computation.compute(list_with_one_element));

		Assert.assertEquals((Integer) 2,
				entire_computation.compute(list_with_two_elements));

		Assert.assertEquals((Integer) 3,
				entire_computation.compute(list_with_three_elements));

		Assert.assertEquals((Integer) 4,
				entire_computation.compute(list_with_four_elements));

		Assert.assertEquals((Integer) 5,
				entire_computation.compute(list_with_five_elements));

		Integer cardinality = 190;
		Assert.assertEquals(cardinality, entire_computation.compute(ListModule
				.makeWithRequestedCardinality(cardinality)));

	}
}
