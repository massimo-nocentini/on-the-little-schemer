import org.junit.Assert;
import org.junit.Test;

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

}
