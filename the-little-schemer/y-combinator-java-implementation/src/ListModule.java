import java.util.LinkedList;
import java.util.List;

public class ListModule {
	private List<Object> internal_list = new LinkedList<Object>();

	private ListModule(List<Object> list) {
		internal_list = new LinkedList<Object>(list);
	}

	public Object car() {
		return internal_list.get(0);
	}

	public ListModule cdr() {
		return new ListModule(internal_list.subList(1, internal_list.size()));
	}

	public ListModule cons(Object obj) {
		ListModule new_list = new ListModule(internal_list);
		new_list.internal_list.add(0, obj);
		return new_list;
	}

	public static ListModule nil() {
		return new ListModule(new LinkedList<Object>());
	}

	public int size() {
		return internal_list.size();
	}

	public static ListModule makeWithRequestedCardinality(int cardinality) {
		ListModule result = ListModule.nil();

		for (int counter = 0; counter < cardinality; counter = counter + 1) {

			result = result.cons(new Object());
		}

		return result;

	}
}
