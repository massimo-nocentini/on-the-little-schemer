/**
 * This interface allow us to stick to the Ninth Commandment, in particular it
 * allow us to abstract the common similarities.<br>
 * <br>
 * What does that mean? In order to "abstract the common similarities" we have
 * always to define an high order object, such that keep as argument what
 * changes (as form of a function) and return a function which encapsulate the
 * common behavior. The returned function do curry-ing on the passed argument,
 * so that the high-order object is a factory of functions.<br>
 * <br>
 * To be a little more formal this interface define the responsibility that an
 * implementor should have to be used as an high order object: it have to take a
 * function and return a function with the same signature (and semantic).
 * 
 * @author massimo
 * 
 */
public interface ListLengthCalculatorHighOrder {

	ListLengthCalculator combine(ListLengthCalculator calculator);
}
