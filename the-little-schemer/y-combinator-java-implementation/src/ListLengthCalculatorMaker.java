/**
 * In order to remove the duplication given by the repetitive definition of the
 * "common similarities" part of high-order object, we introduce a little
 * "algebra-style" manipulation, that is we introduce an object that takes the
 * high-order object and returns what the high-order object returns.
 * 
 * @author massimo
 * 
 */
public interface ListLengthCalculatorMaker {
	ListLengthCalculator map(ListLengthCalculatorHighOrder highOrder);
}
