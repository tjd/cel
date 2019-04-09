package util.debug;

/**
 * Contains static debugging methods.
 */
public class Debug
{
	// disable the default constructor
	private Debug() {}

	public static void ASSERT(boolean expression, String message) {
		if (expression == false) {
			System.err.println("ASSERTION: " + message);
			try {
				throw new RuntimeException(message);
			} catch (RuntimeException e) {
				e.printStackTrace(System.err);
			}
			System.err.println("Program exiting");
			System.exit(1);
		}
	}
	
	public static void ASSERT(Object object, String message) {
		if (object == null) {
			ASSERT(false, message);
		}
	}

	public static void ASSERT(Object object) {
		ASSERT(object, "null object");
	}
}

	
