// DIVA (c) David Croft, 2000. University of Waterloo

package cogsci;

import util.cline.*;	// the command line user interface package
import java.io.*;
import java.text.NumberFormat;

import cogsci.noa.*;
import cogsci.imagery.*;

/**
 * This class contains the main() method and controls the start-up and 
 * execution of the program. It also includes a number of public static 
 * variables that are queried by other classes (eg. the name of the model,
 * default model storage directory).
 */
public class Control {
	
	private static ControlGUI gui_;
	private static CommandRegister cr_;
	private static NumberFormat numberFormat_;
	
	// This is the list of "decoders" that is currently register for the 
	// application. Each decoder is responsible for displaying and processing
	// a specific type of data, whether it be 3-dimensional imagery data 
	// (this is done by the imagery.MindsEye decoder) or whether it is text
	// information stored in the network of association (see the noa package).
	public static final int NUM_DECODERS = 3;
	public static final int IMAGERY = 0;
	public static final int TEXT = 1;
	public static final int FILE = 2;

	public static final String NAME = "diva";
	public static final String FULL_NAME = "diva";
	public static final String LIBRARY_NAME = "c:\\storage\\models";
	private static BlobDecoder decoders_[] = new BlobDecoder[NUM_DECODERS];
	
	/**
	 * Here is the main function for the program. It will return if the JDK 
	 * version is not higher than 1.2.
	 */
	public static void main(String args[]) {
		String vers = System.getProperty("java.version");
		if (vers.compareTo("1.2") < 0) {
			System.out.println(
				"!!! ERROR: must be run with a Java VRM version " +
				"1.2 or higher !!!");
		}
		new Control(args);
	}
	
	/**
	 * This method sets the decoder associated with one of the integer 
	 * codes for this class (eg. IMAGERY, TEXT, FILE)
	 */
	public static void setDecoder(BlobDecoder decoder, int type) {
		decoders_[type] = decoder;
	}
	
	/**
	 * Returns the decoder associated with an integer code 
	 * (eg. IMAGERY, TEXT, FILE).
	 */
	public static BlobDecoder getDecoder(int type) {
		return decoders_[type];
	}
	
	/**
	 * Constructor for the main control class. This is called by the main()
	 * method. This constructor creates the main user interface frame 
	 * (Imagine), initializes the command prompt and registers commands 
	 * for this package, the noa package, and any commands that are 
	 * required for a decoder (for each decoder, registerCommands() is 
	 * called.
	 */
	public Control(String[] files) {
		if (files.length > 0 && files[0] != null) {
			NOA.load(files[0]);
		} else {
			NOA.initialize(NAME);
		}
		
		gui_ = new Imagine();
		cr_ = new CommandRegister(gui_.getPrompt(), NAME); 
		numberFormat_ = NumberFormat.getInstance();

		// Control commands
		cr_.registerCommand(new ExitCommand());
		cr_.registerCommand(new QuestionCommand());
		cr_.registerCommand(new ViewMemoryCommand());
		
		// register NOA commands
		cr_.registerCommand(new TimeCommand());
		cr_.registerCommand(new LoadMemoryCommand());
		cr_.registerCommand(new SaveMemoryCommand());
		cr_.registerCommand(new AssociateCommand());
		cr_.registerCommand(new ActivateCommand());
		cr_.registerCommand(new ProblemSolveCommand());
		cr_.registerCommand(new PrintHubCommand());
		cr_.registerCommand(new DecodeCommand());
		cr_.registerCommand(new ListHubsCommand());
		cr_.registerCommand(new ReadBookCommand());

		// register commands from any of the decoders
		for (int i=0; i<NUM_DECODERS; i++) {
			decoders_[i].registerCommands();
		}

		gui_.show();
	}
	/** 
	 * This method is called by the decoders to register commands 
	 * with the Control object's command prompt.
	 */
	public static void registerCommand(Command command) {
		cr_.registerCommand(command);
	}
	
	/**
	 * Method to format a double number as a string with a specified 
	 * number of fracton digits. For example, format(1.345366, 2) returns 
	 * a string "1.34"
	 */
	public static String format(double number, int fractionDigits) {
		numberFormat_.setMaximumFractionDigits(fractionDigits);
		return numberFormat_.format(number);
	}
	
	/**
	 * This method displays status messages to the Control GUI. If not 
	 * gui exists, then the message is directed to standard output.
	 */
	public static void status(String string) {
		if (gui_ == null) {
			System.out.println(string);
		} else {
			gui_.getPrompt().message(string);
		}
	}

	/**
	 * Calls the toString() method for an object and then displays 
	 * this as a status message.
	 */
	public static void status(Object object) {
		status(object.toString());
	}
	
	/**
	 * Prepends the message with "Error: " and redirects it to the GUI 
	 * and standard error.
	 */
	public static void error(String message) {
		if (gui_ == null) {
			System.err.println("Error: " + message);
		} else {
			gui_.getPrompt().message("Error: " + message);
			System.err.println("Error: " + message);
		}
	}
	
	/**
	 * Same as the error(String) method however it also prints out the 
	 * stack trace from the exception to standard error.
	 */
	public static void error(String message, Exception e) {
		error(message);
		e.printStackTrace(System.err);
	}
	
	/**
	 * Calls the toString() method for this object and then 
	 * calls the error(String) method.
	 */
	public static void error(Object object) {
		error(object.toString());
	}
	
	/**
	 * Asks a user a question through the GUI (usually this puts up a 
	 * dialog box with a question in which the user can enter a response).
	 */
	public static String askQuestion(String question) {
		return gui_.getPrompt().askQuestion(question);
	}
	
	/**
	 * Asks a question and includes a default answer, so that the user can 
	 * just hit enter.
	 */
	public static String askQuestion(String question, String defaultAnswer) {
		return gui_.getPrompt().askQuestion(question, defaultAnswer);
	}
	
	/**
	 * Asks a boolean question to the user.
	 */
	public static boolean askBooleanQuestion(String question) {
		return gui_.getPrompt().askBooleanQuestion(question);
	}
	
	/**
	 * Activates a named memory in the network of association and decodes 
	 * the named concept into working memory. Of course, if no concept with 
	 * the given name exists, nothing will be displayed. A list of concepts 
	 * can be viewed using the noa.ListHubsCommand.
	 */
	public static void viewMemory(String name) {
		gui_.viewMemory(name);
	}
	
	/**
	 * Makes the specified decoder active on the GUI.
	 */
	public static void activateDecoder(BlobDecoder decoder) {
		gui_.activateDecoder(decoder);
	}
	
	/**
	 * Saves the current memory file and exits. The default file name 
	 * for the file is NAME + ".dat".
	 */
	public static void exit() {
		NOA.save(NAME + ".dat");
		System.exit(0);
	}
}
