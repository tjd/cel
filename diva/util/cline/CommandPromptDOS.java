package util.cline;

import java.util.StringTokenizer;
import java.util.Enumeration;
import java.util.Hashtable;
import java.io.*;

/**
 * Command line for text i/o programs. 
 * @see CommandPromptAWT
 */
public class CommandPromptDOS implements CommandPromptInterface {

	private BufferedReader input_;
	private CommandRegister register_;

	public CommandPromptDOS(InputStream input) {
		input_ = new BufferedReader(new InputStreamReader(input));
	}

	public void setRegister(CommandRegister cregister) {
		register_ = cregister;
	}
	
	public void print(String string) { System.out.print(string); }
	public void println(String string) { System.out.println(string); }
	public void print(Object object) { System.out.print(object); }
	public void println(Object object) { System.out.println(object); }

	public void message(String message) {
		// print the message and then display the prompt again!
		println(message);
		printPrompt();
	}

	public void printPrompt() {
		print("\n" + register_.getName() + "> "); 
		System.out.flush();
	}

	public void start() {
		for(;;) {
			printPrompt();
			String command = null;
			try {
				command = input_.readLine();
				if (command != null) {
					register_.executeCommand(register_.parseCommand(command));
				}
			} catch (CommandException e) {
				println(e);
			} catch (IOException e) {
				println("Error: could not read line");
			}
		}
	}

	public boolean askBooleanQuestion(String question) {
		String response = askQuestion(question, "n");
		if (response.equalsIgnoreCase("yes") || 
			response.equalsIgnoreCase("y")) {
			return true;
		} 
		return false;
	}

	public String askQuestion(String question) {
		return askQuestion(question, null);
	}
	
	public String askQuestion(String question, String defaultAns) {
		print(question);
		if (defaultAns != null) {
			print(" (" + defaultAns + ")"); 
		}
		print(": ");
		System.out.flush();
		String response = "";

		try {
			response = input_.readLine();
			if (defaultAns != null &&
				(response.equalsIgnoreCase("") ||
				response.equalsIgnoreCase("\n"))) {
				return defaultAns;
			}
		} catch (IOException e) {
			System.err.println("Error: could not read response");
		}
		return response;
	}
}
