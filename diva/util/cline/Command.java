package util.cline;

import java.util.Enumeration;

/**
 * Interface that all commands need to represent. Note that this class
 * is implemented by the BaseCommand class.
 *
 * @see BaseCommand
 */
public interface Command {
	String getName();
	String getHelp();
	void execute(String args[]);
	void execute(Enumeration enum);
	void set(CommandRegister cregister, CommandPromptInterface prompt);
}

	
