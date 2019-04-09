package util.cline;

import java.util.Enumeration;
import java.util.Vector;

/**
 * Base class to create a named command. The BaseCommand constructor should
 * be called from the constructor of a derived class that is an actual 
 * command. For example:
 * <P><PRE>
 * class ExitCommand extends BaseCommand { 
 * 		ExitCommand() { super("exit"); }
 * }
 * </PRE>
 * @see RunScriptCommand
 */
public abstract class BaseCommand implements Command {

	protected String name_;
	protected int numArgs_;
	protected CommandPromptInterface prompt_; 	
	protected CommandRegister register_;
	protected String help_;
	
	/**
	 * This constructor should be called by all inheriting classes.
	 * ie. they should call <PRE>super(name, numArgs, help)</PRE>
	 * @param name the name of the command
	 * @param numArgs the number of arguments this command takes.
	 * -1 can be specified if the command takes a variable number of 
	 * arguments.
	 * @param help this string is returned to the user when they request 
	 * help about a command. It should include a brief description of what
	 * the command accomplishes and what each of the required arguments
	 * represents.
	 */
	protected BaseCommand(String name, int numArgs, String help) {
		name_ = name;
		numArgs_ = numArgs;
		help_ = help;
	}
	
	protected CommandPromptInterface getPrompt() {
		return prompt_;
	}

	protected CommandRegister getRegister() {
		return register_;
	}

	/**
	 * Make sure this method is called after a Command is constructed
	 * so that help topics have access to each other.
	 */
	public void set(CommandRegister cregister, CommandPromptInterface prompt) {
		register_ = cregister;
		prompt_ = prompt;
	}

	public String getName() { return name_; }

	public abstract void execute(String args[]);

	public void execute(Enumeration enum) {
		String args[] = null;
		if (numArgs_ != -1) {
			args = new String[numArgs_];
			for (int i=0; i<numArgs_; i++) {
				if (!enum.hasMoreElements()) {
					prompt_.println(getHelp());
					return;
				}
				args[i] = (String)enum.nextElement();
			}
		} else {	// unknown number of arguments indicated by -1
			Vector vector = new Vector(5);
			while(enum.hasMoreElements()) {
				vector.add(enum.nextElement());
			}
			args = new String[vector.size()];
			vector.toArray(args);
		}
		execute(args);
	}

	public String getHelp() { return "Usage: " + name_ + " " + help_; }
}

