package util.cline;

import java.util.StringTokenizer;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;
import java.io.*;

public class CommandRegister {

	protected Hashtable commands_ = new Hashtable(50);
	protected String name_;	// to display for prompt

	protected CommandPromptInterface prompt_;

	public CommandRegister(	CommandPromptInterface prompt, 
							String name) {
		prompt_ = prompt;
		prompt_.setRegister(this);
		name_ = name;

		// register default commands 
		registerCommand(new HelpCommand());
		registerCommand(new RunScriptCommand());
	}

	Hashtable getCommands() { return commands_; }
	String getName() { return name_; }

	public void registerCommand(Command command) {
		command.set(this, prompt_);
		commands_.put(command.getName(), command);
	}

	private final Enumeration parseCommandNoQuotes(String input) {
		StringTokenizer tokenizer = new StringTokenizer(input, " \t");
		return (Enumeration)tokenizer;
	}

	final Enumeration parseCommand(String input) {
		if (input.indexOf('"') == -1) {
			return parseCommandNoQuotes(input);
		}

		// else we might have to parse between quotations
		// For example, the string below: 
		// question "how are you" 2
		// contains three tokens (question, how are you, 2)

		int curIndex = 0;
		int endIndex = input.length();

		// used to hold the arguments of the command
		Vector vector = new Vector(4);	// 4 args is a large command

		while (curIndex < endIndex) {
			int spaceIndex = input.indexOf(' ', curIndex);
			int quotationIndex = input.indexOf('"', curIndex);

			//System.out.println("spaceIndex: [" + spaceIndex +
			//	"] quotationIndex: [" + quotationIndex +
			//	"] curIndex: [" + curIndex + "]");

			String next = null;
			if (quotationIndex == -1) {
				Enumeration enum = parseCommandNoQuotes(
					input.substring(curIndex, endIndex));
				while (enum.hasMoreElements()) {
					vector.add(enum.nextElement());
				}
				break;
			} else if (spaceIndex != -1 && spaceIndex < quotationIndex) {
				next = input.substring(curIndex, spaceIndex);
				curIndex = spaceIndex+1;
			} else {	// quotationIndex != -1
				// we have to extract a string between quotations
				int matchingQuote =	input.indexOf('"', quotationIndex+1);
				if (matchingQuote == -1) {
					throw new CommandException(
						"Error: quotations do not match");
				}
				next = input.substring(quotationIndex+1, matchingQuote);

				// this assumes that an end quote will always be followed
				// by a space
				curIndex = matchingQuote+2;
			}
			vector.add(next);
		}
		return vector.elements();
	}

	Command getCommand(String name) {
		if (commands_.containsKey(name)) {
			return (Command)commands_.get(name);
		} else { return null; }
	}

	final void executeCommand(Enumeration enum) {
		if (!enum.hasMoreElements()) {
			throw new CommandException("Error: no command specified");
		}
		String name = (String)enum.nextElement();
		Command command = getCommand(name);
		if (command != null) {
			command.execute(enum);
		} else {
			throw new CommandException("Error: \"" + name + 
				"\" does not compute");
		}
	}
}
