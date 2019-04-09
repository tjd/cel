package util.cline;

import java.util.*;

class HelpCommand extends BaseCommand {

	HelpCommand() {
		super("help", 1, "topic");
	}

	public void execute(String args[]) {
		String topic = args[0];
		Command command = getRegister().getCommand(topic);
		if (command == null) {
			prompt_.println("No help available for " + topic);
		} else {
			prompt_.println(command.getHelp());
		}
	}
	
	// overloads the BaseCommand help function
	public String getHelp() {
		String help = super.getHelp() + " [topic] " +
			"\n - prints out help for a specific topic";
		// add a list of the topics available
		help += "\n\tTopics available are: ";
		
		Set keySet = getRegister().getCommands().keySet();
		Iterator iterator = keySet.iterator();
		while (iterator.hasNext()) {
			String commandName = (String)iterator.next();
			help += "\n\t " + commandName;
		}
		return help;

	}
}
