// DIVA (c) David Croft, 2000. University of Waterloo

package cogsci;

import util.cline.BaseCommand;
import java.util.Enumeration;

/**
 * Command to exit the program.
 */
class ExitCommand extends BaseCommand {

	ExitCommand() {
		super("exit", 0,
			"\n - closes the prompt and forces the system to exit");
	}

	public void execute(String args[]) {
		Control.exit();
	}
}

