// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.noa;

import util.cline.*;
import cogsci.Control;
import java.util.Enumeration;

/**
 * Saves the current memory network to a file. Can then be reloaded later
 * with the loadMemory() command.
 */
public class SaveMemoryCommand extends BaseCommand {

	public SaveMemoryCommand() {
		super("savemem", 1, "fileName" +
			"\n - saves the NOA long term memory to a file");
	}

	public void execute(String args[]) {
		NOA.save(args[0]);
	}
}

