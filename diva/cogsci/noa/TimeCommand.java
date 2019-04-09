// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.noa;

import util.cline.*;
import cogsci.Control;
import java.util.Enumeration;
import java.util.Date;

/**
 * Saves the current memory network to a file. Can then be reloaded later
 * with the loadMemory() command.
 */
public class TimeCommand extends BaseCommand {

	public TimeCommand() {
		super("time", 0,  
			"\n - prints out the current date and time");
	}

	public void execute(String args[]) {
		Control.status("Current time is: " + (new Date()).toString());
	}
}

