// DIVA (c) David Croft, 2000. University of Waterloo

package cogsci;

import util.cline.BaseCommand;
import cogsci.Control;
import java.util.Enumeration;

/**
 * Command to bring up a HubViewer with the specified hub in the center.
 */
public class ViewMemoryCommand extends BaseCommand {

	public ViewMemoryCommand() {
		super("viewmem", 1, "hubName" +
			"\n - views one of the hubs from long term memory" +
			"\n (if no viewer exists, one is created)");
	}

	public void execute(String args[]) {
		Control.viewMemory(args[0]);
	}
}

