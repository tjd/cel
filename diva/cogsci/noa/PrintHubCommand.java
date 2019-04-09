// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.noa;

import util.cline.BaseCommand;
import util.Queue;
import cogsci.Control;
import java.util.Enumeration;
import java.util.Hashtable;

/**
 * This command prints out all the oneway associations defined for the 
 * specified hub.
 */
public class PrintHubCommand extends BaseCommand {

	public PrintHubCommand() {
		super("printhub", 1, "hubName" +
			"\n - prints out all the paths for the specified hub");
	}

	public void execute(String args[]) {
		String hubName = args[0];
		Hub hub = NOA.get(hubName);
		Control.status("This command has not been updated with the latest " +
			"NOA changes - sorry, can't print the associations!");

		/*
		if (hub != null) {
			Queue queue = new Queue();
			String printOut = new String();
			for (int i=0; i<hub.getCount(); i++) {
				Spoke spoke = hub.spokes_[i];
				if (spoke != null) {
					// this needs to be modified to use Spokes rather than
					// the old Connectors -
					// hub.followOutputPath(connector, queue);
					String path = hubName;
					while ( ! queue.empty()) {
						path += " " + ((Hub)queue.remove()).getName();
					}
					printOut += "\n" + path;
				}
			}
			Control.status("Printout of " + hubName + printOut);
		} else {
			Control.error(hubName + " does not exist in network");
			return;
		}
		*/
	}
}
