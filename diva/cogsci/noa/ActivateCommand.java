// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.noa;

import util.cline.*;
import cogsci.Control;
import java.util.*;

/**
 * Command to activate a node within the network. The activation will spread
 * based on the number of levels specified to spread.
 */
public class ActivateCommand extends BaseCommand {

	public ActivateCommand() {
		super("activate", -1, "hub1 hub2 ... hubN" +
			"\n - activates each of the named hubs in the network and" +
			"\n returns each of the hubs that was activated in order" +
			"\n from highest to lowest");
	}

	public void execute(String args[]) {
		Vector hubs = new Vector(args.length);
		for (int i=0; i<args.length; i++) {
			String hubName = args[i];
			Hub next = NOA.get(hubName);
			if (next == null) {
				Control.error("hub named \"" + hubName + "\" does not exist");
				return;
			}
			hubs.add(next);
		}

		Iterator iterator = NOA.activate(hubs.iterator());

		if (iterator == null) { return; }
		
		String printOut = new String(
			"Most active hubs listed from highest to lowest:");
		String mostActive = "";
		for (int i=0; i<5 && iterator.hasNext(); i++) {
			Hub next = (Hub)iterator.next();
			// get the most active name to update the viewer
			if (i==0) { mostActive = next.getName(); }
			printOut += 
				"\n\t" + next.getName() + " (" + next.getActivation() + ")";
		}
		Control.viewMemory(mostActive);
		Control.status(printOut);
	}
}

