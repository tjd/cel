// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.noa;

import util.cline.*;
import cogsci.Control;
import java.util.*;

/**
 * Command to activate a node within the network. The activation will spread
 * based on the number of levels specified to spread.
 */
public class ProblemSolveCommand extends BaseCommand {

	public ProblemSolveCommand() {
		super("psolve", 2, "hub1 hub2" +
			"\n - tries to find feasible pathways between the two hubs");
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

		Iterator iterator = NOA.problemSolve(hubs.iterator());

		if (iterator == null) { return; }
		
		String printOut = "****** solution pathways -> ";

		for(int i=0; iterator.hasNext(); i++) {
			Vector pathway = (Vector)iterator.next();
			if (pathway == null) { continue; }
			printOut += "\n[" + (i+1) + "]: ";
			Iterator spokes = pathway.iterator();
			while (spokes.hasNext()) {
				Spoke spoke = (Spoke)spokes.next();
				printOut += spoke.toString() + ", ";
			}
		}
		//Control.viewMemory(mostActive);
		Control.status(printOut);
	}
}

