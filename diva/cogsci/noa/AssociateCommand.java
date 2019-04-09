// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.noa;

import util.cline.*;
import cogsci.Control;
import java.util.Enumeration;
import java.util.Hashtable;

/**
 * This command associates hubs together. <P>
 * For example: "associate dog is animal"
 */
public class AssociateCommand extends BaseCommand {

	public AssociateCommand() {
		super("associate", 3, "Hub1 AssociationHub Hub2" +
			"\n - creates an association within the NOA long term memory" +
			"\n   For example: \"associate dog is animal\"");
	}

	public void execute(String args[]) {
		Hub hubs[] = new Hub[args.length];
		for (int i=0; i<args.length; i++) {
			String name = args[i];
			
			Hub hub = NOA.get(name);
			if (hub == null) {
				hub = new Hub(name);
			}
			hubs[i] = hub;
		}

		// now associate the hubs as indicated
		hubs[0].associate(hubs[1], hubs[2]);
	}
}

