// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.noa;

import util.cline.BaseCommand;
import cogsci.Control;
import java.util.Enumeration;
import java.util.Hashtable;

/**
 */
public class ListHubsCommand extends BaseCommand {

	public ListHubsCommand() {
		super("listhubs", 0, 
			"\n - prints a list of all the named hubs in memory");
	}

	public void execute(String args[]) {
		Hashtable table = NOA.getHubs();
		Enumeration keys = table.keys();
		String list = "{";
		list += (String)keys.nextElement();
		while(keys.hasMoreElements()) {
			list += ", " + (String)keys.nextElement(); 
		}
		list += "}";
		Control.status("Named hubs in memory are:\n" + list);
	}
}
