// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.noa;

import util.cline.BaseCommand;
import cogsci.Control;
import java.util.Enumeration;
import java.util.Hashtable;

/**
 */
public class DecodeCommand extends BaseCommand {

	public DecodeCommand() {
		super("decode", 1, "hubName" +
			"\n - decodes a chunk of information stored at this hub");
	}

	public void execute(String args[]) {
		String name = args[0];
		Hub hub = NOA.get(name);
		if (hub != null) {
			hub.decode();
		} else {
			Control.status("Hub \"" + name + "\" does not exist");
		}
	}
}
