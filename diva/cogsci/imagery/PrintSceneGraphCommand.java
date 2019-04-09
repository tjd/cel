// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.imagery;

import util.cline.*;
import cogsci.Control;
import javax.media.j3d.*;

/**
 * Utility class to print a scene graph in a text format. Note that the
 * TreePrinter class is adopted from the VRML97 demo code.
 */
public class PrintSceneGraphCommand extends MindsEyeCommand {

	public PrintSceneGraphCommand(MindsEye mindsEye) {
		super(mindsEye, "psg", 0, 
			"\n - prints out the current scene graph to the dos window");
	}

	public synchronized void execute(String args[]) {
		BranchGroup root = getMindsEye().detachRoot();
		if (root != null) {
			TreePrinter tp = new TreePrinter();
			tp.print(root);
			Control.status("current scene graph printed to DOS window");
		} else {
			Control.status("current scene graph is null");
		}
		getMindsEye().reattachRoot();
	}
}

