// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.imagery;

import util.cline.*;
import cogsci.Control;
import cogsci.noa.NOA;
import cogsci.noa.Hub;
import java.util.Enumeration;
import javax.media.j3d.*;
import javax.vecmath.Vector3f;

/**
 */
public class StartCommand extends MindsEyeCommand {
	
	public StartCommand(MindsEye mindsEye) {
		super(mindsEye, "start", 0, 
			"\n - starts animations in the visual buffer");
	}

	public void execute(String args[]) {
		getMindsEye().start();
	}
}

