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
 * This command adds an object from one visual concepts to another. 
 * @see MindsEye.addObject(String, String)
 */
public class AddObjectCommand extends MindsEyeCommand {

	public AddObjectCommand(MindsEye mindsEye) {
		super(mindsEye, "addobj", 2, "objectToAdd scene" +
			"\n - adds the visual properties of one object to another");
	}

	public void execute(String args[]) {
		getMindsEye().addObject(args[0], args[1]);
	}
}

