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
 * Command to retrive the x,y,z translational component for a visual concept.
 */
public class LocateCommand extends MindsEyeCommand {

	public LocateCommand(MindsEye mindsEye) {
		super(mindsEye, "locate", 1, "objectName" +
			"\n - locate the x,y,z co-ordinates of the specified object");
	}

	public void execute(String args[]) {
		String objName = args[0];
		TransformGroup tgroup = getMindsEye().findTransformGroup(objName);
		if (tgroup != null) {
			Transform3D transform = new Transform3D();
			tgroup.getTransform(transform);
			Vector3f vector = new Vector3f();
			transform.get(vector);
			Control.status(objName + " is at location {" + vector.x +
											", " + vector.y + 
											", " + vector.z + "}");
		}
	}
}

