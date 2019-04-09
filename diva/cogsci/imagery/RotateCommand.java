// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.imagery;

import util.cline.*;
import cogsci.Control;
import cogsci.noa.NOA;
import cogsci.noa.Hub;
import java.util.Enumeration;
import javax.media.j3d.*;
import javax.vecmath.*;

/**
 * Modifies the rotational component for a visual concept.
 */
public class RotateCommand extends MindsEyeCommand {

	public RotateCommand(MindsEye mindsEye) {
		super(mindsEye, "rotate", 3, " objectName axis angle" +
			"\n - rotates the specified object about an axis {x,y,z}");
	}

	public void execute(String args[]) {
		String objName = args[0];
		String axis = args[1];
		double angle = 0;
		try {
			angle = Double.parseDouble(args[2]);
			// convert from degrees to radians
			angle = (2.0*3.1415*angle)/360.0; 
		} catch (NumberFormatException e) {
			Control.error("argument " + args[2] +
				" is an incorrect double number format");
		}

		SceneBlob blob = getMindsEye().getSceneBlob(objName);
		Transform3D transform = blob.getTransform();
		
		Matrix3d rotMatrix = new Matrix3d();
		if (axis.equals("x")) {
			rotMatrix.rotX(angle);
		} else if (axis.equals("y")) {
			rotMatrix.rotY(angle);
		} else if (axis.equals("z")) {
			rotMatrix.rotZ(angle);
		} else {
			Control.error("no such axis \"" +axis+ "\" for rotation");
			return;
		}
		transform.setRotation(rotMatrix);
		blob.setTransform(transform);
	}
}
