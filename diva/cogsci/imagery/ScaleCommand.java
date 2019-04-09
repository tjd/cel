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
 * Command to scale a visual concept. Like the move and rotate commands, 
 * this basically modifies the transform matrix for a visual concept.
 */
public class ScaleCommand extends MindsEyeCommand {

	public ScaleCommand(MindsEye mindsEye) {
		super(mindsEye, "scale", 2, "object factor" +
			"\n - scales the specified object");
	}

	public void execute(String args[]) {
		String objName = args[0];
		double scale = 1.0;
		try {
			scale = Double.parseDouble(args[1]);
		} catch (NumberFormatException e) {
			Control.error("scale factor not specified correctly");
		}

		SceneBlob blob = getMindsEye().getSceneBlob(objName);
		Transform3D transform = blob.getTransform();
		transform.setScale(scale);
		blob.setTransform(transform);
	}
}
