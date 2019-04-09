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
 * Command to print out the bounding information for a visual concept. The
 * bounds information is basically where the object is centered and the
 * radius of the sphere required to enclose it.
 */
public class BoundsCommand extends MindsEyeCommand {

	public BoundsCommand(MindsEye mindsEye) {
		super(mindsEye, "bounds", 1, " objectName" +
			"\n - prints out the size and location information for the " +
			"specified visual concept");
	}

	public void execute(String args[]) {
		String objName = args[0];
		Node node = getMindsEye().getSceneBlob(objName).getNode();

		Bounds bounds = node.getBounds();

		String message = "Bounds for " + objName + ": " + bounds;
		if (bounds instanceof BoundingBox) {
			message += " {bounding box type)";
		} else if (bounds instanceof BoundingSphere) {
			message += " (bounding sphere type)";
		} else {
			message += " (bounding polytope type)";
		}
		Control.status(message);
	}
}

