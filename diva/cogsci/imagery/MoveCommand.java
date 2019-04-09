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
 * Moves a visualconcept currently active in the MindsEye. Basically this
 * command modifies the translational component for a visual concept. 
 */
public class MoveCommand extends MindsEyeCommand {

	public MoveCommand(MindsEye mindsEye) {
		super(mindsEye, "move", 4, " objectName x-cord y-cord z-cord" +
			"\n - move the specified object to the new co-ordinate x,y,z " +
			" in the visual buffer");
	}

	public void execute(String args[]) {
		String objName = args[0];
		float coords[] = new float[3];
		for (int i=0; i<3; i++) {
			try {
				coords[i] = Float.parseFloat(args[i+1]);
			} catch (NumberFormatException e) {
				Control.error("argument " + args[i+1] +
					" is an incorrect float format");
			}
		}

		SceneBlob blob = getMindsEye().getSceneBlob(objName);
		Transform3D transform = blob.getTransform();
		Vector3f transVec = new Vector3f(coords);
		transform.setTranslation(transVec);
		blob.setTransform(transform);
	}
}
