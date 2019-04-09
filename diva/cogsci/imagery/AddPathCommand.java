// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.imagery;

import util.cline.*;
import cogsci.Control;
import cogsci.noa.NOA;
import cogsci.noa.Hub;
import java.util.Enumeration;
import javax.media.j3d.*;
import javax.vecmath.Vector3f;
import javax.vecmath.Point3f;

/**
 * This command adds an interpolation pathway to a visual concept. 
 * (The result is that the object becomes animated, continually following
 * the pathway specified in the command). 
 * For example: <pre>
 * "addpath cow 0 0 0 5 0 0" would add an interpolator to the cow concept,
 * and animate the cow to move between 0 and 5 along the x-axis. </pre>
 * @see MindsEye.addPath(String, float[], Point3f[])
 */
public class AddPathCommand extends MindsEyeCommand {

	public AddPathCommand(MindsEye mindsEye) {
		super(mindsEye, "addpath", 7, "objectName x1 y1 z1 x2 y2 z2" +
			"\n - adds an interpolation path from point 1 to 2");
	}

	public void execute(String args[]) {
		try {
			float x1 = Float.parseFloat(args[1]);
			float y1 = Float.parseFloat(args[2]);
			float z1 = Float.parseFloat(args[3]);
			float x2 = Float.parseFloat(args[4]);
			float y2 = Float.parseFloat(args[5]);
			float z2 = Float.parseFloat(args[6]);

			Point3f start = new Point3f(x1, y1, z1);
			Point3f end = new Point3f(x2, y2, z2);

			float knots[] = {0.0f, 1.0f};
			Point3f positions[] = new Point3f[2];
			positions[0] = start;
			positions[1] = end;
			getMindsEye().addPath(args[0], knots, positions);
		} catch (NumberFormatException e) {
			Control.error("path values not specified correctly");
		}

	}
}

