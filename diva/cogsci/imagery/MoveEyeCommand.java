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
 * Command to move around the current viewpoint.
 */
public class MoveEyeCommand extends MindsEyeCommand {
	
	public MoveEyeCommand(MindsEye mindsEye) {
		super(mindsEye, "moveye", 6, 
			"eye_x eye_y eye_z lookat_x lookat_y lookat_z" +
			"\n - moves the minds eye to a new location specified by the " +
			"\n   eye co-ordinates and facing the lookat co-ordinates");
	}

	public void execute(String args[]) {
		try {
			float eye_x = Float.parseFloat(args[0]);
			float eye_y = Float.parseFloat(args[1]);
			float eye_z = Float.parseFloat(args[2]);
			float loc_x = Float.parseFloat(args[3]);
			float loc_y = Float.parseFloat(args[4]);
			float loc_z = Float.parseFloat(args[5]);
			getMindsEye().updateView(eye_x, eye_y, eye_z, loc_x, loc_y, loc_z);
		} catch (NumberFormatException e) {
			Control.error("number format exception: " + e);
		}
		return;
	}
}

