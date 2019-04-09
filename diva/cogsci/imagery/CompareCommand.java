// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.imagery;

import util.cline.*;
import cogsci.Control;
import cogsci.noa.NOA;
import cogsci.noa.Hub;
import java.util.*;

import javax.media.j3d.*;
import javax.vecmath.Vector3f;

/**
 * Command to compare one visual concept with another. This is the basic
 * command for visual analogies. The top (default 5) mappings between any
 * two visual concepts are drawn to the visual buffer (left window) of the
 * user interface.
 *
 * @see MindsEye.compare(SceneBlob,SceneBlob,int,int)
 */
public class CompareCommand extends MindsEyeCommand {

	public CompareCommand(MindsEye mindsEye) {
		super(mindsEye, "compare", -1, 
			"scene1 scene2 numberMappingsToReturn(3) weightingScheme(1)" +
			"\n - compares the associative, static and dynamic content of " +
			"two visual concepts. The weighting scheme is either 0 for " +  
			"constant weights, or 1 (default) for differential weights");
	}

	public void execute(String args[]) {
		
		// Note - because this command takes a variable number of commands
		// we have to be extra careful to check that we are passed the 
		// correct number of arguments (normally this is handledby the 
		// BaseCommand execute() method however this is not the case when 
		// -1 arguments are specified in the constructor)
		if (args.length < 2) {
			prompt_.println(getHelp());
			return;
		}
			

		SceneBlob scene1 = getMindsEye().getSceneBlob(args[0]);
		SceneBlob scene2 = getMindsEye().getSceneBlob(args[1]);
		
		// default is 3 for the number of mappings to return
		int numberToReturn = 3;
		if (args.length > 2) {
			try {
				numberToReturn = Integer.parseInt(args[2]);
			} catch (NumberFormatException e) {
				Control.error("argument " + args[2] +
					" is an incorrect integer number format");
			}
		}
		
		// default is 1 for the constraint network weighting scheme
		int weightingScheme = 1;
		if (args.length > 3) {
			try {
				weightingScheme = Integer.parseInt(args[3]);
			} catch (NumberFormatException e) {
				Control.error("argument " + args[3] +
					" is an incorrect integer number format");
			}
		}

		if (scene1 == null) {
			Control.error("scene1 does not exist or is not visual");
			return;
		} else if (scene2 == null) {
			Control.error("scene2 does not exist or is not visual");
			return;
		} 
		getMindsEye().compare(scene1, scene2, numberToReturn, weightingScheme);
	}
}

