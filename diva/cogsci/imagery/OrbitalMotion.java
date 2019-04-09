// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.imagery;

import cogsci.Control;
import javax.media.j3d.*;
import javax.vecmath.*;
import java.util.*;

/**
 * Similar idea to the ConvergenceMotion class, however for an orbital motion 
 * (eg. atom, solar system). 
 * TODO - the algorithms to detect and compare orbital motion are not
 * complete.
 * @ see ConvergenceMotion
 */
class OrbitalMotion extends MotionPattern {

	OrbitalMotion(BranchGroup root, Point3d center, double scale) 
		throws MotionPatternException {
		super(root, center, scale, "Orbital Motion");
	}

	double compareBehaviors(	MotionPattern motionPatternTj, 
								Behavior si, Behavior tj,
								StringBuffer similarityTest) {
		double ratio = 0.0;

		if ( ! (motionPatternTj instanceof OrbitalMotion) ) {
			return ratio;
		}

		OrbitalMotion orbitalTj = (OrbitalMotion)motionPatternTj;
		
		// check that node si is in this pattern, and that node tj is in the 
		// other pattern
		if ( ! (containsBehavior(si) && orbitalTj.containsBehavior(tj))) {
			return ratio;
		}

		similarityTest.append("OrbitalMotion: ");
		return ratio;
	}

	boolean checkMotionPattern(Vector behaviors) {
		return false;
	}
}
