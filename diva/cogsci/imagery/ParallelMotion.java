// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.imagery;

import cogsci.Control;
import javax.media.j3d.*;
import javax.vecmath.*;
import java.util.*;

/**
 * Similar idea to the ConvergenceMotion class, however for parallel motion 
 * (eg. raindrops, two objects moving down a road). 
 * TODO - the algorithms to detect and compare parallel motion are not
 * complete.
 * @ see ConvergenceMotion
 */
class ParallelMotion extends MotionPattern {

	ParallelMotion(BranchGroup root, Point3d center, double scale) 
		throws MotionPatternException {
		super(root, center, scale, "Parallel Motion");
	}

	double compareBehaviors(	MotionPattern motionPatternTj, 
								Behavior si, Behavior tj,
								StringBuffer similarityTest) {
		double ratio = 0.0;

		if ( ! (motionPatternTj instanceof ParallelMotion) ) {
			return ratio;
		}

		ParallelMotion parallelTj = (ParallelMotion)motionPatternTj;
		
		// check that node si is in this pattern, and that node tj is in the 
		// other pattern
		if ( ! (containsBehavior(si) && parallelTj.containsBehavior(tj))) {
			return ratio;
		}

		similarityTest.append("ParallelMotion: ");
		return ratio;
	}

	boolean checkMotionPattern(Vector behaviors) {

		System.out.println("--- Checking for parallel motions ---");
		boolean parallelMotionFound = false;

		if (behaviors.isEmpty()) {
			return false;
		}
		System.out.println(
			"analyzing " + behaviors.size() + " behaviors");
		Vector3f dirVectors[] = new Vector3f[behaviors.size()];
		Enumeration e = behaviors.elements();
		for (int i=0; e.hasMoreElements(); i++) {
			Behavior behave = (Behavior)e.nextElement();
			if (behave instanceof Interpolator) {
				Alpha alpha = ((Interpolator)behave).getAlpha();
				//System.out.println("Alpha: " + alpha);
				if (behave instanceof PathInterpolator) {
					int numElements = 
						((PathInterpolator)behave).getArrayLengths();
					// getKnots(float[] knots);
					if (behave instanceof PositionPathInterpolator) {
						PositionPathInterpolator path =
							(PositionPathInterpolator)behave;
						TransformGroup target = path.getTarget();
						Transform3D axis = path.getAxisOfTranslation();
						Point3f[] positions = new Point3f[numElements];
						for (int j=0; j<numElements; j++) {
							positions[j] = new Point3f();
						}
						path.getPositions(positions);
						for (int j=0; j<numElements; j++) {
							dirVectors[i] = computeDirVector(positions);
						}
					}
				}
			}
			//System.out.println("dir vector " + i + ": " + dirVectors[i]);
		}

		// TODO
		// we need to fix up this algorithm to return a vector with matches
		return false;
	}
}
