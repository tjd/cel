// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.imagery;

import cogsci.Control;
import javax.media.j3d.*;
import javax.vecmath.*;
import java.util.*;

/**
 * Base class for any motion pattern (eg. ConvergenceMotion, ParallelMotion,
 * OrbitalMotion).
 *
 */
abstract class MotionPattern extends AnalogyBase {
	
	protected BranchGroup root_;
	protected Vector contributingNodes_ = null;
	protected Point3d center_;
	protected double scale_;
	
	MotionPattern(	BranchGroup root, Point3d center, double scale, 
					String patternType) throws MotionPatternException {
		
		root_ = root;
		center_ = center;
		scale_ = scale;
		
		// check to see if this motion pattern exists
		// note that checkMotionPattern will be called for the sub-class
		// eg. ConvergenceMotion, ParallelMotion or OrbitalMotion

		if ( ! checkMotionPattern(getBehaviors(root)) ) {
			throw new MotionPatternException(patternType);
		}
	}

	abstract boolean checkMotionPattern(Vector behaviors); 

	Vector getContributingNodes() {
		return contributingNodes_;
	}

	boolean containsBehavior(Behavior node) {
		Vector nodes = getContributingNodes();

		if (nodes == null) { 
			return false; 
		}

		Enumeration e = nodes.elements();
		while (e.hasMoreElements()) {
			Behavior next = (Behavior)e.nextElement();
			if (node.equals(next)) {
				return true;
			}
		}

		// no match found
		return false;
	}

	abstract double compareBehaviors(	MotionPattern other, 
										Behavior si, Behavior tj,
										StringBuffer similarityTest);


	// TODO : improve this method to incorporate all of the positions
	// maybe it could be extended to fit a line in 3d space to the set of 
	// points. This would require some math though, for now I just compute
	// the direction vector between the first and last point in the array
	protected final Vector3f computeVector(Point3f[] positions) {

		
		// if there is only one point we can't calculate a vector!
		if (positions.length < 2) {
			return new Vector3f();
		}
		Point3f head = positions[0];
		Point3f tail = positions[1];
		
		Vector3f vector = new Vector3f( tail.x - head.x,
										tail.y - head.y,
										tail.z - head.z);
		return vector;
	}
		
	protected final Vector3f computeDirVector(Point3f[] positions) {
		Vector3f dirVector = computeVector(positions);
		dirVector.normalize();
		return dirVector;
	}

	protected final boolean areCloseTogether(	Point3f one, Point3f two,
												float scale,
												float precision) {
		float distance = one.distance(two);
		if ( Math.abs(distance) < scale*precision ) {
			return true;
		} 
		return false;
	}

	protected final boolean areCloseTogether(	Point3d one, Point3d two,
												double scale,
												double precision) {
		double distance = one.distance(two);
		if ( Math.abs(distance) < scale*precision) {
			return true;
		} 
		return false;
	}
}
