// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.imagery;

import cogsci.Control;
import javax.media.j3d.*;
import javax.vecmath.*;
import java.util.*;

/**
 * This class is responsible for the general motion analysis within a scene.
 * The basic idea is to detect any "high-level" patterns such as convergence,
 * orbital motion or parallel motion.
 */
class DynamicsAnalysis {

	private Vector patterns_ = new Vector(3);

	DynamicsAnalysis(BranchGroup root, Point3d center, double scale) {

		MotionPattern pattern = null;

		try {
			pattern = new OrbitalMotion(root, center, scale);
			patterns_.add(pattern);
		} catch (MotionPatternException e) {
			System.out.println(e);
		}

		try {
			pattern = new ConvergenceMotion(root, center, scale);
			patterns_.add(pattern);
		} catch (MotionPatternException e) {
			System.out.println(e);
		}

		try {
			pattern = new ParallelMotion(root, center, scale);
			patterns_.add(pattern);
		} catch (MotionPatternException e) {
			System.out.println(e);
		}
	}

	public ConvergenceMotion convergenceMotion() {
		Enumeration e = patterns_.elements();
		while (e.hasMoreElements()) {
			MotionPattern next = (MotionPattern)e.nextElement();
			if (next instanceof ConvergenceMotion) {
				return (ConvergenceMotion)next;
			}
		}
		return null;
	}

	public ParallelMotion parallelMotion() {
		Enumeration e = patterns_.elements();
		while (e.hasMoreElements()) {
			MotionPattern next = (MotionPattern)e.nextElement();
			if (next instanceof ParallelMotion) {
				return (ParallelMotion)next;
			}
		}
		return null;
	}

	public OrbitalMotion orbitalMotion() {
		Enumeration e = patterns_.elements();
		while (e.hasMoreElements()) {
			MotionPattern next = (MotionPattern)e.nextElement();
			if (next instanceof OrbitalMotion) {
				return (OrbitalMotion)next;
			}
		}
		return null;
	}

	public boolean containsMotionPattern() {
		if (patterns_.size() > 0) {
			return true;
		}
		return false;
	}
}
