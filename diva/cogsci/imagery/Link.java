// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.imagery;

import cogsci.Control;
import javax.media.j3d.*;
import javax.vecmath.*;
import java.util.*;

/**
 * A link within the constraint network that is used for visual analogies.
 * @see AnalogyEngine
 */
class Link {

	Unit a_;
	Unit b_;
	double weight_;

	Link(Unit a, Unit b, double weight) {
		a_ = a;
		b_ = b;
		weight_ = weight;
	}
	
	public String toString() {
		return a_.getNameAndActivation() + " <-(" + weight_ + ")-> " +
			b_.getNameAndActivation();
	}

	public double getWeight() {
		return weight_;
	}

	public double getActivationOpposite(Unit unit) {
		if (unit.equals(a_)) {
			return b_.getActivation();
		} else if (unit.equals(b_)) {
			return a_.getActivation();
		} 
		Control.error("getActivationOpposite() called for unit not a " +
			"member of current link");
		return 0.0;
	}
}
