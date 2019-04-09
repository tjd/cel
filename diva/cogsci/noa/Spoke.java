// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.noa;

import java.io.Serializable;

/**
 * This class models the one way connector between nodes in the network.<P>
 * When using the cogsci.noa API, most interfacing with the network 
 * should be done through the Hub and NOA classes, you should never
 * have to use this Spoke class directly.
 */
public class Spoke implements Serializable {
	private Hub a_;
	private Hub b_;
	private Hub association_;

	private boolean pathTaken_ = false;	// used for problem solving in the
										// semantic network when this path 
										// has already been travelled

	private boolean deadEnd_ = false;	// used for problem solving in the
										// semantic network when this path 
										// does not connect with solution

	Spoke(Hub a, Hub association, Hub b) {
		a_ = a;
		b_ = b;
		association_ = association;
	}

	final public Hub a() {
		return a_;
	}

	final public Hub b() {
		return b_;
	}

	final public Hub association() {
		return association_;
	}

	final void markPathTaken(boolean pathTaken) {
		pathTaken_ = pathTaken;
	}

	final boolean pathTaken() {
		return pathTaken_;
	}

	final void markDeadEnd(boolean deadEnd) {
		deadEnd_ = deadEnd;
	}

	final boolean deadEnd() {
		return deadEnd_;
	}

	public String getName() {
		return association_.getName();
	}

	public String toString() {
		return a_.getName() + " -> " + association_.getName() + " -> " +
			b_.getName();
	}
}
