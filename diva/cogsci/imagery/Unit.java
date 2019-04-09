// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.imagery;

import cogsci.Control;
import javax.media.j3d.*;
import javax.vecmath.*;
import java.util.*;

import java.text.NumberFormat;

/**
 * This is the base class for any class of unit used in the ACME network.
 */
public class Unit implements Comparable {
	
	String name_;
	String description_;
	Vector links_;

	boolean useGrossbergRule_ = true;
	
	double activation_;
	double newActivation_;	// used for parallel updating

	double MAX_ACTIVATION = 2.0;	// max activation for a unit
	double MIN_ACTIVATION = -2.0;	// min activation for a unit
	double DECAY_AMOUNT = 0.05;		// amount that unit's activation decays
									// at each time step
	double OUTPUT_THRESHOLD = -2.0;	// min. influence of a unit

	protected Unit(double activation, String name, String description) {
		activation_ = activation;
		newActivation_ = 0.0;
		name_ = name;
		description_ = description;
		links_ = new Vector(10);
	}
	
	// this interface is implemented for sorting the list of mapping units
	// note that Collections.sort(list) automatically sorts ascending,
	// so the list has to be reverse to sort descending
	public int compareTo(Object other) {
		Unit otheru = (Unit)other;
		if (activation_ > otheru.activation_) {
			return +1;
		} else if (activation_ == otheru.activation_) {
			return 0;
		} else {
			return -1;
		}

		// Note - we cannot do something simple like this:
		// return (int)(activation_ - otheru.activation_);
		// Most of the activations are between 0 and 1, and therefore the 
		// substractions will be 0.3, 0.5 etc.., which will be rounded off 
		// when cast to and int 
	}

	public final void add(Link link) {
		links_.add(link);
	}

	public final Enumeration elements() {
		return links_.elements();
	}

	public final double getActivation() {
		return activation_;
	}

	public void updateActivation(boolean grossberg) {
		if (grossberg = true) {
			updateActivationGrossberg();
		} else {
			updateActivationRumelhartMcClelland();
		}
	}

	private void updateActivationRumelhartMcClelland() {

		double netInput = getNetInput(false, false);
		double add1 = activation_*(1.0-DECAY_AMOUNT);
		double add2 = 0.0;
		if (netInput > 0.0) {
			add2 = netInput*(MAX_ACTIVATION-activation_);
		} else {
			add2 = netInput*(activation_-MIN_ACTIVATION);
		}
		double max = Math.max(MIN_ACTIVATION, add1+add2);
		newActivation_ = Math.min(MAX_ACTIVATION, max);
	}

	private void updateActivationGrossberg() {
		double netExcitation = getNetInput(true, true);
		double netInhibition = getNetInput(true, false);
		double add1 = activation_*(1.0-DECAY_AMOUNT);
		double add2 = (MAX_ACTIVATION-activation_)*netExcitation;
		double add3 = (activation_-MIN_ACTIVATION)*netInhibition;
		double max = Math.max(MIN_ACTIVATION, add1+add2+add3); 
		newActivation_ = Math.min(MAX_ACTIVATION, max);
	}
	
	/**
	 * getNetInput() is used to calculate the input coming into this node.
	 * If Grossberg's rule is being used, then the first parameter is 
	 * set to true, and the second parameter specifies whether to count
	 * just excitations, or if it is false, then only inhibitions will be 
	 * counted.
	 */
	private double getNetInput(boolean grossberg, boolean excitationOnly) {
		double result = 0.0;
		Enumeration e = links_.elements();
		while (e.hasMoreElements()) {
			Link next = (Link)e.nextElement();
			double weight = next.getWeight();

			// grossberg wants either just inhibitions or just excitations
			if (grossberg) {
				if ( (weight > 0.0) && ( ! excitationOnly) ) {
					continue;
				} else if ( (weight <= 0.0) && excitationOnly ) {
					continue;
				}
			}

			double incomingActivation = next.getActivationOpposite(this);
			double max = Math.max(OUTPUT_THRESHOLD, incomingActivation);
			result += max*weight;
		}
		return result;
	}

	public double setNewActivation() {
		double difference = Math.abs(newActivation_-activation_);
		activation_ = newActivation_;
		return difference;
	}

	public String getNameAndActivation() {
		String activationString = 
			"[" + Control.format(activation_, 2) + "]: ";
		return activationString + name_;
	}
	
	public String toString() {
		return getNameAndActivation() + ": " + description_;
	}
}
