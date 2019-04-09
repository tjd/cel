// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.noa;

import cogsci.Control;
import util.debug.Debug;
import util.Queue;
import java.util.*;

/**
 * The Hub class stores the spokes between hubs in the network.
 * (This is a network of associations). 
 * The 'meaning' of something represented by a Hub is defined 
 * solely by the connections to other Hubs.
 */
public class Hub implements Comparable, java.io.Serializable {
	
	private String name_;	// if this Hub (node) has a name, can be null
	private Spoke spokes_[];	// spokes (links) for this hub

	protected int count_ = 0;	// number of spokes (links) with this hub
	protected int size_ = 10;	// initial size of a Hub
	protected int activation_ = 0;	// activation level
	protected Hub activationOrigin_ = null;

	// size for the hub to grow by when more connections required
	protected final int increment_ = 50;

	/**
	 * Create a Hub with no name.
	 */
	public Hub() {
		name_ = NOA.registerNewName();
		NOA.registerHub(this);
		set();
	}
	
	/**
	 * Create a named Hub. This should be done by the NOA.register() 
	 * method to ensure that the name is referenced in the network
	 * dictionary.
	 */
	public Hub(String name) {
		// check with NOA about the name
		name_ = NOA.registerName(name);
		NOA.registerHub(this);
		set();
		
		// if the name already exits, associate this hub with the 
		// original hub having that name, name_ will have been set 
		// to name + "_" + a unique integer.
		// Note: equalsIgnoreCase() is important here since registerName()
		// returns a name with all lower case!
		if ( ! (name_.equalsIgnoreCase(name)) ) {
			associate(NOA.INSTANCEOF, NOA.get(name));
		}
	}

	private void set() {
		spokes_ = new Spoke[size_];
	}
	
	/** 
	 * If this hub is a blob type this method should behave virtual
	 * and call the Blob.decode() method. If this hub is not a blob,
	 * nothing will happen - good!
	 */
	public void decode() {}

	/**
	 * Returns a hash code for this Hub. <P>
	 *
	 * TODO: improve this hash code to use the associations as well 
	 * as the name of the Hub (currently just uses the hash of the name).
	 */
	public int hashCode() {
		if (name_ == null) {
			return 0;
		}
		return name_.hashCode();
	}
	
	/**
	 * This method is required for the java.lang.Comparable interface.
	 * This is used when sorting activation. NOTE this is backwards 
	 * because the sort algorithm sorts in ascending order.
	 */
	public int compareTo(Object o) {
		if ( ! (o instanceof Hub)) {
			throw new 
				ClassCastException("cannot compare hub to non-hub: " + o);
		}
		Hub other = (Hub)o;
		if (activation_ > other.activation_) {
			return -1;
		} else if (activation_ == other.activation_) {
			return 0;
		} else {
			return +1;
		}
	}

	private final void connect(Spoke spoke) {
		// check to see if we need to make more space
		if (count_ == size_) {
			int oldSize = size_;
			size_+=increment_;
			Spoke bigSpokes_[] = new Spoke[size_];
			for (int i=0; i<count_; i++) {
				bigSpokes_[i] = spokes_[i];
			}
			spokes_ = bigSpokes_;
		}
		spokes_[count_] = spoke;
		count_++;
	}
	
	/**
	 * Call this method to associate with another hub.
	 * For example, if this Hub represents the concept dog:
	 * <P><PRE>
	 * dog.associate(ISA, animalHub);
	 * </PRE><P>
	 * Will create the association "Dog is a kind of animal"
	 * within the network. 
	 */
	public final void associate(Hub association, Hub other) {
		String message = "Cannot associate with a null Hub";
		Debug.ASSERT(association, message);
		Debug.ASSERT(other, message);
		
		Spoke spoke = new Spoke(this, association, other);

		connect(spoke);
		other.connect(spoke);
	}
	
	/**
	 * Spreading activation method that is called recursively.
	 */
	final void activate(int levelsToActivate, 
						Hashtable activationTable,
						Hub origin,
						boolean problemSolving) {

		addCount(levelsToActivate, activationTable);
	
		// when we are problem solving
		if (problemSolving) {
			if (activationOrigin_ == null) {
				activationOrigin_ = origin;
			} else if (activationOrigin_ != origin) {
				//System.out.println("Found problem link: " + getName());
				activationTable.put("problem_link", this); 
			}
		}
		
		Hub next = null;

		for (int i=0; i<count_; i++) { 
			if (spokes_[i] != null) {
				next = getOpposite(spokes_[i]);
				if (next != origin && levelsToActivate >= 1) {
					next.activate(	levelsToActivate-1,
									activationTable,
									origin, problemSolving);
				}
			}
		}
	}
	
	private final void addCount(int weight, Hashtable activationTable) {
		activation_ += weight;
		//System.out.println(toString());
		if ( ! activationTable.containsKey(getName())) {
			activationTable.put(getName(), this);
		}
	}

	public final Spoke getSpoke(int index) {
		if (index > count_) {
			Control.error("spoke range [" + index + 
				"] is greater than hub size for '" + getName() + "'");
			return null;
		}
		return spokes_[index];
	}
	
	public final Hub getOpposite(Spoke spoke) {
		return spoke.a() == this  ? spoke.b() : spoke.a();
	}

	public final Hub getOpposite(int index) {
		return spokes_[index] == null ? null : getOpposite(spokes_[index]);
	}

	public Hub getAssociation(int index) {
		return spokes_[index] == null ? 
			null : spokes_[index].association();
	}

	public int getActivation() {
		return activation_;
	}

	public int decrementActivation() {
		if (activation_ > 0) {
			activation_ -= 1;
		} else {
			// we have reached zero - we need to clear all 
			// the information for the connected spokes
			for (int i=0; i<count_; i++) {
				Spoke spoke = spokes_[i];
				spoke.markPathTaken(false);
				spoke.markDeadEnd(false);
			}
		}
		return activation_;
	}

	public int getCount() {
		return count_;
	}
	
	/**
	 * @return the name of this hub.
	 */
	public String getName() {
		return name_;
	}	

	/**
	 * @return the name of this hub.
	 */
	public String toString() {
		return getName() + "-<" + activation_ + ">";
	}
}
