// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.noa;

import cogsci.Control;
import util.debug.Debug;
import java.io.*;
import java.util.*;

/**
 * This class controls the structure and contents of the network.<P>
 *
 * It maintains a dictionary of named hubs, and defines the basic types
 * of associations that are permited within the network. Eg. kindof, partof.
 */
public class NOA implements Serializable {

	private static NOA noa_ = null;	// at all times there should only be one 
									// active instance of a NOA - this is it

	private int numPersists_;	// number of times this object has been 
								// persisted
    String name_;

	// Primitive Hubs that can be used to form associations
	public static Hub IS;			// 
	public static Hub HAS;			// 
	public static Hub FROM;			//
	public static Hub INSTANCEOF;	// used for name collisions, eg.
									// their might be many instances of 
									// the 'dog' concept. They will be named
									// dog_1, dog_2 automatically by NOA

	public static Hub INPUT;		// when conecpts entered with a perceptual
									// module implementing NOAInput
	public static Hub TIME;			// time association

	private Hashtable hubDictionary_;
	private Hashtable nameCollisionSpace_;

	private int noNameCount_;		// used for naming hubs with no name
									// this way they can still be listed
									// in the dictionary

	// this is set to true any time the network is activated
	// the activation monitor will reset it false after the 
	// activation for each node fades out
	private boolean isActive_  = false;	
	private static ActivationMonitor monitor_ = null;

	static {
		monitor_ = new ActivationMonitor();
		monitor_.start();
	}

	/**
	 * Used to keep count of how many times a given name
	 * has been used to name a hub. This permits a unique name
	 * to always be generated in the registerName() method.
	 */
	private class NameCounter implements Serializable {
		int count_;
		NameCounter() {
			count_=0;
		}
		int increment() {
			return ++count_;
		}
	}
	
	/**
	 * Construct a network with a specified name.
	 */
	private NOA(String name) {
		name_ = name;
	}

	public static void initialize(String name) {
		noa_ = new NOA(name);
		noa_.set();
	}

	private void set() {
		numPersists_ = 0;

		hubDictionary_ = new Hashtable(100);
		nameCollisionSpace_ = new Hashtable(40);

		IS = 			new Hub("is");
		HAS = 			new Hub("has");
		FROM =			new Hub("from");
		INSTANCEOF = 	new Hub("instance of");
		INPUT =			new Hub("input");
		TIME = 			new Hub("time");
	}

	/**
	 * This method registers a new name for use in the NOA.
	 */
	static synchronized String registerName(String name) {
		if (name == null) { 
			return null;
		}
		name = name.toLowerCase();	// all names should be stored lower case
									// to avoid capital/lowercase problems !!

		if (noa_.hubDictionary_.containsKey(name)) {
			NameCounter curCount; 
			if (noa_.nameCollisionSpace_.containsKey(name)) {
				curCount = (NameCounter)noa_.nameCollisionSpace_.get(name);
			} else {
				curCount = noa_.new NameCounter();
				noa_.nameCollisionSpace_.put(name, curCount);
			}
			return name + "_" + curCount.increment();
		} else {
			return name;
		}
	}

	static synchronized String registerNewName() {
		String name = "no_name_" + noa_.noNameCount_;
		noa_.noNameCount_++;
		return name;
	}
	
	/**
	 * Registers the hub in the named hubs table.
	 * Must be called after registerName()!.
	 * @see the Hub constructor Hub(String name)
	 */
	static synchronized void registerHub(Hub hub) {
		String name = hub.getName();
		if (noa_.hubDictionary_.containsKey(name)) {
			// did you forget to register the name?
			Control.error("cannot register " + name + " hub. " +
				" Call registerName() first!");
		}
		noa_.hubDictionary_.put(name, hub);
	}

	/**
	 * This method adds a time association for the specified hub.
	 * This can be used when you want to have some temporal relationships
	 * stored within the network. It basically associates the hub with 
	 * the TIME hub and a new hub containing the current date and time.
	 */
	public static void markTime(Hub hub) {
		Calendar cal = Calendar.getInstance();
		String timeDescription = 
			cal.get(Calendar.YEAR) + "." + 
			cal.get(Calendar.MONTH) + "." +
			cal.get(Calendar.DAY_OF_MONTH) + "." +
			cal.get(Calendar.HOUR_OF_DAY) + "." +
			cal.get(Calendar.MINUTE);
		hub.associate(TIME, new Hub(timeDescription));
	}
	
	/**
	 * @return the hashtable containing the named hubs present in the
	 * network.
	 */
	public static Hashtable getHubs() {
		return noa_.hubDictionary_;
	}

	public static Hub get(String name) {
		name = name.toLowerCase();
		if (noa_.hubDictionary_.containsKey(name)) {
			return (Hub)noa_.hubDictionary_.get(name);
		}
		return null;
	}
	
	private void writeObject(ObjectOutputStream s)	throws IOException {    	
		numPersists_++;	// persisting, increment the records
		s.defaultWriteObject();    
	}
    
	private void readObject(ObjectInputStream s) 
		throws IOException, ClassNotFoundException {
			s.defaultReadObject();    
	}
	
	/**
	 * @return a string containing the name of this network and 
	 * the number of times it has been persisted.
	 */
	public String toString() {
		return name_ + " (" + numPersists_ + ")";
	}
	
	/**
	 * @return the name of this network.
	 */
	public String getName() {
		return name_;
	}
	
	/**
	 * Save this network to file. The network can then be reloaded at a 
	 * later time using the load(fileName) method.
	 */
	public static void save(String fileName) {
		try {
			System.out.println("Saving " + noa_ + " to file " + fileName);
			FileOutputStream fo = new FileOutputStream(fileName);
			ObjectOutputStream so = new ObjectOutputStream(fo);
			so.writeObject(noa_);		
			so.flush();	    
			fo.close();
		} catch (Exception e) {
			System.err.println("Error: Failed to save NOA - " + e);
		}	
	}
	
	/**
	 * Method to load a network from file.
	 */
	public static void load(String fileName) {
	    try {		
			long time = System.currentTimeMillis();
			FileInputStream fi = new FileInputStream(fileName);
			ObjectInputStream si = new ObjectInputStream(fi);  
			noa_ = (NOA)si.readObject();
			Debug.ASSERT(noa_);
			fi.close();
			time = System.currentTimeMillis() - time;
			System.out.println("Loaded " + noa_ + " from " + fileName + 
				" in " + time + " ms.");
		} catch (Exception e) {
			System.err.println("Error: Failed to load NOA - " + e);
			System.exit(1);
		}
	}    

	public synchronized static boolean isActive() {
		return noa_ == null ? false : noa_.isActive_;
	}

	public synchronized static void setActive(boolean active) {
		if (noa_ != null) {
			noa_.isActive_ = active;
		}
	}

	/**
	 * This method models the spreading activation within an associative 
	 * network. All of the associations with this node are added to a 
	 * vector, and then the activations spreads to the nodes that are 
	 * connected with this node, and the method is called recursively for 
	 * each of these nodes. Thus spreading activation through a large number
	 * of levels can take a long time.
	 */
	public static Iterator activate(Iterator hubs) {

		// TODO: figure out the rough size the table needs to be so 
		// that we can call the Hashtable(capacity, loadFactor) constructor.
		Hashtable activationTable = new Hashtable();
		
		while( hubs.hasNext() ) {
			Hub next = (Hub)hubs.next();
			next.activate(3, activationTable, next, false);
		}
	
		List list = new ArrayList(activationTable.values());
		try {
			Collections.sort(list);
		} catch (ClassCastException e) {
			Control.error("while sorting activation list: " + e);
		} catch (UnsupportedOperationException e) {
			Control.error("while sorting activation list: " + e);
		}
		setActive(true);
		return list.iterator(); 
	}
	
	public static Iterator problemSolve(Iterator hubs) {

		// TODO: figure out the rough size the table needs to be so 
		// that we can call the Hashtable(capacity, loadFactor) constructor.
		Hashtable activationTable = new Hashtable();

		// currently the algorithm works for only two origin nodes
		Hub origin1 = null;
		Hub origin2 = null;
		
		// get the first two hubs from the list
		// - I have made this generic so that one day I can extend the 
		// problems solving algorithm to work with more than two origins
		// if this might be useful
		while( hubs.hasNext() ) {
			Hub next = (Hub)hubs.next();
			if (origin1 == null) {
				origin1 = next;
			} else if (origin2 == null) {
				origin2 = next;
			}
			next.activate(4, activationTable, next, true);
		}
		
		// the problem link represents the first hub where the spreading
		// activation from the two origins meets . eg. 
		// if you have:
		// dog -> meat -> deer -> forest -> trees
		// and you problem solve between dog and trees, problem link
		// would probably be deer in this example, 
		Hub problemLink = (Hub)activationTable.get("problem_link");

		// if problemLink is null then there is no connection between
		// the concepts given the current network activation. It is
		// possible that with greater activation there might be a connection
		if (problemLink == null) {
			Control.status("no solution found");
			return null;
		} else if (origin1 == null || origin2 == null) {
			Control.status("require 2 origins to solve a problem: " +
				"origin1: " + origin1 + " origin2: " + origin2);
			return null;
		}
		
		// I am not sure if using a problem link is psychologically
		// plausible. So for now I am trying not to use it.
		// Also, if it is plausible, should it be activated to emphasize
		// the problem solution pathway?
		//problemLink.activate(3, activationTable, origin, false);

		Vector pathways = new Vector(5);
		boolean result = noa_.findPathways(pathways, origin1, origin2);
		Control.status("Searching for pathways between : " + origin1 + 
			" and " + origin2);

		if (result == false) {
			return null;
		}

		/*
		// we need to reverse the pathway from origin2 to the problemLink
		pathway2.trimToSize();
		Vector reversePathway2 = new Vector(pathway2.size());
		for (int i=(pathway2.size()-1); i>=0; i--) {
			reversePathway2.add(pathway2.elementAt(i));
		}
		
		// add the two pathways together
		pathway1.addAll(reversePathway2);
		*/
		
		return pathways.iterator(); 
	}

	private final boolean findPathways(	Vector pathways,
										Hub origin1, 
										Hub origin2) {

		if (pathways == null) {
			Control.error("room must be allocated for pathways");
			return false;
		}

		boolean result = true;

		int i=0;	

		outer_loop: for (; i<10; i++) {
			Vector pathway = new Vector(10);

			if ( findPathway(pathway, origin1, null, origin2, 0) ) {

				// the only way to check if this pathway is the same
				// as the last one we found is to check it here. If 
				// we found two pathways that are identical, then 
				// there are no more unique pathways to be found and 
				// we should stop the search. You cannot detect the 
				// same pathway anywhere else without a lot of work
				// You have to compare the most recent pathway with all
				// the pathways previously found!

				if (i > 0) {
					for (int j=0; j<i; j++) {
						Vector previousPath = (Vector)pathways.elementAt(j);
						boolean same = true;
						if (previousPath.size() == pathway.size()) {
							for (int k=0; k<pathway.size(); k++) {
								if ( ! 	previousPath.elementAt(k).equals(
										pathway.elementAt(k)) ) {
									same = false;
									break;
								}
							}
						} else {
							same = false;
						}
						if (same == true) {
							// break the search and return true
							result = true;
							break outer_loop;
						}
					}
				} // if we made it past the check this is a new unique path
				pathways.add(pathway);
				System.out.println("Found pathway " + (i+1));
			} else {
				break;
			}
		}

		Control.status("Found " + i + " pathways");
		if (i == 0) {
			result = false;
		} 

		// mark the network as active to make sure the ActivationMonitor
		// runs to clear activation, pathTaken and deadEnd
		setActive(true);
		return result;
	}
	
	private final boolean findPathway(	Vector path,
										Hub currentHub, 
										Spoke lastSpoke,
										Hub origin2, 
										int depth) {

		// because this method is called recursively ->
		// we can only search so far -> there may be no connection 
		// and we are just going around in circles
		if (depth >= 8) { 
			System.out.println("MAX DEPTH REACHED");
			return false; 
		}

		int newDepth = depth + 1;
		System.out.println("FINDING pathway from " + currentHub + 
			" DEPTH " + newDepth);
		
		Spoke spoke = null;

		for (int i=0; i<currentHub.getCount(); i++) {
			spoke = getNextSpoke(currentHub, lastSpoke, origin2, path, true);
			System.out.println("\tNext spoke: " + spoke);
			if (spoke == null) {
				// no spoke found, so try again using an existing path
				// calling this method with false should never return null
				// unless all the spokes from this current one have 
				// no activation except for the one that we came from
				// (or their is only one spoke - the one we came from)
				spoke = 
					getNextSpoke(currentHub, lastSpoke, origin2, path, false);

				if (spoke == null) {
					return false;
				}
			}

			Hub next = currentHub.getOpposite(spoke);
			System.out.println("\tFound next hub to search down: " + next);

			path.add(spoke);	// temporarily add this spoke to our 
								// path. If it is a dead end or we end
								// up taking a different route it will
								// be removed.

			if (next == origin2) {
				System.out.println(
					"\tPath complete, found " + origin2.getName() + 
					" link after " + depth + " hubs");
				break;
			
			// else start looking down this next hub for a route
			} else if ( findPathway(path, next, spoke, origin2, newDepth) ) {
				break;

			// no path found on that spoke, so move onto the next one
			} else {
				System.out.println("\tFrom hub " +
					next + ": spoke " + spoke + " is a dead end");
				// mark the last spoke we as a dead end
				path.remove(spoke);
				spoke.markDeadEnd(true);
				continue;
			}
		}
		
		// at this point we should have searched all the spokes from 
		// the current hub trying to find a new pathway. 
		if (spoke == null) {
			System.out.println("something funny!");
			return false;
		}
		
		// mark this spoke that if we are searching for more than
		// one pathway we know that this one has already been taken
		spoke.markPathTaken(true);
		
		return true;
	}

	private final Spoke getNextSpoke(	Hub current, 
										Spoke lastSpoke,
										Hub origin2,
										Vector path,
										boolean lookForNewPath) {
		Hub mostActive = null;
		Spoke theSpoke = null; 

		for (int i=0; i<current.getCount(); i++) {
			Spoke spoke = current.getSpoke(i);

			// if the head of this spoke is not the current hub, then
			// this spoke is coming into current from the opposite direction
			// see discussion of this in the final report for psych446
			// I finally decided that the direction is not important 
			// the algorithm should follow spokes in both directions
			// if (spoke.a() != current) { continue; }

			Hub next = current.getOpposite(spoke);
			int activation = next.getActivation();

			// if the spoke has already been taken, then try another spoke,
			// however, if this has no connection with the problem link,
			// (we won't know this until we search it out recursively)
			// then move onto the spoke we have already taken!

			if ( spoke == lastSpoke ) {	// don't go backwards!
				continue;
			} else if ( next == origin2 ) {
				return spoke;
			} else if ( activation == 0 ) {	// don't consider non-active spokes 
				continue;
			} else if ( spoke.deadEnd() ) {
				continue;
			} else if ( spoke.pathTaken() && lookForNewPath ) {
				continue;
			} else if ( wildGooseChase(next, path) ) {
				// we need to make sure we are not looping back to some
				// point we have already been on the path
				continue;
			} else if (	mostActive == null || 
						mostActive.getActivation() < activation ) {
				// this should cover the case when lookForNewPath is false
				// also
				mostActive = next;
				theSpoke = spoke;
			}
		}

		//	System.out.println("\tLast spoke: " + lastSpoke + 
		//		" <---------> Next spoke: " + theSpoke);
		return theSpoke;
	}
	
	// This method checks to see if the given path already goes through 
	// the given Hub (we have the hub in our path)
	private final boolean wildGooseChase(	Hub next,
											Vector path) {
		Iterator iterator = path.iterator();
		while (iterator.hasNext()) {
			Spoke spoke = (Spoke)iterator.next();
			if (spoke.a() == next || spoke.b() == next) {
				System.out.println("\tWILD GOOSE CHASE");
				return true;
			}
		}
		return false;
	}
}
