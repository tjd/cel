package cogsci.imagery;

// adopted from the VRML97 demo code

import javax.media.j3d.*;
import javax.vecmath.Color3f;
import java.util.*;

public class TreeCleaner {

    private static final int CLEAN_UNUSED = 1;
    private static final int CLEAN_NONE = 2;

    private static final int ALREADY_CLEANED = -1;

    static final boolean debug = false;

    /**
     * Clears pickable and collidable flags in Shape3Ds in the subgraph 
     * under the input node.  The pickable flag is set to false if there
     * are no groups above the Shape3D with ENABLE_PICK_REPORTING set. The
     * collidable flag on Shape3ds is always set to false.
     */
    static public void cleanSubgraph(javax.media.j3d.Node implNode)
		throws RestrictedAccessException {
        Hashtable sharedGroups = new Hashtable();

	checkAndClean(implNode, CLEAN_UNUSED, sharedGroups);

	// now go through the shared groups we found, since traversing
	// the sg's can find new sg's, we need to iterate
	int numGroupsCleaned = 0;
	Integer alreadyCleaned = new Integer(ALREADY_CLEANED);

	while (numGroupsCleaned < sharedGroups.size()) {
	    if (debug) {
		System.out.println("Cleaning shared groups: " + 
		 	numGroupsCleaned + " done out of " + 
			sharedGroups.size());
	    }
	    Enumeration e = sharedGroups.keys(); 
	    while (e.hasMoreElements()) {
		SharedGroup sg = (SharedGroup)e.nextElement();
		int sgFlag = ((Integer)sharedGroups.get(sg)).intValue();
		if (sgFlag != ALREADY_CLEANED) {
		    checkAndClean(sg, sgFlag, sharedGroups);
		    sharedGroups.put(sg, alreadyCleaned);
		    numGroupsCleaned++;
		}
	    }
	}
    }

    static void checkAndClean(javax.media.j3d.Node node, int pickingFlag,
	    Hashtable sharedGroups) throws RestrictedAccessException {
	if (node != null) {
	    if (node.isLive()) {
		throw new RestrictedAccessException(
			    "Can't clean a live scene graph");
	    } else {
		clean(node, pickingFlag, sharedGroups);
	    }
	}
    }

    static void clean(javax.media.j3d.Node node, int pickingFlag, 
	Hashtable sharedGroups) {
	
	if (node instanceof javax.media.j3d.Group) {
	    // if current flag is unused and this group is pickable, keep it's
	    // children pickable
	    if ((pickingFlag == CLEAN_UNUSED) &&
	        (node.getCapability(
				javax.media.j3d.Node.ENABLE_PICK_REPORTING))) {
		pickingFlag = CLEAN_NONE;
	    }
	    Enumeration e = ((javax.media.j3d.Group)node).getAllChildren();
	    while (e.hasMoreElements())  {
		clean((javax.media.j3d.Node)(e.nextElement()), pickingFlag, 
			sharedGroups);
	    }
	} else if (node instanceof javax.media.j3d.Link) {
	    javax.media.j3d.Link link = (javax.media.j3d.Link) node;
	    SharedGroup sg = link.getSharedGroup();
	    Integer value = (Integer)sharedGroups.get(sg);
	    // Set value if none set before or this value is more restrictive
	    if ((value == null) || (pickingFlag > value.intValue())) {
		value = new Integer(pickingFlag);
		sharedGroups.put(sg, new Integer(pickingFlag));
	    }
	} else if (node instanceof Shape3D) {
	    if (pickingFlag != CLEAN_NONE) {
		node.setPickable(false);
	    }
	    node.setCollidable(false);
	}
    }
}
