package cogsci.imagery;

import javax.media.j3d.BranchGroup;

/**
 * Used by the VisualBuffer (note that the Java3D code in the visual buffer 
 * class was adopted from the demo code in the VRML97 package).
 */
public class NullNode extends javax.media.j3d.BranchGroup {
    NullNode() {
        super();
	setCapability(BranchGroup.ALLOW_DETACH);
        // keep the debug utility happy
        setCapability(BranchGroup.ALLOW_CHILDREN_READ);
    }
}

