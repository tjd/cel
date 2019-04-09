package cogsci.imagery;

// adopted from the VRML97 demo code

import javax.media.j3d.*;
import java.io.PrintStream;
import java.util.Set;
import java.util.HashSet;
import java.util.Enumeration;
import java.util.Iterator;

public class TreePrinter {
    PrintStream printStream;
    String j3dPkg = new String("javax.media.j3d.");
    String v97Pkg = new String("com.sun.j3d.loaders.vrml97.impl.");

    public void print(PrintStream s, Locale l) {
	printStream = s;
	HashSet sharedGroups = new HashSet();
	printTree(l, 0, sharedGroups);
	Iterator iterator = sharedGroups.iterator(); 
	while (iterator.hasNext()) {
	    SharedGroup sg = (SharedGroup) iterator.next();
	    print(s, sg);
	}
    }

    public void print(Locale l) {
	print(System.out, l);
    }

    private void printTree(Locale l, int graphDepth, Set sharedGroups) {
	printNode(l,0,sharedGroups);
	try {
	    Enumeration e = l.getAllBranchGraphs();
	    while (e.hasMoreElements()) {
		Object o = e.nextElement();	
		if ( o instanceof Locale ) 
		    printTree((Locale)o, graphDepth+1, sharedGroups);
		else if ( o instanceof SceneGraphObject )
		    printTree((SceneGraphObject)o, graphDepth+1, sharedGroups);
		else 
		    printStream.println(o+ " unknown and in tree");
	    }
	} catch (CapabilityNotSetException e) {
	    printStream.println("No capability to read children");
	}
    }

    public void print(PrintStream s, SceneGraphObject sgo) {
	printStream = s;
	HashSet sharedGroups = new HashSet();
	printTree(sgo, 0, sharedGroups);
	Iterator iterator = sharedGroups.iterator(); 
	while (iterator.hasNext()) {
	    SharedGroup sg = (SharedGroup) iterator.next();
	    print(s, sg);
	}
    }

    public void print(SceneGraphObject sgo) {
	print(System.out, sgo);
    }

    private void printTree(SceneGraphObject sgo, 
		int graphDepth, Set sharedGroups) {
	
	printNode(sgo, graphDepth, sharedGroups);
	if(sgo instanceof javax.media.j3d.Group) {
	    try {
		Enumeration e = ((javax.media.j3d.Group)sgo).getAllChildren();
		while (e.hasMoreElements())  {
		    printTree((SceneGraphObject)(e.nextElement()),graphDepth+1,
			sharedGroups);
		}
	   } catch (CapabilityNotSetException e) {
		// Can't read handled below
	   }
	} 
    }

    private String nodeString(Object o) {
	String objString = o.toString();
	if (objString.startsWith(j3dPkg)) {
	    objString = objString.substring(j3dPkg.length());
	}
	if (objString.startsWith(v97Pkg)) {
	    objString = objString.substring(v97Pkg.length());
	}
	return objString;
    }

    private void printNode(Object o, int indent, Set sharedGroups) {
	for(int i = 0; i<indent; i++) printStream.print(">");
	printStream.print(nodeString(o) + ": ");
	if (o instanceof SceneGraphObject) {
	    SceneGraphObject sgo = (SceneGraphObject) o;
	    int capBits = 0;
	    // TODO: how to make sure we always check all the valid bits?
	    for (int i = 0; i < 64; i++) {
		if (sgo.getCapability(i)) {
		    capBits |= 1 << i;
		}
	    }
	    printStream.print("capBits:Ox" + Integer.toHexString(capBits)); 
	    if (o instanceof javax.media.j3d.Group) {
		javax.media.j3d.Group g = (javax.media.j3d.Group)o;
		int numChildren = 0;
		try {
		    numChildren = g.numChildren();
		} catch (CapabilityNotSetException e) {
		    //anyone who is using treePrinter, is debugging, so it is
		    //alright to blindly allow read. you should first detach
		    //browser.curScene, print the tree, then add it back to 
		    //browser.locale when finished.
		    g.setCapability(javax.media.j3d.Group.ALLOW_CHILDREN_READ);
		    numChildren = g.numChildren();
		    //System.out.println("Can't read children on group");
		    //return;
		}
		printStream.print(" children:" + numChildren);
		if (o instanceof TransformGroup) {
		    Transform3D	transform = new Transform3D();
		    Transform3D	identity = new Transform3D();
		    TransformGroup t = (TransformGroup) o;
		    t.getTransform(transform);
		    // TODO: use getBestType() when implemented
		    if (transform.equals(identity)) {
			printStream.print(" xform:IDENTITY ");
		    } else {
			printStream.print(" xform:NON-IDENTITY ");
		    }
		}
	    } else if (o instanceof javax.media.j3d.Link) {
		javax.media.j3d.Link l = (javax.media.j3d.Link)o;
		SharedGroup sg = l.getSharedGroup();
		printStream.print(" sg:"+ nodeString(sg));
		sharedGroups.add(sg);
	    } else {
		printStream.print(": leaf");
	    }
	}
	printStream.println();
    }
}
