// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.imagery;

import cogsci.Control;
import cogsci.noa.*;
import util.debug.Debug;

import java.util.Hashtable;
import java.util.Enumeration;
import java.util.Set;
import java.util.Iterator;

import javax.media.j3d.*;
import javax.vecmath.*;

import com.sun.j3d.utils.geometry.*;

// TODO
// complete or remove this class

/**
 * This class is currently not used in the DIVA model. The idea was to 
 * generate a code in two different ordering procedures (preorder and inorder)
 * that could be used to easily print-out a graph in text-format. I ended up
 * improving the graph drawer and using a number system to identify a node
 * within the graph. Using the numbering system a node (6,2), corresponds to
 * the node on level six of a graph (five levels down from the root node), and
 * the second node in from the left. 
 *
 * @see AnalogyEngine
 */
public class GraphCoder {

	private static final char BRANCH_GROUP_CODE			= 'a';
	private static final char TRANSFORM_GROUP_CODE		= 'b';
	private static final char SHARED_GROUP_CODE			= 'c';
	private static final char BOX_CODE					= 'd';
	private static final char SPHERE_CODE				= 'e';
	private static final char CYLINDER_CODE				= 'f';
	private static final char CONE_CODE					= 'g';
	private static final char UNKNOWN_GROUP_CODE		= 'h';
	private static final char BEHAVIOR_CODE				= 'i';
	private static final char VIEW_PLATFORM_CODE		= 'j';
	private static final char COLOR_CUBE_CODE			= 'k';
	private static final char TEXT2D_CODE				= 'l';
	private static final char UNKNOWN_LEAF_CODE			= 'm';
	private static final char UNKNOWN_NODE_CODE			= 'n';

	public static final String codeGraph(Node node) {
		StringBuffer code = new StringBuffer(10);
		codeSubGraph(node, code);
		System.out.println("encoded graph: " + code);
		return code.toString();
	}

	private static final void codeSubGraph(	Node node,
											StringBuffer code) {
		addCodeLetter(node, code);
		if (node instanceof Group) {
			Group parent = (Group)node;
			for (int i=0; i<parent.numChildren(); i++) {
				Node child = parent.getChild(i);
				codeSubGraph(child, code);
			}
		}
	}

	private static final void addCodeLetter(Node node, StringBuffer code) {
		char code_char = 'z';
		if (node instanceof Group) {
			if (node instanceof BranchGroup) {
				code_char = BRANCH_GROUP_CODE;
			} else if (node instanceof TransformGroup) {
				code_char = TRANSFORM_GROUP_CODE; 
			} else if (node instanceof SharedGroup) {
				code_char = SHARED_GROUP_CODE;
			} else if (node instanceof Primitive) {
				if (node instanceof Box) {
					code_char = BOX_CODE;
				} else if (node instanceof Cone) {
					code_char = CONE_CODE;
				} else if (node instanceof Cylinder) {
					code_char = CYLINDER_CODE;
				} else if (node instanceof Sphere) {
					code_char = SPHERE_CODE;
				}
			} else {
				code_char = UNKNOWN_GROUP_CODE;
			}
		} else if (node instanceof Leaf) {
			if (node instanceof Behavior) {
				code_char = BEHAVIOR_CODE;
			} else if (node instanceof ViewPlatform) {
				code_char = VIEW_PLATFORM_CODE;
			} else if (node instanceof Shape3D) {
				if (node instanceof ColorCube) {
					code_char = COLOR_CUBE_CODE;
				} else if (node instanceof Text2D) {
					code_char = TEXT2D_CODE;
				}
			} else {
				code_char = UNKNOWN_LEAF_CODE;
			}
		} 
		code.append(code_char);
		System.out.println("new code: " + code);
	}
}
