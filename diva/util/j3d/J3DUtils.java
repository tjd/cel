package util.j3d;

import java.util.*;
import javax.media.j3d.*;
import javax.vecmath.*;
import com.sun.j3d.utils.geometry.*;

/**
 * Miscellany of Java3D utility methods developed for the DIVA model.
 */
public class J3DUtils {
	
	/**
	 * This method counts every leaf node that exists below the given node.
	 * This method calls itself recursively to descend all the way down the 
	 * graph. If node is itself a leaf, then this method returns 1.
	 */
	public static int countLeafs(Node node) {
		int count = 0;
		if (node instanceof Group) {
			Group parent = (Group)node;
			for (int i=0; i<parent.numChildren(); i++) {
				Node child = parent.getChild(i);
				count += countLeafs(child);
			}
		} else {	// this is a leaf
			count = 1;
		}
		return count;
	}

	/**
	 * This method measures the depth of a graph below the given node. 
	 * If the given node is a leaf, then 0 is returned.
	 */
	public static int measureDepth(Node node) {
		if (node instanceof Group) {
			Group parent = (Group)node;
			int[] depths = new int[parent.numChildren()];
			for (int i=0; i<parent.numChildren(); i++) {
				Node child = parent.getChild(i);
				depths[i] = measureDepth(child) + 1;
			}
			return getLargest(depths);
		} 
		return 1;
	}

	public static int getLargest(int[] array) {
		int max=0;
		for (int i=0; i<array.length; i++) {
			if (array[i] > max) {
				max = array[i];
			}
		}
		return max;
	}

	public static double getLargest(double[] array) {
		double max=0.0;
		for (int i=0; i<array.length; i++) {
			if (array[i] > max) {
				max = array[i];
			}
		}
		return max;
	}

	public static double getLargest(Tuple3d tuple3d) {
		double[] array = new double[3];
		tuple3d.get(array);
		return getLargest(array);
	}
	
	/**
	 * Given a Bounds object, this method determines the center of the 
	 * bounding object and returns the radius.  A Point3d is passed into
	 * the method to be set with the center co-ordinates.
	 */
	public static double getRadiusAndCenter(Node node, Point3d center) {
		Bounds bounds = node.getBounds();
		double radius = 0.0;
		if (bounds instanceof BoundingSphere) {
			BoundingSphere boundSph = (BoundingSphere)bounds;
			boundSph.getCenter(center);
			radius = boundSph.getRadius();
		} else if (bounds instanceof BoundingBox) {
			BoundingBox boundBox = (BoundingBox)bounds;
			Point3d lower = new Point3d();
			boundBox.getLower(lower);
			Point3d upper = new Point3d();
			boundBox.getUpper(upper);
			center.x = (lower.x + upper.x) / 2.0;
			center.y = (lower.y + upper.y) / 2.0;
			center.z = (lower.z + upper.z) / 2.0;
			double radiusx = Math.abs(upper.x - center.x);
			double radiusy = Math.abs(upper.y - center.y);
			double radiusz = Math.abs(upper.z - center.z);
			double[] radi = {radiusx, radiusy, radiusz};
			radius = getLargest(radi);
		} else {
			System.err.println("J3DUtils: unknown bounding type");
		}
		return radius;
	}

	public static double getRadius(Node node) {
		Point3d center = new Point3d();
		return getRadiusAndCenter(node, center);
	}

	public static void setCapabilities(Node node) {
		if (node instanceof BranchGroup) {
			setBranchGroupCapabilities((BranchGroup)node);
		} else if (node instanceof TransformGroup) {
			setTransformGroupCapabilities((TransformGroup)node);
		} else if (node instanceof Primitive) {
			setPrimitiveGroupCapabilities((Primitive)node);
		} else if (node instanceof Group) {
			setGroupCapabilities((Group)node);
		} else {
			setNodeCapabilities(node);
		}
	}
	
	/**
	 * Another utility method that given a node, returns the first 
	 * TransformGroup at or below the node. If the node itself is a 
	 * TransformGroup then that node is returned, if the node is a 
	 * BranchGroup then the first TransformGroup that is a child will be 
	 * returned. If no TransformGroup is found in the given node or its 
	 * children, then a new BranchGroup and TransformGroup are allocated
	 * and inserted above the current node. The new TransformGroup is 
	 * then returned.
	 */
	public static TransformGroup findTransformGroup(Node node) {
		TransformGroup tgroup = null;

		if (node instanceof BranchGroup) {
			BranchGroup root = (BranchGroup)node;
			Enumeration children = root.getAllChildren();
			while (children.hasMoreElements()) {
				Node next = (Node)children.nextElement();
				if (next instanceof TransformGroup) {
					tgroup = (TransformGroup)next;
					break;
				}
			}
		} else if (node instanceof TransformGroup) {
			tgroup = (TransformGroup)node;
		} else {
			BranchGroup group = new BranchGroup();
			setBranchGroupCapabilities(group);

			Group parent = (Group)node.getParent();
			for (int i=0; i<parent.numChildren(); i++) {
				Node child = parent.getChild(i);
				if (child == node) {
					parent.setChild(group, i);
					break;
				}
			}

			tgroup = new TransformGroup(); 
			setTransformGroupCapabilities(tgroup);
			group.addChild(tgroup);
			tgroup.addChild(node);
		}
		return tgroup;
	}

	public static void setBranchGroupCapabilities(BranchGroup group) {
		
		group.setCapability(BranchGroup.ALLOW_DETACH);
		setGroupCapabilities(group);
	}
	
	public static void setTransformGroupCapabilities(TransformGroup group) {
		
		group.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
		group.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
		setGroupCapabilities(group);
	}

	public static void setPrimitiveGroupCapabilities(Primitive primitive) {
		primitive.setCapability(Primitive.ENABLE_APPEARANCE_MODIFY);
		setAppearanceCapabilities(primitive.getAppearance());
		setGroupCapabilities(primitive);
	}

	public static void setGroupCapabilities(Group group) {
		
		// Group capability bits
		group.setCapability(Group.ALLOW_CHILDREN_READ);
		group.setCapability(Group.ALLOW_CHILDREN_WRITE);
		group.setCapability(Group.ALLOW_CHILDREN_EXTEND);
		group.setCapability(Group.ALLOW_COLLISION_BOUNDS_READ);
		
		setNodeCapabilities(group);
		
		// now handle all the children of the group
		Enumeration enum = group.getAllChildren();
		while (enum.hasMoreElements()) {
			Node node = (Node)enum.nextElement();
			if (node != null) {
				setCapabilities(node);
			}
		}

	}

	public static void setAppearanceCapabilities(Appearance appearance) {
		if (appearance == null) {
			return;
		}
		appearance.setCapability(Appearance.ALLOW_COLORING_ATTRIBUTES_READ);
		appearance.setCapability(Appearance.ALLOW_LINE_ATTRIBUTES_READ);
		appearance.setCapability(Appearance.ALLOW_MATERIAL_READ);
		appearance.setCapability(Appearance.ALLOW_TEXTURE_READ);
		setMaterialCapabilities(appearance.getMaterial());
	}

	public static void setMaterialCapabilities(Material material) {
		// some Appearance NodeComponents will have null materials
		if (material == null) {
			return;
		}
		material.setCapability(Material.ALLOW_COMPONENT_READ);
	}

	public static void setNodeCapabilities(Node node) {
		node.setCapability(Node.ALLOW_BOUNDS_READ);
		node.setCapability(Node.ALLOW_PICKABLE_READ);
		node.setCapability(Node.ALLOW_AUTO_COMPUTE_BOUNDS_READ);
		node.setCapability(Node.ALLOW_LOCAL_TO_VWORLD_READ);
		if (node instanceof Leaf) {
			setLeafCapabilities((Leaf)node);
		}
	}

	public static void setLeafCapabilities(Leaf leaf) {
		if (leaf instanceof Shape3D) {
			setShape3DCapabilities((Shape3D)leaf);
		} 
		// setBehaviourCapabilities((Behaviour)leaf);
	}

	public static void setShape3DCapabilities(Shape3D shape) {
		shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
		shape.setCapability(Shape3D.ALLOW_GEOMETRY_READ);
		shape.setCapability(Shape3D.ALLOW_COLLISION_BOUNDS_READ);
		setAppearanceCapabilities(shape.getAppearance());
		Enumeration e = shape.getAllGeometries();
		while (e.hasMoreElements()) {
			setGeometryCapabilities((Geometry)e.nextElement());
		}
	}

	public static void setGeometryCapabilities(Geometry geometry) {
		if (geometry instanceof GeometryArray) {
			geometry.setCapability(GeometryArray.ALLOW_COLOR_READ);
			geometry.setCapability(GeometryArray.ALLOW_COUNT_READ);
			geometry.setCapability(GeometryArray.ALLOW_COORDINATE_READ);
			geometry.setCapability(GeometryArray.ALLOW_FORMAT_READ);
			geometry.setCapability(GeometryArray.ALLOW_NORMAL_READ);
			geometry.setCapability(GeometryArray.ALLOW_REF_DATA_READ);
			geometry.setCapability(GeometryArray.ALLOW_TEXCOORD_READ);
		}
	}
}
