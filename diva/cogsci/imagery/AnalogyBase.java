// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.imagery;

import util.j3d.J3DUtils;
import cogsci.Control;

import javax.media.j3d.*;
import javax.vecmath.*;
import com.sun.j3d.utils.geometry.*;
import java.util.*;

/**
 * Base class for all analogy classes that generate ratios used in the 
 * ConstraintNetwork.
 */
class AnalogyBase {

	private final double BOTH_ARE_ZERO = 0.3;

	protected static final Point3f zeroPoint_ = new Point3f();

	/**
	 * This method returns a ratio for positive integer values.
	 * For example, 2 and 8 gives a ratio of 0.25.
	 */
	final double getRatio(int num1, int num2) {

		if (num1 < 0 || num2 < 0) {
			System.out.println("WARNING: getRatio() only works for " +
				"positive integer values");
		}

		//System.out.println("getRatio: " + num1 + ", " + num2);

		double ratio = ((double)num1)/((double)num2);

		// if we divided large by small, then invert the ratio
		if (ratio > 1.0) {
			ratio = 1.0/ratio;
		}
		
		//System.out.println("getRatio returns: " + ratio);

		return ratio;
	}
	
	/**
	 * This method requires that num1 and num2 translate into the same 
	 * -1,1 range when divided by their corresponding scales. The 
	 * method then shifts the ratios into a 0-1 basis where they are 
	 * compared. For example, getRelativeRatio(-2.0, 8.0, 1.0, 4.0) 
	 * would calculate -2.0/8.0 = -0.25 and 1.0/4.0 = 0.25. These are 
	 * 0.5 units apart. The total possible difference between two units
	 * is 2.0, so the ratio is (2.0-0.5)/2.0 = 0.75. 
	 * Note that if we are comparing numbers that are only in the positive 
	 * range (eg. colors), then the negativeNumbers parameter should be false 
	 * so that the range is constrained to one.
	 */
	final double getRelativeRatio(	double num1, double scale1,
									double num2, double scale2,
									boolean negativeNumbers) {

    if (scale1 < num1) {
      System.out.println("WARNING: scaleSi (" + scale1 + 
              ") less than valueSi(" + num1 + ")");
      scale1 = num1;
      System.out.println("Shifting scaleSi to: " + scale1);
    }
    if (scale2 < num2) {
      System.out.println("WARNING: scaleTj (" + scale2 + 
              ") less than valueTj(" + num2 + ")");
      scale2 = num2;
      System.out.println("Shifting scaleTj to: " + scale2);
     }

		double range = 1.0;
		if (negativeNumbers == true) {
			range = 2.0;

			// note - for many of the comparisons involving negative 
			// numbers (eg. co-ordinates), if one of the numbers 
			// is zero, return a 0.0 ratio. 
			if ( (num1 == 0.0 && num2 != 0.0) ||
				 (num1 != 0.0 && num2 == 0.0) ) {
				return 0.0;
			// another special case is when both numbers are 0, which 
			// occurs very frequently. We don't want to return a similarity
			// of 1.0, because 0 and 0 is not very interesting and is the 
			// default value for so many things
			} else if (num1 == 0.0 && num2 == 0.0) {
				return BOTH_ARE_ZERO;
			}
		}

		
		double num1b = num1/scale1;
		double num2b = num2/scale2;

		double difference = Math.abs(num1b-num2b);
		double ratio = (range-difference)/range;

		// square the ratio to increase the range of ratios
		ratio = ratio*ratio;

		//System.out.println("getRelativeRatio returns: " + ratio);

		return ratio;
	}
	
	final double compareTuples(	Tuple3d tupSi, double magnitudeSi,
								Tuple3d tupTj, double magnitudeTj,
								boolean negativeNumbers) {
		/*
		System.out.println("comparing tuples: ");
		System.out.println("magnitudes si: " + magnitudeSi + ", tj: " + 
			magnitudeTj);
		System.out.println("tuples: ");
		System.out.println(tupSi);
		System.out.println(tupTj);
		*/

		double ratiox = 
			getRelativeRatio(tupSi.x, magnitudeSi, tupTj.x, magnitudeTj, 
				negativeNumbers);
		double ratioy = 
			getRelativeRatio(tupSi.y, magnitudeSi, tupTj.y, magnitudeTj, 
				negativeNumbers);
		double ratioz = 
			getRelativeRatio(tupSi.z, magnitudeSi, tupTj.z, magnitudeTj, 
				negativeNumbers);
		double average = (ratiox+ratioy+ratioz)/3.0;

		//System.out.println("average: " + average);
		return average;
	}

	double getLargestValue(Point3d[] vertexArray) {
		double largest = 0.0;
		for (int i=0; i<vertexArray.length; i++) {
			Point3d next = (Point3d)vertexArray[i];
			double x = Math.abs(next.x);
			double y = Math.abs(next.y);
			double z = Math.abs(next.z);
			
			if (x>largest) {
				largest = x;
			}
			if (y>largest) {
				largest = y;
			} 
			if (z>largest) {
				largest = z;
			}
		}
		if (largest == 0.0) {
			System.out.println("WARNING: largest value in array is zero");
		}
		return largest;
	}

	final Vector getBehaviors(BranchGroup root) {
		Vector behaviors = new Vector(10);
		int graphDepth = 0;

		try {
			Class clss = Class.forName("javax.media.j3d.Behavior");
			extractObjectsFromSG(root, clss, behaviors, graphDepth);
		} catch (Exception e) {
			Control.error("Could not find behavior class: " + e);
		}
		return behaviors;
	}

	final void extractObjectsFromSG(	SceneGraphObject node,
										Class clss,
										Vector list,
										int graphDepth) {
		if (node instanceof Group) {
			try {
				Enumeration e = ((Group)node).getAllChildren();
				while (e.hasMoreElements()) {
					SceneGraphObject next = (SceneGraphObject)e.nextElement();
					extractObjectsFromSG(next, clss, list, graphDepth+1);
				}
			} catch (CapabilityNotSetException e) {
				Control.error(
					"capability not set exception while processing scene" + e);
			}
		} else if (clss.isInstance(node)) {
			list.add(node);
		}
	}

	public void getEulerAngles(Transform3D transform, Vector3d eulerAngles) {
		
		double mat[] = new double[16];
		transform.get(mat);
		double RADIANS = 180.0/Math.PI;

		// Note - the following steps to extract the Euler angles 
		// from a 4x4 matrix are adopted from the 
		// Matrix and Quaternion FAQ by hexapod@netcom.com (1997)

		double angle_x = 0.0;
		double angle_y = 0.0;
		double angle_z = 0.0;
		double tempx = 0.0;
		double tempy = 0.0;

		angle_y = -Math.asin( mat[2]);
		double C = Math.cos( angle_y );
		angle_y *= RADIANS;

		if ( Math.abs( angle_y ) > 0.0005 ) {
		  tempx = mat[10] / C;
		  tempy = -mat[6] / C;

		  angle_x  = Math.atan2( tempy, tempx ) * RADIANS;

		  tempx      =  mat[0] / C;
		  tempy      = -mat[1] / C;

		  angle_z  = Math.atan2( tempy, tempx ) * RADIANS;
		} else {
		  angle_x  = 0.0;

		  tempx      = mat[5];
		  tempy      = mat[4];

		  angle_z  = Math.atan2( tempy, tempx ) * RADIANS;
		}

		//System.out.println("angle x,y,z: " + angle_x + ", " + angle_y +
		//	", " + angle_z);

		//angle_x = clamp( angle_x, 0, 360 );
		//angle_y = clamp( angle_y, 0, 360 );
		//angle_z = clamp( angle_z, 0, 360 );
		eulerAngles.set(angle_x, angle_y, angle_z);

		// note - clamp does not seem to work well for negative angles
		// for example, given and angle such as -45.0, clamp sets this to 0.0
		eulerAngles.clamp(-360.0, 360.0);
		//System.out.println("eulerAngles: " + eulerAngles);
	}
}
