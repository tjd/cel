// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.imagery;

import cogsci.Control;
import javax.media.j3d.*;
import javax.vecmath.*;
import java.util.*;
import util.j3d.J3DUtils;

/**
 * This class is responsible for detecting and comparing covergence motions.
 */
class ConvergenceMotion extends MotionPattern {

	private final double CONVERGENCE_CENTER_ACTIVATION = 4.0;

	private Point3d convergenceCenter_;
	
	// This is a list of objects that are not part of the actual convergence
	// motion, but might be targets at which the "convergers" are
	// converging. These are important to consider in dynamic analogies.
	private Vector centerObjects_ = null;

	// Note, for convergence we need to keep track of the parent 
	// node of each converging behavior node. This is because 
	// contributingNodes_ is just the actual behavior nodes, however we need
	// to keep track of the entire subgraph representing a converging object.
	// The behavior that targets the subgraph is usually one level below 
	// a root node representing the entire object in motion.
	// The parents are used when determining the center objects that are 
	// being converged upon, (it allows us to eliminate the converging 
	// objects themselves from consideration).  
	private Vector contributingNodeRoots_;

	ConvergenceMotion(BranchGroup root, Point3d center, double scale) 
		throws MotionPatternException {
		super(root, center, scale, "Convergence Motion");

		// Note - contributingNodeRoots_ is required by the next method
		// determineObjectsAtConvergenceCenter - make sure we call it first
		contributingNodeRoots_ = determineConvergerRoots(root); 
		
		// Now try and determine the 'target objects' that are being 
		// converged upon
		centerObjects_ = determineObjectsAtConvergenceCenter(root);
	}

	Point3d getConvergenceCenter() {
		return convergenceCenter_;
	}

	Vector getCenterObjects() {
		return centerObjects_;
	}

	double compareWithCenterObjects(	ConvergenceMotion convergenceTj,
										Group si, Group tj,
										StringBuffer similarityTest) {

		if (convergenceTj == null) {
			return 0.0;
		}

		Vector sourceObjects = getCenterObjects();
		Vector targetObjects = convergenceTj.getCenterObjects();

		if ((sourceObjects != null && targetObjects != null) &&
			(sourceObjects.contains(si) && 
			 targetObjects.contains(tj))) {
				similarityTest.append("ConvergenceCenter(" +
					Control.format(CONVERGENCE_CENTER_ACTIVATION, 1) + 
					"): ");
					return CONVERGENCE_CENTER_ACTIVATION;
		}
		return 0.0;
	}

	double compareBehaviors(	MotionPattern motionPatternTj, 
								Behavior si, Behavior tj,
								StringBuffer similarityTest) {

		if (( motionPatternTj == null ) || 
			(! (motionPatternTj instanceof ConvergenceMotion)) ) {
			return 0.0;
		}

		double ratio = 0.0;
		ConvergenceMotion convergenceTj = (ConvergenceMotion)motionPatternTj;
		
		// check that node si is in this pattern, and that node tj is in the 
		// other pattern
		if ( ! (containsBehavior(si) && convergenceTj.containsBehavior(tj))) {
			return ratio;
		}

		similarityTest.append("ConvergenceMotion: ");

		if (si instanceof Interpolator && tj instanceof Interpolator) {
			Alpha alphaSi = ((Interpolator)si).getAlpha();
			Alpha alphaTj = ((Interpolator)tj).getAlpha();
			//System.out.println("AlphaSi: " + alphaSi);
			//System.out.println("AlphaTj: " + alphaTj);

			if (si instanceof PathInterpolator && 
				tj instanceof PathInterpolator) {

				int numElementsSi = 
					((PathInterpolator)si).getArrayLengths();
				int numElementsTj = 
					((PathInterpolator)tj).getArrayLengths();

				// getKnots(float[] knots);

				if (si instanceof PositionPathInterpolator &&
					tj instanceof PositionPathInterpolator) {

					PositionPathInterpolator pathSi =
						(PositionPathInterpolator)si;
					PositionPathInterpolator pathTj =
						(PositionPathInterpolator)tj;

					TransformGroup targetSi = pathSi.getTarget();
					TransformGroup targetTj = pathTj.getTarget();
					Transform3D axisSi = pathSi.getAxisOfTranslation();
					Transform3D axisTj = pathTj.getAxisOfTranslation();

					Point3f[] positionsSi = new Point3f[numElementsSi];
					for (int j=0; j<numElementsSi; j++) {
						positionsSi[j] = new Point3f();
					}
					pathSi.getPositions(positionsSi);

					Point3f[] positionsTj = new Point3f[numElementsTj];
					for (int j=0; j<numElementsTj; j++) {
						positionsTj[j] = new Point3f();
					}
					pathTj.getPositions(positionsTj);

					// first - compare the length of the two pathways

					double pathLengthSi = computeVector(positionsSi).length();
					double pathLengthTj = computeVector(positionsTj).length();

					double pathLengthRatio = getRelativeRatio(pathLengthSi,
						scale_, pathLengthTj, convergenceTj.scale_, false);

					ratio += pathLengthRatio;

					similarityTest.append("pathLength(" + 
						Control.format(pathLengthRatio, 3) + "), ");

					// second - compare the direction of the two pathways

					Vector3f dirVectorSi = computeDirVector(positionsSi);
					Vector3f dirVectorTj = computeDirVector(positionsTj);

					float angle = dirVectorSi.angle(dirVectorTj);
					
					// if the angle between the two vectors is small, then 
					// they are similar (1.0 if the angle is 0.0)
					double dirRatio = 1.0 - (((double)angle)/Math.PI);

					ratio += dirRatio;
					
					similarityTest.append("dirVector(" +
						Control.format(dirRatio, 3) + "), ");

				}
			}
		}
		return ratio;
	}

	boolean checkMotionPattern(Vector behaviors) {

		System.out.println("--- Checking for convergence motions ---");

		if (behaviors.isEmpty()) {
			return false;
		}

		System.out.println(
			"analyzing " + behaviors.size() + " behaviors");
		
		Vector points = new Vector(behaviors.size());
		Enumeration e = behaviors.elements();
		for (int i=0; e.hasMoreElements(); i++) {
			Behavior behave = (Behavior)e.nextElement();
			Point3f[] point = null;
			if (behave instanceof Interpolator) {
				Alpha alpha = ((Interpolator)behave).getAlpha();
				//System.out.println("Alpha: " + alpha);
				if (behave instanceof PathInterpolator) {
					int numElements = 
						((PathInterpolator)behave).getArrayLengths();
					// getKnots(float[] knots);
					if (behave instanceof PositionPathInterpolator) {
						PositionPathInterpolator path =
							(PositionPathInterpolator)behave;
						TransformGroup target = path.getTarget();
						Transform3D axis = path.getAxisOfTranslation();
						Point3f[] positions = new Point3f[numElements];
						for (int j=0; j<numElements; j++) {
							positions[j] = new Point3f();
						}
						path.getPositions(positions);
						point = positions;
					}
				}
			}
			points.add(point);
			
			/*
			String printout = "point " + i + ": ";
			if (point != null) {
				for (int j=0; j<point.length; j++) {
					printout += point[j].toString() + ", ";
				}
			} else {
				printout += "null";
			}
			System.out.println(printout);
			*/
		}

		// now look for a common point among the sets
		Point3f[] firstSet = null;
		int firstSetIndex=0;
		for ( ;firstSetIndex < points.size(); firstSetIndex++) {
			firstSet = (Point3f[])points.elementAt(firstSetIndex);
			if (firstSet != null) {
				break;
			}
		}

		if (firstSet == null) {
			System.out.println("while checking convergence - " +
				"no points sets found in behaviors");
			return false;
		}
	
		int numMatches = 0;

		// store the nodes that are converging in this vector
		Vector convergers = null;

		// now - starting with the first set of points, go through all
		// the other set of points and see how well they match

		for (int i=0; i<firstSet.length; i++) {
			Point3f ni = firstSet[i];
			int matches = 0;
			Vector convergeWith_ni = new Vector(points.size());
			convergeWith_ni.addElement(behaviors.elementAt(firstSetIndex));
			for (int j=firstSetIndex+1; j<points.size(); j++) {
				Point3f[] nextSet = (Point3f[])points.elementAt(j);
				if (nextSet == null) { 
					continue; 
				}
				for (int k=0; k<nextSet.length; k++) {
					Point3f nk = nextSet[k];
					if (areCloseTogether(ni, nk, (float)scale_, (float)0.01)) {
						matches++;
						convergeWith_ni.addElement(behaviors.elementAt(j));
						break; 	// some sets might have more than one matching
								// point - be sure to break out at first match
					}
				}
			}
			if (matches > numMatches) {
				numMatches = matches;
				// we have to convert from a Point3f to Point3d
				convergenceCenter_ = new Point3d(ni);
				convergers = convergeWith_ni;
			}
		}
		
		if (convergers != null) {
			System.out.println("Found " + convergers.size() + 
				" convergers around point: " + convergenceCenter_);
		} 
		
		// set these as the behavior nodes that contribute to the 
		// motion pattern
		contributingNodes_ = convergers;

		return true;
	}

	Vector determineConvergerRoots(BranchGroup root) {
		Vector behaviorParents = new Vector(contributingNodes_.size());
		searchForBehaviorParents(root, behaviorParents);
		return behaviorParents;
	}

	void searchForBehaviorParents(Group group, Vector parents) {
		Enumeration e = group.getAllChildren();
		while (e.hasMoreElements()) {
			Node next = (Node)e.nextElement();
			if (next instanceof Behavior &&
				contributingNodes_.contains(next)) {
				parents.add(group);
				break;
			} else if (next instanceof Group) {
				searchForBehaviorParents((Group)next, parents);
			}
		}
	}
		
	Vector determineObjectsAtConvergenceCenter(BranchGroup root) {
		if (convergenceCenter_ ==  null) {
			System.out.println("convergence center does not exist, " + 
								"no central objects detected");
			return null;
		}
		//System.out.println("Convergence center: " + convergenceCenter_ + 
		//	" Scale: " + scale_);
		Vector centerObjects = new Vector(3);
		searchForNodeAtConvergenceCenter(root, centerObjects);
		return centerObjects;
	}

	void searchForNodeAtConvergenceCenter(Node node, Vector centerObjects) {

		if (node instanceof Group && 
			contributingNodeRoots_.contains(node)) {
			//System.out.println("Node is a converger root: " + node);
			return;
		}
		
		Point3d nodeCenter = new Point3d();
		double nodeRadius = J3DUtils.getRadiusAndCenter(node, nodeCenter);

		//System.out.println("Node center: " + nodeCenter + 
		//	" Radius: " + nodeRadius);
		
		if (areCloseTogether(convergenceCenter_, nodeCenter, scale_, 0.15)) {

			//System.out.println("Are close together");

			// TODO - improve this. We should more intelligently determine 
			// when an object is in fact being converged upon. 
			if (nodeRadius < (scale_/4.0)) {
				System.out.println("Found object at convergence center: " + 
					node);
				centerObjects.add(node);

				// break out the search once we have found the highest node
				// in the tree (otherwise many of it's children might also
				// be classified as center objects - we only want the whole
				// object)
				return;
			}

		// Note - if this node is off-center with the convergence point then
		// we don't want to keep searching down this tree. The co-ordinate
		// system becomes relative and there will be many irrelevant matches
		// with the convergence center.
		} else if (convergenceCenter_.distance(nodeCenter) > nodeRadius) {
			return;
		}

		// only keep searching down the tree if this nodeCenter lines 
		// up or the bounds of the object overlap with the convergenceCenter_
		if (node instanceof Group) {
			Enumeration e = ((Group)node).getAllChildren();
			while (e.hasMoreElements()) {
				Node next = (Node)e.nextElement();
				searchForNodeAtConvergenceCenter(next, centerObjects);
			}
		}
	}
}
