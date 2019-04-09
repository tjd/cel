// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.imagery;

import util.j3d.J3DUtils;
import cogsci.Control;

import javax.media.j3d.*;
import javax.vecmath.*;
import com.sun.j3d.utils.geometry.*;
import java.util.*;

/**
 * This class contains the main algorithms for static and dynamic visual
 * analogies. Much of the dynamics analysis is done in the class by that 
 * name. The AnalogyEngine established a set of mappings between two scenes,
 * and then uses a constraint network to settle upon the most prominent
 * mappings. 
 *
 * @see DynamicsAnalysis
 * @see Unit
 * @see MappingUnit
 * @see Link
 */
class AnalogyEngine extends AnalogyBase {
	
	// hard-coded values used in the constraint network and to establish 
	// initial activation values for mappings between graph nodes

	private final double SPECIAL_LINK_WEIGHT = 0.03;
	private final double INHIBIT_LINK_WEIGHT = -0.02;
	private final double GRAPH_LINK_WEIGHT = 0.05;

	private final double ZERO_MATRIX_ACTIVATION = 0.3;
	private final double IDENTITY_MATRIX_ACTIVATION = 0.2;
	private final double IDENTICAL_MATRIX_ACTIVATION = 0.9;
	private final double BRANCH_GROUP_ACTIVATION = 0.2;
	private final double NEGATIVE_DETERMINANT_ACTIVATION = 0.1;

	
	// if a scene contains a motion pattern, then all of the static 
	// visual mappings will be scaled to give prominence to the dynamic
	// aspects of the scene. 
	private final double STATIC_WEIGHTING_FACTOR = 0.5;
	
	// when the compare command is called, the weighting scheme can be 
	// set to initialize the constraint network with differential constraint
	// weightings (weightingScheme_ will be set to non-zero value). When
	// zero is used, the constraint weightings are default values, and it 
	// is only the initial unit activations that are based on the 
	// mapping similarity ratios
	private int weightingScheme_ = 0;
	
	// if we add constraints between all the mappings that are established
	// between a node si and any potential nodes tj, it was found that 
	// too much negative inhibition spreads to all these nodes (they are
	// surrounded by inhibitory links. The current solution is that for each 
	// node si, only the top NUM_MAPPINGS_FOR_EACH_SI are considered.
	// See the method addConstraintsBetweenMappingsForSi()
	private final int NUM_MAPPINGS_FOR_EACH_SI = 3;

	private Vector links_;			// links for the constraint network
	private Vector mappings_;		// mappings for the constraint network
	private SpecialUnit special_;	// SpecialUnit - always active 
	
	// a number of identity and zero matrices/vectors are used to make 
	// make comparisons (eg. when comparing TransformGroup nodes)
	private Transform3D identityTransform3D_;
	private Matrix3d identityMatrix3d_;
	private Vector3d zeroVector3d_;

	double sourceRadius_ = 0.0;
	double targetRadius_ = 0.0;

	// before establishing mappings, each scene is analyzed for a high-level
	// motion pattern. If one exists, then behaviours should be considered
	// when establishing mappings. This variable will be set true if 
	// the dynamics analysis find a high-level pattern such as convergence.
	boolean compareBehaviors_ = false;
	DynamicsAnalysis sourceDynamics_ = null;
	DynamicsAnalysis targetDynamics_ = null;
	
	// a set of indexes are kept for the source and target scene graph.
	// each index is stored as a pair (depth, index) that specifies the 
	// depth of a node and it's index at that depth. For example, (3,4) 
	// specifies that a node is 3 levels deep (root is level 1) and that 
	// it is the fourth node across that level - note there might be a node
	// (3,2) that has a different parent from (3,4), it is however still at 
	// the same depth, and is the second unit in from the right of the graph.
	// The array index specifies the depth, and the integer value at that 
	// index stores the count of how many nodes there are at that depth level.
	int[] depthIndexesSi_;
	int[] depthIndexesTj_;

	AnalogyEngine() {

		mappings_ = new Vector(100);
		links_ = new Vector(200);
		special_ = new SpecialUnit();

		identityTransform3D_ = new Transform3D();	// defaults to identity
		identityMatrix3d_ = new Matrix3d();			// defaults all zero
		identityMatrix3d_.setIdentity();			// set to the identity
		zeroVector3d_ = new Vector3d();				// defaults all zero
	}

	private void initializeGraphIndexes(	BranchGroup root1,
											BranchGroup root2) {

		depthIndexesSi_ = new int[J3DUtils.measureDepth(root1)];
		for (int i=0; i<depthIndexesSi_.length; i++) {
			depthIndexesSi_[i]=0;
		}
		depthIndexesTj_ = new int[J3DUtils.measureDepth(root2)];
		for (int i=0; i<depthIndexesTj_.length; i++) {
			depthIndexesTj_[i]=0;
		}
	}
	
	/**
	 * The weighting scheme is either 0 for constant constraint weightings,
	 * (and differential initial unit activations) or 1 for differential
	 * constraint weightings and initial activations.
	 */
	final Vector compare(	BranchGroup sourceRoot, 
							BranchGroup targetRoot,
							int numberMappingsToReturn,
							int weightingScheme) {

		mappings_.clear();
		links_.clear();
		special_ = new SpecialUnit();	// special might have links to
										// units from a previous comparison
		initializeGraphIndexes(sourceRoot, targetRoot);
		weightingScheme_ = weightingScheme;
		
		Point3d center1 = new Point3d();
		Point3d center2 = new Point3d();
		sourceRadius_ = J3DUtils.getRadiusAndCenter(sourceRoot, center1);
		targetRadius_ = J3DUtils.getRadiusAndCenter(targetRoot, center2);
		
		// Step 1 - see if there is any high-level motion patterns in 
		// the two scenes. If there is a motion pattern, then we will 
		// consider behaviour node mappings in Step 2.

		sourceDynamics_ = new DynamicsAnalysis(	sourceRoot, 
												center1, sourceRadius_);

		targetDynamics_ = new DynamicsAnalysis(	targetRoot, 
												center2, targetRadius_);

		if (sourceDynamics_.containsMotionPattern() && 
			targetDynamics_.containsMotionPattern() ) {
			System.out.println(
				"Motion pattern detected, behavior nodes will be compared");
			compareBehaviors_ = true;
		} else {
			compareBehaviors_ = false;
		}

		// Step 2 - for each node Si in the source scene graph, 
		// see if there is a similar node in the target scene graph

		// if a node is found in targetRoot that is similar to the BranchGroup
		// sourceRoot, then a mapping unit will be established sourceRoot=Tj, 
		// and a constraint will be established. The method calls itself 
		// recursively to go through each node in the source graph (and for 
		// each node in the source graph, the target graph is searched for 
		// potentially mapping nodes). Note that if a node si maps with 
		// more than one tj, the method addConstraintsBetweenMappingsForSi()
		// is called to make inhibitory links between these nodes (so that 
		// only one mapping for each si will be accepted)

		searchGraphsForMappings(	0, sourceRoot, targetRoot, 
									null, sourceRadius_);

		// print out the mappings and constraints that were established

		System.out.println(mappings_.size() + " mappings established: ");
		Enumeration e = mappings_.elements();
		for (int i=1; e.hasMoreElements(); i++) {
			MappingUnit next = (MappingUnit)e.nextElement();
			System.out.println(i + "\t" + next);
		}
		
		System.out.println(links_.size() + " constraints established: ");
		e = links_.elements();
		for (int i=1; e.hasMoreElements(); i++) {
			Link next = (Link)e.nextElement();
			System.out.println(i + "\t" + next);
		}
		
		// Step 3 - now that we have established all the mapping units and 
		// established constraints between them, settle the network. 

		settleConstraintNetwork();

		// return a vector containing the top mappings
		return getTopMappings(numberMappingsToReturn);
	}
	
	/**
	 * This method is called by the compare() method to return the top 
	 * mappings after two scenes have been compared.
	 */
	private final Vector getTopMappings(int numberToReturn) {
		if (mappings_ == null) {
			return null;
		}
		// print out the top mappings and add them to the vectors
		// that will be later passed to the graph drawer to graphically
		// illustrate the analogous mapping between scene graphs
		System.out.println("\nTop " + numberToReturn + " mappings are: ");
		Vector topMappings = new Vector(numberToReturn);
		Enumeration e = mappings_.elements();
		for (int i=1; (i<=numberToReturn && e.hasMoreElements()); i++) {
			Unit next = (Unit)e.nextElement();
			System.out.println(i + "\t" + next);
			topMappings.add(next);
		}
		return topMappings;
	}

	private final MappingUnit makeMappingUnit(	double activation, 
												String clazzMoniker,
												String similarityDesc,
												Node sourceNode,
												Node targetNode,
												int depthSi, int indexSi,
												int depthTj, int indexTj) {

		// if there is a motion pattern in the scene, then all static
		// mappings are scaled down so that the dynamic aspects of the 
		// scene are more prominent
		if ( compareBehaviors_ == true &&
			(!(sourceNode instanceof Behavior) && 
			 !(targetNode instanceof Behavior))) {
			 activation *= STATIC_WEIGHTING_FACTOR;
		}
		
		// Note - depthSi and depthTj have one added to shift the indexes
		// such that the first index is 1 and not 0 (0 is required for 
		// the array indexing)
		MappingUnit munit = new MappingUnit(activation, clazzMoniker,
											similarityDesc,
											sourceNode, targetNode,
											depthSi+1, indexSi,
											depthTj+1, indexTj);
		mappings_.add(munit);
		return munit;
	}

	final Link makeLink(Unit a, double weight, Unit b) {
		Link link = new Link(a, b, weight);
		a.add(link);
		b.add(link);
		links_.add(link);
		return link;
	}
	
	final void searchGraphsForMappings(	int depthSi, Node si, 
										BranchGroup root2, Group siP, 
										double branchRadius) {

		Vector mappingsForSi = new Vector(10);
		Vector mappingsForSiP = new Vector(10);
					
		// increment the index for this depth
		depthIndexesSi_[depthSi]++; 

		// searchTargetGraph method is called to check all of the nodes 
		// in the target graph and see how they compare with si
		searchTargetGraphForSimilarNode(depthSi, 0, si, root2, siP, null, 
										mappingsForSi, branchRadius, 
										targetRadius_);

		// having compared all the nodes in the target graph with si,
		// reset the indexes for the target graph
		for (int i=0; i<depthIndexesTj_.length; i++) {
			depthIndexesTj_[i]=0;
		}
		
		// this method checks the mappings that were added for the node si
		// if more than one mapping was added between si and nodes in the 
		// target scene graph, then negative links will be established
		// to inhibit both these mappings from being active (si can only
		// map with one node in the target scene graph). The same is done for
		// mappings that were established between parent nodes
		if (mappingsForSi.size() > 1) {
			addConstraintsBetweenMappingsForSi(mappingsForSi);
		}

		// if this node is a Group then we must descend through the children
		// if si is a Primitive, then we don't descend further in the tree
		if (si instanceof Group && !(si instanceof Primitive) ) {
			double newBranchRadius = branchRadius;
			int numChildrenSi = ((Group)si).numChildren();
			if (numChildrenSi > 1) {
				newBranchRadius = J3DUtils.getRadius(si);
			}
			
			try {
				Enumeration e = ((Group)si).getAllChildren();
				while (e.hasMoreElements()) {
					Node si_plus1 = (Node)e.nextElement();

					// call this funtion recursively starting from the next
					// node in the source scene graph (si_plus1)
					searchGraphsForMappings(depthSi+1, si_plus1, 
											root2, (Group)si, 
											newBranchRadius);
				}
			} catch (CapabilityNotSetException e) {
				Control.error(
					"capability not set to descend source graph: " + e);
			}
		} 
	}

	final void searchTargetGraphForSimilarNode(	int depthSi,
												int depthTj,
												Node si, 
												Node tj,
												Group siP,
												Group tjP,
												Vector mappingsForSi,
												double branchSiRadius,
												double branchTjRadius) {
		
		// increment the target graph index
		depthIndexesTj_[depthTj]++;

		MappingUnit mappingUnit = checkNodeSimilarity(	depthSi, depthTj,
														si, tj,
														branchSiRadius,
														branchTjRadius);

		if (tj instanceof Group && !(tj instanceof Primitive)) {
			
			double newBranchRadius = branchTjRadius;
			int numChildrenTj = ((Group)tj).numChildren();
			if (numChildrenTj > 1) {
				newBranchRadius = J3DUtils.getRadius(tj);
			}

			// tj is a Group so we need to go through all of its children
			try {
				Enumeration e = ((Group)tj).getAllChildren();
				for (int i=0; e.hasMoreElements(); i++) {
					Node tj_plus1 = (Node)e.nextElement();

					searchTargetGraphForSimilarNode(depthSi,
													depthTj+1,
													si, tj_plus1, 
													siP, (Group)tj,
													mappingsForSi,
													branchSiRadius,
													newBranchRadius);
				}
			} catch (CapabilityNotSetException e2) {
				Control.error(
					"capability not set to descend target graph: " + e2);
			}
		} 

		if (mappingUnit != null) {
			mappingsForSi.add(mappingUnit);

			// create a link between this mapping unit and the special
			// unit that is always active
			if (weightingScheme_ == 0) {
				makeLink(mappingUnit, SPECIAL_LINK_WEIGHT, special_);
			} else {
				double ratio = mappingUnit.getActivation()/10.0;
				makeLink(mappingUnit, ratio, special_);
			}

			// to conserve graph relationships, consider adding a link between
			// this mapping (si=tj) and a mapping between parents (siP=tjP)

			if (siP != null && tjP != null) {
				// search to see if there is already a mapping
				MappingUnit parentMapping = searchForExistingMapping(siP, tjP);

				// if a mapping (siP=tjP) does not exists at this point in 
				// the recursion, then checkGroupSimilarity(siP, tjP) 
				// must have returned null at a level above in the recursion.
				if (parentMapping != null) {
					if (weightingScheme_ == 0) {
						makeLink(mappingUnit, GRAPH_LINK_WEIGHT, special_);
					} else {
						double ratio = mappingUnit.getActivation()/10.0;
						makeLink(parentMapping, ratio, mappingUnit);
					}
				}

			}
		}
	}

	final void addConstraintsBetweenMappingsForSi(Vector mappings) {

		sortMappingUnits(mappings);
		
		// trim down the mappings vector to just the top elements
		if (mappings.size() > NUM_MAPPINGS_FOR_EACH_SI) {
			mappings.setSize(NUM_MAPPINGS_FOR_EACH_SI);
			mappings.trimToSize();
		}

		// now establish inhibition constraints for mappings between nodes
		// in the target graph and the same node si (we only want one _best_
		// mapping for each node si)
		for (int i=0; i<mappings.size(); i++) {
			MappingUnit uniti = (MappingUnit)mappings.elementAt(i);
			for (int j=(i+1); j<mappings.size(); j++) {
				MappingUnit unitj = (MappingUnit)mappings.elementAt(j);
				makeLink(uniti, INHIBIT_LINK_WEIGHT, unitj);
			}
		}
	}

	final MappingUnit searchForExistingMapping(Node si, Node tj) {
		Enumeration e = mappings_.elements();
		while (e.hasMoreElements()) {
			MappingUnit munit = (MappingUnit)e.nextElement();
			if (munit.getSi().equals(si)) {
				if (munit.getTj().equals(tj)) {
					return munit;
				}
			}
		}
		return null;
	}

	// S I M I L A R I T Y  T E S T S

	final MappingUnit checkNodeSimilarity(	int depthSi, int depthTj,
											Node si, Node tj,
											double branchSiRadius,
											double branchTjRadius) {

		String qualifiedName = si.getClass().getName();
		String clazzMoniker = 
			qualifiedName.substring(qualifiedName.lastIndexOf('.')+1);
		StringBuffer similarityTest = new StringBuffer(80);
		double activation = 0.0;
		MappingUnit mappingUnit = null;

		if (si instanceof Group && tj instanceof Group) {
		
		int numChildrenSi = ((Group)si).numChildren();
		int numChildrenTj = ((Group)tj).numChildren();

		// SIMILARITY TEST
		// the ratio of similarity for Groups is based on the 
		// number of children. Note, if both have 1 child, do 
		// not use the full ratio of 1/1 for similarity. There 
		// are too many uninteresting group nodes with only 1 child

		similarityTest.append("Group");

		if (numChildrenSi > 1 && numChildrenTj > 1) {
			double ratio = getRatio(numChildrenSi, numChildrenTj);
			similarityTest.append("(" + Control.format(ratio, 3) + ")");
			activation += ratio;
		} 
		similarityTest.append(": ");


		// There are a few special motion objects that we check for 
		// For example, if there is a convergence motion pattern,
		// do these nodes represent an object that is being converged at?
		if (compareBehaviors_ == true) {

			ConvergenceMotion sourceConvergence = 
				sourceDynamics_.convergenceMotion();
			ConvergenceMotion targetConvergence = 
				targetDynamics_.convergenceMotion();

			if (sourceConvergence != null && targetConvergence != null) {
				activation += sourceConvergence.compareWithCenterObjects(	
														targetConvergence, 
														(Group)si, (Group)tj,
														similarityTest);
			}
		}
		
		// B R A N C H  G R O U P
		
		if (si instanceof BranchGroup && tj instanceof BranchGroup) {
			
			// BranchGroups server as the root nodes of a scene. 
			// Make a mapping between these nodes simply based on the 
			// fact that they are root nodes
			similarityTest.append("BranchGroup: root pointer (" +
				Control.format(BRANCH_GROUP_ACTIVATION, 1) + ")");
			activation += BRANCH_GROUP_ACTIVATION;
		
		// T R A N S F O R M  G R O U P 
		
		} else if (	si instanceof TransformGroup && 
					tj instanceof TransformGroup) {

			similarityTest.append("TransformGroup: ");

			Transform3D transformSi = new Transform3D();
			Transform3D transformTj = new Transform3D();
			((TransformGroup)si).getTransform(transformSi);
			((TransformGroup)tj).getTransform(transformTj);
			
			/*
			System.out.println(
				"Checking for similarity between transforms: ");
			System.out.println("TransformSi:");
			System.out.println(transformSi);
			System.out.println("TransformTj:");
			System.out.println(transformTj);
			*/
			
			// note if we map between all transfrom groups simply because 
			// they are the same type, we endup with a huge number of mappings

			// see the Transform3D documentation for type info
			// (eg. ZERO, IDENTITY, SCALE/TRANSLATION...)
			/*
			System.out.println("Best types: " + transformSi.getBestType() +
				", " + transformTj.getBestType());
			System.out.println("Types: " + transformSi.getType() +
				", " + transformTj.getType());
			*/

			int typeSi = transformSi.getBestType();
			int typeTj = transformTj.getBestType();
			
			// check to see whether the Transform3D are identical. 
			// If they are, then si * tj(inverse) = identity

			Transform3D check = new Transform3D();
			check.mulInverse(transformSi, transformTj);

			if (check.equals(identityTransform3D_)) {
				if ((typeSi & Transform3D.ZERO) > 0 && 
					(typeTj & Transform3D.ZERO) > 0 ) {

					similarityTest.append("ZERO matrices (" +
						Control.format(ZERO_MATRIX_ACTIVATION, 1) + ")");
					activation += ZERO_MATRIX_ACTIVATION;

				} else if (	(typeSi & Transform3D.IDENTITY) > 0 && 
							(typeTj & Transform3D.IDENTITY) > 0 ) {

					similarityTest.append("IDENTITY matrices (" +
						Control.format(IDENTITY_MATRIX_ACTIVATION, 1) + ")");
					activation += IDENTITY_MATRIX_ACTIVATION;

				} else {

					similarityTest.append("Identical matrices (" +
						Control.format(IDENTICAL_MATRIX_ACTIVATION, 1) + ")");
					activation += IDENTICAL_MATRIX_ACTIVATION;
				}

			// if the matrices are not identical, then we have to go 
			// through the painful process of finding out what might be 
			// potentially similar... start with SCALE... 

			// S C A L E
			// these matrices contain scale information
			// Note a scale matrix is in the following format:
			/*
					| X  0  0  0 |
				M = | 0  Y  0  0 |
					| 0  0  Z  0 |
					| 0  0  0  1 |
			*/
			} else if (	(typeSi & Transform3D.SCALE) > 0 && 
						(typeTj & Transform3D.SCALE) > 0 ) {

				// note - there is also a getScale(Vector3d) method in 
				// Transform3D that gets the possibly different x,y,z scale
				// factors for this Transform. However, for the purpose of 
				// visual analogy, if the Transform is a scale, then a 
				// mapping should be established even if they scale is in 
				// a different direction. However, this similarity test could
				// be expanded so that a stronger mapping would be established
				// if the x,y,z scale factors were very close, rather than 
				// just the largest scale factor that is currently checked
				// with the getScale() method.

				activation += compareUniformScales(	transformSi, 
													transformTj,
													branchSiRadius, 
													branchTjRadius,
													similarityTest);
			} else {

			// Note - Java3D seems to have problems selecting the 'best type'
			// for any matrix containing translation and rotation information.
			// I kept having problems with translation matrices being 
			// classified with the best type of Transform3D.ORTHOOGONAL
			// To get around this, I manually classify the matrix, by 
			// looking at the upper left 3x3 - if it is an identity matrix
			// I only check for translation components. If it is not,
			// then it also contains rotation information...

			Matrix3d upper3by3Si = new Matrix3d();
			Vector3d transCompSi = new Vector3d();
			Matrix3d upper3by3Tj = new Matrix3d();
			Vector3d transCompTj = new Vector3d();

			transformSi.get(upper3by3Si, transCompSi);
			transformTj.get(upper3by3Tj, transCompTj);
			
			// T R A N S L A T I O N
			// if these are not pure rotations, then maybe they are pure
			// translations (all 1's on the diagonal)
			// Note a translation matrix is in the following format:
			/*
					| 1  0  0  X |
				M = | 0  1  0  Y |
					| 0  0  1  Z |
					| 0  0  0  1 |
			*/
			
			// no rotation components - therefore a pure translation
			if (	upper3by3Si.equals(identityMatrix3d_) &&
					upper3by3Tj.equals(identityMatrix3d_) ) {
				
				activation += compareTranslations(	transCompSi, transCompTj,
													branchSiRadius,
													branchTjRadius,
													similarityTest);
			
			// O R T H O G O N A L (Rotation)
			// (scale is unity and there are no translational components -
			// this matrix can be used for pure rotation. If the determinant
			// is negative - it is a reflection and a rotation matrix).
			// Note - a rotation matrix is in the following format:
			/*
					|  CE      -CF      -D   0 |
				M = | -BDE+AF   BDF+AE  -BC  0 |
					|  ADE+BF  -ADF+BE   AC  0 |
					|  0        0        0   1 |

			  where A,B are the cosine and sine of the X-axis rotation axis,
					C,D are the cosine and sine of the Y-axis rotation axis,
					E,F are the cosine and sine of the Z-axis rotation axis.
			*/

			// no translation components - therefore a pure rotation
			} else if (	transCompSi.equals(zeroVector3d_) &&
						transCompTj.equals(zeroVector3d_) ) {
				
				activation += compareRotations(	transformSi, transformTj,
												similarityTest);
			
			// not a pure translation or rotation
			} else { 
				
				activation += compareTranslations(	transCompSi, transCompTj,
													branchSiRadius,
													branchTjRadius,
													similarityTest);

				activation += compareRotations(	transformSi, transformTj,
												similarityTest);
			}
			}

//
// - here is the code from before I found problems with the Java3D
//   getBestType() method - it was not classifying matrices correctly
//   see the new approach above
//
//			// T R A N S L A T I O N
//			// if these are not pure rotations, then maybe they are pure
//			// translations (all 1's on the diagonal)
//			// Note a translation matrix is in the following format:
//			/*
//					| 1  0  0  X |
//				M = | 0  1  0  Y |
//					| 0  0  1  Z |
//					| 0  0  0  1 |
//			*/
//			} else if (	(typeSi & Transform3D.TRANSLATION) > 0 && 
//						(typeTj & Transform3D.TRANSLATION) > 0 ) {
//
//				activation += compareTranslations(	transformSi, transformTj,
//													branchSiRadius,
//													branchTjRadius,
//													similarityTest);
//
//			// O R T H O G O N A L
//			// (scale is unity and there are no translational components -
//			// this matrix can be used for pure rotation. If the determinant
//			// is negative - it is a reflection and a rotation matrix).
//			// Note - a rotation matrix is in the following format:
//			/*
//					|  CE      -CF      -D   0 |
//				M = | -BDE+AF   BDF+AE  -BC  0 |
//					|  ADE+BF  -ADF+BE   AC  0 |
//					|  0        0        0   1 |
//
//			  where A,B are the cosine and sine of the X-axis rotation axis,
//					C,D are the cosine and sine of the Y-axis rotation axis,
//					E,F are the cosine and sine of the Z-axis rotation axis.
//			*/
//
//			} else if (	(typeSi & Transform3D.ORTHOGONAL) > 0 && 
//						(typeTj & Transform3D.ORTHOGONAL) > 0 ) {
//
//				// Java3D bug - matrices that are very much translations
//				// are often classified as 'ORTHOGONAL' by Java3D
//				// So we also need to compare the translational components
//
//				activation += compareTranslations(	transformSi, transformTj,
//													branchSiRadius,
//													branchTjRadius,
//													similarityTest);
//
//				activation += compareRotations(	transformSi, transformTj,
//												similarityTest);
//				
//				// note if the determinant is negative, then there is 
//				// no inverse of this 4x4 matrix. however, what about 
//				// the upper 3x3? 
//				if ((typeSi & Transform3D.NEGATIVE_DETERMINANT) > 0 && 
//					(typeTj & Transform3D.NEGATIVE_DETERMINANT) > 0 ) {
//					/*
//					similarityTest.append("Negative Determinant (" +
//						Control.format(NEGATIVE_DETERMINANT_ACTIVATION, 1) +
//						")");
//					// not sure how to compare similarity of reflections?
//					activation += NEGATIVE_DETERMINANT_ACTIVATION;
//					*/
//				}
//
//			// R I G I D
//			// these matrices are rotation and translations with unity scale
//			// The upper 3x3 is orthogonal 
//			} else if ( (typeSi & Transform3D.RIGID) > 0 &&
//						(typeTj & Transform3D.RIGID) > 0 ) {
//
//				activation += compareTranslations(	transformSi, transformTj,
//													branchSiRadius, 
//													branchTjRadius,
//													similarityTest);
//
//				activation += compareRotations(	transformSi, transformTj,
//												similarityTest);
//
//			// C O N G R U E N T
//			// this is an angle- and length-preserving matrix, meaning that 
//			// it can translate, rotate, and reflect about an axis, and scale 
//			// by an amount that is uniform in all directions. These 
//			// operations preserve the distance between any two points, 
//			// and the angle between any two intersecting lines
//			} else if ( (typeSi & Transform3D.CONGRUENT) > 0 &&
//						(typeTj & Transform3D.CONGRUENT) > 0 ) {
//
//				activation += compareUniformScales(	transformSi, transformTj,
//													branchSiRadius, 
//													branchTjRadius,
//													similarityTest);
//				activation += compareRotations(	transformSi, transformTj,
//												similarityTest);
//				activation += compareTranslations(	transformSi, transformTj,
//													branchSiRadius, 
//													branchTjRadius,
//													similarityTest);
//
//			// A F F I N E
//			// an affine matrix can translate, rotate, reflect, scale 
//			// anisotropically, and shear. Lines remain straight, and 
//			// parallel lines remain parallel, but the angle between 
//			// intersecting lines can change
//			} else if ( (typeSi & Transform3D.AFFINE) > 0 &&
//						(typeTj & Transform3D.AFFINE) > 0 ) {
//
//				activation += compareUniformScales(	transformSi, transformTj,
//													branchSiRadius, 
//													branchTjRadius,
//													similarityTest);
//				activation += compareRotations(	transformSi, transformTj,
//												similarityTest);
//				activation += compareTranslations(	transformSi, transformTj,
//													branchSiRadius, 
//													branchTjRadius,
//													similarityTest);
//
//				// TODO: how do we extract shear information?
//			}


		// P R I M I T I V E
		// eg. Box, Cone, Cylinder, Sphere
		} else if (	si instanceof Primitive && 
					tj instanceof Primitive) {
			
			double radiusSi = J3DUtils.getRadius(si);
			double radiusTj = J3DUtils.getRadius(tj);

			double primitiveSimilarity = getRelativeRatio(	radiusSi, 
															branchSiRadius,
															radiusTj,
															branchTjRadius,
															true);
			String primSimString = "Primitive(" + 
				Control.format(primitiveSimilarity, 3) + "): ";

			if (si instanceof Box &&
				tj instanceof Box) {
				similarityTest.append(primSimString + "Box, ");
				activation += primitiveSimilarity;

			} else if (	si instanceof Cone &&
						tj instanceof Cone) {
				similarityTest.append(primSimString + "Cone, ");
				activation += primitiveSimilarity;

			} else if (	si instanceof Cylinder &&
						tj instanceof Cylinder) {
				similarityTest.append(primSimString + "Cylinder, ");
				activation += primitiveSimilarity;

			} else if (	si instanceof Sphere &&
						tj instanceof Sphere) {
				similarityTest.append(primSimString + "Sphere, ");
				activation += primitiveSimilarity;
			}
			
			// compare the Appearances - note the appearance of primitives 
			// are compared regardless of their shape - we still want to 
			// map between a red box and a red cone based on their appearance
			Appearance appearanceSi = ((Primitive)si).getAppearance();
			Appearance appearanceTj = ((Primitive)tj).getAppearance();
			activation +=
				compareAppearances(appearanceSi, appearanceTj, similarityTest);
		}

		// L E A F  N O D E S
		
		} else if (si instanceof Leaf && tj instanceof Leaf) {
			similarityTest.append("Leaf: ");

		if (si instanceof Behavior && tj instanceof Behavior) {
			if (compareBehaviors_ == true) {

				Behavior bSi = (Behavior)si;
				Behavior bTj = (Behavior)tj;

				ConvergenceMotion convergenceSi = 
					sourceDynamics_.convergenceMotion();
				ConvergenceMotion convergenceTj = 
					targetDynamics_.convergenceMotion();

				if (convergenceSi != null &&
					convergenceTj != null ) {

						System.out.println(
							"Comparing behaviors for convergence");

						activation += convergenceSi.compareBehaviors(
										convergenceTj, bSi, bTj,
										similarityTest);
				}
			}
		} else if (si instanceof Shape3D && tj instanceof Shape3D) {

			similarityTest.append("Shape3D: ");

			Shape3D shapeSi = (Shape3D)si;
			Shape3D shapeTj = (Shape3D)tj;
			
			// compare appearances for the Shape3D
			Appearance appSi = shapeSi.getAppearance();
			Appearance appTj = shapeTj.getAppearance();
			activation += 
				compareAppearances(appSi, appTj, similarityTest);
			
			// note - shape3D nodes are part of a primitive, and should
			// contain the same number of geometries if in fact they are 
			// similar. Note however, that most primitives only contain 
			// 1 geometry (spheres and cones both contain one geometry, ah!
			// this makes it hard to accurately compare shapes

			int numGeosSi = shapeSi.numGeometries();
			int numGeosTj = shapeTj.numGeometries();

			//System.out.println("shapeSi has " + numGeosSi + 
			//					", shape Tj has " + numGeosTj);


			if ( (numGeosSi == numGeosTj) && (numGeosSi > 0)) {
				Enumeration e1 = shapeSi.getAllGeometries();
				Enumeration e2 = shapeTj.getAllGeometries();

				// note - e1 and e2 have been checked to ensure that 
				// they contain the same number of elements
				double total = 0.0;
				int i=0;
				for (; e1.hasMoreElements(); i++) {
					Geometry si_i = (Geometry)e1.nextElement();
					Geometry tj_i = (Geometry)e2.nextElement();
					total += compareGeometries(si_i, tj_i);
				}
				double geometrySim = total/(double)i;
				similarityTest.append("Geometry(" + 
					Control.format(geometrySim, 3) + "), ");
				// use the average of the geometries contained in the Shape3D
				activation += geometrySim;
			}
		}

		} // end of leaf tests

		//System.out.println("SimilarityTest: " + similarityTest);

		// if one or more of the similarity tests proved successful, 
		// then there will greater than zero activation for a new 
		// mapping unit between the two Transform nodes
		// Note - only establish mappings for similarity tests greater 
		// than 0.2? - otherwise some comparisons end up with thousands
		// of mappings between largely irrelevant nodes
		if (activation > 0.2) {	
			if (activation > 2.0) {
				System.out.println(
					"WARNING: similarity tests greater than 2.0");
				//activation = 1.0;
			}
			mappingUnit = makeMappingUnit(	activation, clazzMoniker, 
											similarityTest.toString(), 
											si, tj,
											depthSi, 
											depthIndexesSi_[depthSi],
											depthTj,
											depthIndexesTj_[depthTj]);
		}
		return mappingUnit;		
	}

	double compareUniformScales(Transform3D transformSi, 
								Transform3D transformTj,
								double radiusSi, double radiusTj,
								StringBuffer similarityTest) {

		double ratio = getRelativeRatio(transformSi.getScale(), radiusSi,
										transformTj.getScale(), radiusTj,
										true);
		similarityTest.append("Scale(" + Control.format(ratio, 3) + "), ");
		return ratio;
	}

	double compareRotations(Transform3D transformSi, 
							Transform3D transformTj,
							StringBuffer similarityTest) {

		Vector3d eulerAnglesSi = new Vector3d();
		Vector3d eulerAnglesTj = new Vector3d();
		getEulerAngles(transformSi, eulerAnglesSi);
		getEulerAngles(transformTj, eulerAnglesTj);
		double ratio = compareTuples(	eulerAnglesSi, 360.0, 
										eulerAnglesTj, 360.0, true);

		similarityTest.append("Rotation(" + Control.format(ratio, 3) + "), ");
		return ratio;
	}

	double compareTranslations(	Vector3d transSi, Vector3d transTj,
								double radiusSi, double radiusTj,
								StringBuffer similarityTest) {

		double ratio = compareTuples(	transSi, radiusSi, transTj, radiusTj, 
										true);
		similarityTest.append(
			"Translation(" + Control.format(ratio, 3) + "), ");
		return ratio;
	}

	final double compareAppearances(Appearance appSi, Appearance appTj,
									StringBuffer similarityTest) {
		double ratio = 0.0;
		Material matSi = appSi.getMaterial();
		Material matTj = appTj.getMaterial();
		if (matSi != null && matTj != null) {
			ratio += compareMaterials(matSi, matTj);
		}

		Texture texSi = appSi.getTexture();
		Texture texTj = appTj.getTexture();
		// note - Textures are not currently compared
		// however, if both objects are textured, establish a weak mapping
		if (texSi != null && texTj != null) {
			ratio += 0.1;
		}
		similarityTest.append(
			"Appearance(" + Control.format(ratio, 3) + "), ");
		return ratio;
	}

	final double compareMaterials(Material matSi, Material matTj) {
		Color3f temp = new Color3f();

		// D I F F U S E  C O L O R
		matSi.getDiffuseColor(temp);
		Tuple3d diffuseSi = new Vector3d(temp);
		matTj.getDiffuseColor(temp);
		Tuple3d diffuseTj = new Vector3d(temp);

		// note colours are all in the zero to one range
		return compareTuples(diffuseSi, 1.0, diffuseTj, 1.0, false);
	}

	final double compareGeometries(Geometry geoSi, Geometry geoTj) {

		//System.out.println("geoSi class: " + geoSi.getClass().getName());
		//System.out.println("geoTj class: " + geoTj.getClass().getName());
		
		// note - geometries contain seperate arrays of positional 
		// coordinates, colors, normals and texture coordinates. The 
		// appearance has already been compared, so this method only 
		// compare the coordinates. 

		if (geoSi instanceof GeometryArray && 
			geoTj instanceof GeometryArray) {

			// Only compare geometries if the vertex count is equal - 
			// this is how we distinguish between LineArray, QuadArray and 
			// TriangleArray objects.
			int vertexCount = ((GeometryArray)geoSi).getVertexCount();
			int vertexCountTj = ((GeometryArray)geoTj).getVertexCount();
			if (vertexCount != vertexCountTj) {
				//System.out.println(
				//	"compareGeometries: Vertex count not equal");
				return 0.0;
			}

			//System.out.println("Vertex count: " + vertexCount);
			
			// compare the vertices normalized
			Point3d[] geoSiVerts = new Point3d[vertexCount];
			Point3d[] geoTjVerts = new Point3d[vertexCount];

			// initialize these arrays (getCoordinates requires this)
			for (int i=0; i<vertexCount; i++) {
				geoSiVerts[i] = new Point3d();
				geoTjVerts[i] = new Point3d();
			}
			((GeometryArray)geoSi).getCoordinates(0, geoSiVerts);
			((GeometryArray)geoTj).getCoordinates(0, geoTjVerts);
		
		/* 
			// TODO
			// only works if data set by reference? - see java docs, possibly
			// this could be a future speed improvement
			Point3d[] geoSiVerts = ((GeometryArray)geoSi).getCoordRef3d();
			Point3d[] geoTjVerts = ((GeometryArray)geoTj).getCoordRef3d();
		*/

			double largestValueSi = getLargestValue(geoSiVerts);
			double largestValueTj = getLargestValue(geoTjVerts);
			
			double total = 0.0;
			for (int i=0; i<vertexCount; i++) {
				Point3d vertSi = (Point3d)geoSiVerts[i];
				Point3d vertTj = (Point3d)geoTjVerts[i];
				total += compareTuples(	vertSi, largestValueSi,
										vertTj, largestValueTj, true);
				//System.out.println(total);
			}
			// use the average
			double ratio = total/((double)vertexCount);
			//System.out.println("Ratio: " + ratio);
			return ratio;
		}
		System.out.println("WARNING: unsupported geometry class");
		return 0.0;
	}
	
	/**
	 * This method is called by compare() to settle the network after
	 * mappings have been established.
	 */
	private final void settleConstraintNetwork() {

		if (mappings_ == null) {
			Control.error(
				"settleConstraintNetwork() called when no mappings set");
			return;
		}

		for (int i=0; i<1000; i++) {
			double maxDifference = 0.0;
			Enumeration e = mappings_.elements();
			while (e.hasMoreElements()) {
				Unit next = (Unit)e.nextElement();
					
				// the argument to updateActivation specifies whether
				// or not to use Grossberg's update rule
				next.updateActivation(false);
			}

			// now go through all the units again and set the newly 
			// calculated activation values to the current activation
			// check the difference between the new activation and 
			// the old activation to see if we are asymptoting
			e = mappings_.elements();
			while (e.hasMoreElements()) {
				Unit next = (Unit)e.nextElement();
				double difference = next.setNewActivation();
				if (difference > maxDifference) {
					maxDifference = difference;
				}
			}
			if (maxDifference < 0.001) {
				Control.status(
					i + " iterations to settle constraint network");
				break;
			}
		}
		sortMappingUnits(mappings_);
	}

	final private void sortMappingUnits(Vector mappings) {
		try {
			Collections.sort(mappings);
			Collections.reverse(mappings);	// we want descending order
											// from most active to least
		} catch (ClassCastException e) {
			Control.error("while sorting mappings: " + e); 
		} catch (UnsupportedOperationException e) {
			Control.error("while sorting mappings: " + e);
		}
	}
}
