// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.imagery;

import cogsci.Control;
import javax.media.j3d.*;
import javax.vecmath.*;
import java.util.*;

/**
 * Mapping unit is one of the two unit types in the constraint network. The
 * other unit type is Special. The mapping unit stores a potential mapping
 * between a node in one scene graph and a node in another scene graph.
 */
class MappingUnit extends Unit {

	private Node si_;
	private Node tj_;

	int depthSi_;
	int indexSi_;
	int depthTj_;
	int indexTj_;
	
	/**
	 * The constructor for a mapping unit requires the two nodes involved 
	 * in the mapping, plus a string describing why the two nodes were 
	 * deemed similar.
	 */
	MappingUnit(double activation, String clazzMoniker, String similarityDesc,
				Node si, Node tj, 
				int depthSi, int indexSi, int depthTj, int indexTj) {
		super(	activation, "MAPPING-" + clazzMoniker,
				"\t\t(" + depthSi + "," + indexSi + ") <--- " + 
					Control.format(activation, 2) + " ---> (" + depthTj +
					"," + indexTj + ")\n\tSimilarity test: " + similarityDesc);
			/*
			(si.toString()).substring((si.toString()).lastIndexOf('@')+1) + 
				" <---?=?---> \t" + 
			(tj.toString()).substring((tj.toString()).lastIndexOf('@')+1) + 
			*/

		si_ = si;
		tj_ = tj;

		depthSi_ = depthSi;
		indexSi_ = indexSi;
		depthTj_ = depthTj;
		indexTj_ = indexTj;
	}

	final Node getSi() {
		return si_;
	}

	final Node getTj() {
		return tj_;
	}
}
