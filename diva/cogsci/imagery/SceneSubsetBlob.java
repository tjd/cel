// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.imagery;

import cogsci.Control;
import java.io.Serializable;
import javax.media.j3d.Node;
import javax.media.j3d.Transform3D;
import javax.media.j3d.BranchGroup;
import cogsci.noa.Blob;
import cogsci.noa.BlobDecoder;

/**
 * Similar to a SceneBlob but represents a sub-graph of a scene-graph.
 */
public class SceneSubsetBlob extends SceneBlob implements Serializable {
	
	private Node subsetNode_;		// the subset of the scene graph,
									// eg. the leg from the dog scene
	public SceneSubsetBlob(	String name,
							MindsEye decoder, 
							BranchGroup root, 
							ViewInfo viewInfo,
							Node subset) {
		super(name, decoder, root, viewInfo);
		subsetNode_ = subset;
	}

	public Node getNode() { return subsetNode_; }
	public BranchGroup getRoot() { return root_; }

	public void serialize() {
		super.serialize();
		subsetNode_ = null;
	}
}
