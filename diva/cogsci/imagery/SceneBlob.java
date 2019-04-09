// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.imagery;

import cogsci.Control;
import java.io.Serializable;
import javax.media.j3d.*;
import cogsci.noa.Blob;
import cogsci.noa.BlobDecoder;

/**
 * All visual concepts are represented by SceneBlob objects in the associative
 * network (the model for long-term memory). A scene blob is basically a 
 * concept that contains specific Java3D data. All blobs require a decoder to
 * extract the data from long-term memory into working memory. In the case of
 * the scene blob, this is the MindsEye class.
 *
 * @see MindsEye
 * @see cogsci.noa.Blob
 */
public class SceneBlob extends Blob implements java.io.Serializable {
	
	protected BranchGroup root_;
	protected TransformGroup tg_;
	protected ViewInfo viewInfo_;

	SceneBlob(	String name,
				MindsEye meye,
				BranchGroup root) {
		super(name, meye);
		set(meye, root, meye.getViewInfo());
	}

	SceneBlob(	String name,
				MindsEye meye,
				BranchGroup root,
				ViewInfo viewInfo) {
		super(name, meye);
		set(meye, root, viewInfo);
	}

	private void set(MindsEye meye, BranchGroup root, ViewInfo viewInfo) {
		// all roots stored in memory
		// should be fully accessible,
		// readible, modifiable !!
		root_ = root;
		viewInfo_ = viewInfo;
		tg_ = meye.findTransformGroup(root);
		if (tg_ == null) {
			BranchGroup newRoot = new BranchGroup();
			TransformGroup tg = new TransformGroup();
			newRoot.addChild(tg);
			tg_ = tg;
			tg_.addChild(root_);
			root_ = newRoot;
		}
		meye.setLiveCapabilities(root_);
	}

	Node getNode() { return root_; }

	ViewInfo getViewInfo() { return viewInfo_; }
	void setViewInfo(ViewInfo view) { viewInfo_ = view; }

	public void serialize() {
		root_ = null;	// we can't serialize SceneGraphObjects's right now
						// hopefully a later release of Java3D will
						// support serialization
		tg_ = null;
		viewInfo_ = null;
		decoder_ = null;
	}

	public void unserialize() {
		decoder_ = Control.getDecoder(Control.IMAGERY);
	}

	// visual operations
	
	public Transform3D getTransform() {
		Transform3D transform = new Transform3D();
		tg_.getTransform(transform);
		return transform;
	}

	public void setTransform(Transform3D transform) {
		tg_.setTransform(transform);
	}
}
