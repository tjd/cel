// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.imagery;

import cogsci.Control;
import javax.media.j3d.*;

/**
 * Storage class to hold all the necessary info to recreate the current
 * view tree. There are three Transform3Ds, one for each of the 
 * TransformGroups in the view tree of the Visual Buffer. 
 * (vpWorldTrans, vpOrientTrans, vpBrowserTrans) and also a place 
 * for the current ViewPlatform.
 */
class ViewInfo {

	Transform3D worldTrans_;
	Transform3D orientTrans_;
	Transform3D browserTrans_;
	ViewPlatform platform_;
	
	ViewInfo(	Transform3D world, 
				Transform3D orient,
				Transform3D browser,
				ViewPlatform viewPlat) {

		worldTrans_ = world;
		orientTrans_ = orient;
		browserTrans_ = browser;
		platform_ = viewPlat;
	}
}
