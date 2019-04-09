// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.imagery;

// much of this code in this class was adopted from the VRML97 demo code

import cogsci.Control;
import util.debug.Debug;
import util.j3d.J3DUtils;

import java.io.*;
import java.awt.*;
import java.awt.font.TextLayout;
import java.awt.event.*;
import java.util.Enumeration;
import java.util.Vector;

import javax.media.j3d.*;
import javax.vecmath.*;

import vrml.InvalidVRMLSyntaxException;

import com.sun.j3d.utils.geometry.*;
import com.sun.j3d.utils.universe.SimpleUniverse;
import com.sun.j3d.utils.behaviors.mouse.*;
import com.sun.j3d.loaders.vrml97.VrmlLoader;
import com.sun.j3d.loaders.Loader;
import com.sun.j3d.loaders.vrml97.node.Viewpoint;

/**
 * The visual buffer class abstracts the bulk of the Java3D code required to
 * render and view 3-dimensional data in the user interface.
 */
public class VisualBuffer extends Canvas3D 
	implements MotionNotifierInterface {

    VirtualUniverse     universe;
    Locale              locale_;
    View                view;
    Loader				loader_;
    Viewpoint[]			fileViewpoints = null;
    TransformGroup[]	fileVpWorldTrans;
    TransformGroup[]	fileVpOrientTrans;
    TransformGroup[]	fileVpTrans;
    ViewPlatform[]		fileVp;
    BranchGroup			visualBufferRoot_;	// root node for the buffer
	BranchGroup			sceneRoot_;			// root node for the current
											// scene - this is the first 
											// child of visualBufferRoot_
    BoundingSphere		objBounds = null;
    BranchGroup			vpRoot_;
    BranchGroup			browserGroup_;
	BranchGroup			backgroundGroup_;

    DirectionalLight	headLight;
    AmbientLight		ambLight;
    ViewPlatform		vp_;
    TransformGroup 		vpBrowserTrans_, prevVpBrowserTrans;
    TransformGroup		vpWorldTrans_, vpOrientTrans_;
    TransformGroup		viewTransformGroup_;
    TransformGroup		objVpWorldTrans[] = new TransformGroup[NUM_OBJ_VIEWS];
    TransformGroup		objVpOrientTrans[] = new TransformGroup[NUM_OBJ_VIEWS];
    TransformGroup		objVpTrans[] = new TransformGroup[NUM_OBJ_VIEWS];
    ViewPlatform		objVp[] = new ViewPlatform[NUM_OBJ_VIEWS];
    BoundingSphere		browserBounds;
    float				vpFieldOfView, vpFrontClip, vpBackClip;
    float				objVpFieldOfView, objVpFrontClip;
    float				objVpBackClip;
    int					viewMode = EXAMINE;
    int					initVp = OBJ_VIEW_PLUS_Z;

    Transform3D			identity_ = new Transform3D();
    boolean				debug = false;
    boolean				timing;
    int					numTris;
    boolean				startupTiming;
    int					startupCount;
    int					startupFrames = 5;
    int					numFrames = 0;
    long				postTime;
    long 				baseTime = System.currentTimeMillis();

    TreePrinter		tp = new TreePrinter();

    static final int	AMB_LIGHT = 0;
    static final int	DIR_LIGHT = 1;
    static final int	BEHAVIOR  = 2;
    static final int	NUM_SLOTS = 3;

    public static final int	EXAMINE = 1;
    public static final int	FLY = 2;

    public static final int	FILE_VIEW = -1;

	// Default perspective views
    public static final int    OBJ_VIEW_PLUS_X = 0;
    public static final int    OBJ_VIEW_PLUS_Y = 1;
    public static final int    OBJ_VIEW_PLUS_Z = 2;
    public static final int    NUM_OBJ_VIEWS = 3;

	final Font infoFont_ = new Font("Helvetica", Font.PLAIN, 9);

	protected GraphDrawer graphDrawer_;
	private BranchGroup axesBranchGroup_;

	private boolean drawGraph_ = false;
	

	public VisualBuffer(GraphicsConfiguration config) {
		super(null);
		// note a GraphicsConfiguration is supplied to the visual buffer,
		// however if you use the graphicsConfiguration from the main window
		// of the program (Imagine.java), the Java3D rendering window is 
		// really choppy. Calling the Canvas3D constructor with null seems 
		// to allocate a seperate thread for Java3D and this makes animations
		// much smoother. unfortunately, super(null) gives a warning, i 
		// don't know why - super(null) is used in some of the Java3d demos
		// super(config)
		// maybe this would be fine on a faster computer, however my guess
		// is that by initializing a new GraphicsConfiguration for the 
		// Canvas3D it is able to prioritize CPU time.
		
		loader_ = new VrmlLoader();

		// load everything: viewpoints, sounds, behaviors ...
		// hopefully one day behaviors will be loaded !!
		loader_.setFlags(Loader.LOAD_ALL);
		setupJ3D();

		graphDrawer_ = new GraphDrawer();
    }

	public void setColors(Color foreground, Color background) {
		setForeground(foreground);
		setBackground(background);
		graphDrawer_.setColors(foreground, background);

		if (backgroundGroup_ != null) {
			locale_.removeBranchGraph(backgroundGroup_);
		}

		Background bgrnd = new Background(new Color3f(background));
		Bounds bounds = new BoundingSphere(new Point3d(0.0,0.0,0.0),100.0);
		bgrnd.setApplicationBounds(bounds);

		backgroundGroup_ = new BranchGroup();
		backgroundGroup_.setCapability(BranchGroup.ALLOW_DETACH);
		TransformGroup tg = new TransformGroup();
		backgroundGroup_.addChild(tg);
		tg.addChild(bgrnd);
		

		backgroundGroup_.compile();	// we can complile the background because
									// we don't have to clone them or 
									// do any fancy tricks with this graph

		locale_.addBranchGraph(backgroundGroup_);
		repaint();
	}

	public void setDrawGraph(boolean drawGraph) {
		drawGraph_ = drawGraph;
		if (drawGraph_ == true) {
			stopRenderer();
		} else {
			startRenderer();
		}
		repaint();
	}

	public void setDrawAxes(boolean drawAxes) {
		if (drawAxes) {
			if (axesBranchGroup_ == null) {
				createAxesBranchGroup();
			}
			locale_.addBranchGraph(axesBranchGroup_);
		} else {
			locale_.removeBranchGraph(axesBranchGroup_);
		}
		repaint();
	}
	
	//
	// Make sure to class remove old scene first
	// removeOldScene()
	//
	synchronized void display(BranchGroup node) {
		display(node, null, null);
	}

	synchronized void display(	BranchGroup node, 
								BranchGroup node2,
								Vector mappings) {

		try {
			//System.out.println("Displaying node: " + node);
			if (debug ) {
				System.gc();
				System.out.println("After parse, freeMemory = " + 
					Control.format(
					Runtime.getRuntime().freeMemory()/(1024*1024.0),2) +  
						" totalMemory = " + 
					Control.format(
					Runtime.getRuntime().totalMemory()/(1024*1024.0),2));

			   System.out.println("Adding subtree:");
			   tp.print(node);
			   System.out.println("");
			}
			
			// We could compile the scene group here to optimize the 
			// rendering speed, however because we want to dynamically
			// modify the scene don't bother compliling. Speed is not 
			// that important for this model
			// sceneGroup.compile();

			// add the brachgraph to our object tree
			visualBufferRoot_.addChild(node);

			// set this as the current scene root
			sceneRoot_ = node;
			locale_.addBranchGraph(visualBufferRoot_);
			objBounds = (BoundingSphere)visualBufferRoot_.getBounds();

			if (debug) {
				System.out.println("Object Bounds = " + objBounds);
			}
			scaleViewTransForScene();
			graphDrawer_.setSecondGraph(node2);
			graphDrawer_.setMappings(mappings);
		} catch (InvalidVRMLSyntaxException e) {
			System.err.println("VRML parse error");
			e.printStackTrace(System.err);
		}
		repaint();
		setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
    }

    public void paint(Graphics g) {
        Graphics2D g2 = (Graphics2D) g;
		if (drawGraph_ == true) {
			graphDrawer_.update(sceneRoot_, getWidth());
			graphDrawer_.drawAll(g2);
		} else {
			super.paint(g);
			drawAll(g2);
		}
    }

	public void drawAll(Graphics2D g2) {
		if (objBounds != null) {
			// Print the index information in the bottom left corner
			Point3d center = new Point3d();
			objBounds.getCenter(center);
			float x = (float)center.x;
			float y = (float)center.y;
			float z = (float)center.z;
			String string = "Center = {" + x + ", " + y + ", " + z + 
				"} Radius = " + (float)objBounds.getRadius();
			TextLayout info = new TextLayout(string, infoFont_,
				g2.getFontRenderContext());
			info.draw(g2, 10, 50);
			Control.status(string);
		}
	}

    // This is the normal UniverseBuilder code, except that the ViewPlatform
    // will only be used if VRML doesn't define one
    void setupJ3D() {
		setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
		
		// enable double buffering
		setDoubleBufferEnable(true);

		// Establish an unnamed virtual universe, with a single hi-res Locale
        universe = new VirtualUniverse();
        locale_ = new Locale(universe);

        // Create a PhysicalBody and Physical Environment object
        PhysicalBody body = new PhysicalBody();
        PhysicalEnvironment environment = new PhysicalEnvironment();

        // Create a View and attach the Canvas3D and the physical
        // body and environment to the view.
        view = new View();
        view.addCanvas3D(this);
        view.setPhysicalBody(body);
        view.setPhysicalEnvironment(environment);

        // Create a branch group node for the default view platform
        vpRoot_ = new BranchGroup();
		J3DUtils.setBranchGroupCapabilities(vpRoot_);

		// This is the stuff that the browser needs to have in the same
		// place as the view platform.  It will get added to the view platform
		// group when a view is selected
		browserGroup_ = new BranchGroup();
		browserGroup_.setCapability(BranchGroup.ALLOW_DETACH);
		browserGroup_.setCapability(BranchGroup.ALLOW_CHILDREN_READ);
		browserGroup_.setCapability(BranchGroup.ALLOW_CHILDREN_WRITE);
		setSize(browserGroup_, NUM_SLOTS);
		BoundingSphere lightBounds = 
                new BoundingSphere(new Point3d(), Double.MAX_VALUE);
		ambLight = new AmbientLight(true, new Color3f(0.2f, 0.2f, 0.2f));
		ambLight.setInfluencingBounds(lightBounds);
		ambLight.setCapability(Light.ALLOW_STATE_WRITE);
		browserGroup_.setChild(ambLight, AMB_LIGHT);
		headLight = new DirectionalLight();
		headLight.setColor(new Color3f(0.8f, 0.8f, 0.8f));
		headLight.setCapability(Light.ALLOW_STATE_WRITE);
		headLight.setInfluencingBounds(lightBounds);
		browserGroup_.setChild(headLight, DIR_LIGHT);


        // initialize the default view tree.  These are predefined views
		// which display the whole object along each of the axes.
		// The Object transform is used to scale and translate the object so 
		// it's BoundingSphere is at the origin with radius=1 in the View 
		// space.  The World transform is used to tranform the "world" 
		// relative to the view (examine mode). The Orient tranform is used 
		// to set the initial placement of the view (set by the VRML fields 
		// for VRML Viewpoints and initialzed here for obj views) and the
		// VpTrans is the transform from the initial view platform orientation
		// made by the browser
		objVpFieldOfView = .785398f;
		objVpFrontClip = 0.01f;
		objVpBackClip = 30.0f;
		double eyeDist = 1.1 / Math.tan(objVpFieldOfView/ 2.0);
		Point3d origin = new Point3d();
		Point3d offset = new Point3d();
		Vector3d up = new Vector3d();
		Transform3D eyeTrans = new Transform3D();

        viewTransformGroup_ =  new TransformGroup();
        viewTransformGroup_.setCapability(
			vpBrowserTrans_.ALLOW_TRANSFORM_READ);
        viewTransformGroup_.setCapability(
			vpBrowserTrans_.ALLOW_TRANSFORM_WRITE);
        viewTransformGroup_.setCapability(Group.ALLOW_CHILDREN_READ);
        viewTransformGroup_.setCapability(Group.ALLOW_CHILDREN_WRITE);
        viewTransformGroup_.setCapability(Group.ALLOW_CHILDREN_EXTEND);

	for (int i = 0; i < NUM_OBJ_VIEWS; i++) {
	    objVpWorldTrans[i] =  new TransformGroup();
	    objVpWorldTrans[i].setCapability(
			vpBrowserTrans_.ALLOW_TRANSFORM_READ);
	    objVpWorldTrans[i].setCapability(
			vpBrowserTrans_.ALLOW_TRANSFORM_WRITE);
	    objVpWorldTrans[i].setCapability(Group.ALLOW_CHILDREN_READ);
	    objVpWorldTrans[i].setCapability(Group.ALLOW_CHILDREN_WRITE);
	    objVpWorldTrans[i].setCapability(Group.ALLOW_CHILDREN_EXTEND);
	    viewTransformGroup_.addChild(objVpWorldTrans[i]);
	    objVpOrientTrans[i] =  new TransformGroup();
	    objVpOrientTrans[i].setCapability(
			vpBrowserTrans_.ALLOW_TRANSFORM_READ);
	    objVpOrientTrans[i].setCapability(
			vpBrowserTrans_.ALLOW_TRANSFORM_WRITE);
	    objVpOrientTrans[i].setCapability(Group.ALLOW_CHILDREN_READ);
	    objVpOrientTrans[i].setCapability(Group.ALLOW_CHILDREN_WRITE);
	    objVpOrientTrans[i].setCapability(Group.ALLOW_CHILDREN_EXTEND);
	    // set orient trans to view sphere of radius 1 at origin
		//System.out.println("eye dist: " + eyeDist);
	    switch (i) { 
	      case OBJ_VIEW_PLUS_X:
		offset.x = eyeDist;
		offset.y = 0.0;
		offset.z = 0.0;
		up.x = 0.0;
		up.y = 1.0;
		up.z = 0.0;
		eyeTrans.lookAt(offset, origin, up);
		//System.out.println("+X eye xform = \n" + eyeTrans);
		break;
	      case OBJ_VIEW_PLUS_Y:
		offset.x = 0.0;
		offset.y = eyeDist;
		offset.z = 0.0;
		up.x = 0.0;
		up.y = 0.0;
		up.z = 1.0;
		eyeTrans.lookAt(offset, origin, up);
		// System.out.println("+Y eye xform = \n" + eyeTrans);
		break;
	      case OBJ_VIEW_PLUS_Z:
		offset.x = 0.0;
		offset.y = 0.0;
		offset.z = eyeDist;
		up.x = 0.0;
		up.y = 1.0;
		up.z = 0.0;
		eyeTrans.lookAt(offset, origin, up);
		// System.out.println("+Z eye xform = \n" + eyeTrans);
		break;
	    }
	    eyeTrans.invert(); // need to invert because on view side of tree
	    objVpOrientTrans[i].setTransform(eyeTrans);
	    objVpWorldTrans[i].addChild(objVpOrientTrans[i]);
	    objVpTrans[i] = new TransformGroup();
	    objVpTrans[i].setCapability(vpBrowserTrans_.ALLOW_TRANSFORM_READ);
	    objVpTrans[i].setCapability(vpBrowserTrans_.ALLOW_TRANSFORM_WRITE);
	    objVpTrans[i].setCapability(Group.ALLOW_CHILDREN_READ);
	    objVpTrans[i].setCapability(Group.ALLOW_CHILDREN_WRITE);
	    objVpTrans[i].setCapability(Group.ALLOW_CHILDREN_EXTEND);
	    objVpOrientTrans[i].addChild(objVpTrans[i]);
	    objVp[i] = new ViewPlatform();
		objVp[i].setCapability(Node.ALLOW_BOUNDS_READ);
		objVp[i].setCapability(Node.ALLOW_LOCAL_TO_VWORLD_READ);
	    objVpTrans[i].addChild(objVp[i]);
	}

	browserBounds = new BoundingSphere(new Point3d(), Double.MAX_VALUE);

	// currently don't have a file version for these, so they will always
	// be the obj versions
	vpFieldOfView = objVpFieldOfView;
	vpFrontClip = objVpFrontClip;
	vpBackClip = objVpBackClip;

	updateBehavior();

	vpRoot_.addChild(viewTransformGroup_);

	// DC - WHAT?
	objVpTrans[0].addChild(browserGroup_);
	prevVpBrowserTrans = objVpTrans[0];

	// setup the initial obj viewpoints and use one
	scaleViewTransForScene();
	setObjViewpoint(0);

	// Attach the vpRoot_ to the universe, via the Locale.
	locale_.addBranchGraph(vpRoot_);

	visualBufferRoot_ = new BranchGroup();
	J3DUtils.setBranchGroupCapabilities(visualBufferRoot_);
		
	setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
    }

    protected void setSize(javax.media.j3d.Group g, int size) {
        for (int i = 0; i < size; i++) {
            g.addChild(new NullNode());
        }
    }

    void removeOldScene() {
		visualBufferRoot_.detach();
		int numToRemove = visualBufferRoot_.numChildren();
		for (int i = 0; i < numToRemove; i++) {
			visualBufferRoot_.removeChild(0);
		}
		fileViewpoints = null;
		objBounds = null;
    }

    public void resetViewpoint() {
		vpWorldTrans_.setTransform(identity_);
		vpBrowserTrans_.setTransform(identity_);
		repaint();
    }

    private void removeBgFromPrev() {
		if (debug) System.out.println("removeBgFromPrev()");
		int numChildren = prevVpBrowserTrans.numChildren();
		boolean found = false;
		for (int i = numChildren-1; i >= 0; i--) { 
			Node child = prevVpBrowserTrans.getChild(i);
			if (child.equals(browserGroup_)) {
				if (debug) System.out.println("removeBgFromPrev():" +
					" removing child " + i + " from group " +
					prevVpBrowserTrans);
				prevVpBrowserTrans.removeChild(i);
				found = true;
			}
		}
		if (!found) {
			System.out.println(
				"Can't find browserGroup_ on previous vpBrowserTrans_");
		}
    }
	
    private void updateView() {
		if (debug) System.out.println("updateView()");
		resetViewpoint();
		removeBgFromPrev();
		vpBrowserTrans_.addChild(browserGroup_);
		if (debug) System.out.println("updateView(): updating view");
		view.setFieldOfView(vpFieldOfView);
		view.setFrontClipDistance(vpFrontClip);
		view.setBackClipDistance(vpBackClip);
		view.attachViewPlatform(vp_);
		updateBehavior();
		prevVpBrowserTrans = vpBrowserTrans_;
		repaint();
    }

	public void setViewInfo(ViewInfo viewInfo) {
		if (debug) System.out.println("setViewInfo()");
		if (viewInfo == null) {
			Control.error("cannot update with a null view");
			return;
		}
		vpWorldTrans_.setTransform(viewInfo.worldTrans_);
		vpOrientTrans_.setTransform(viewInfo.orientTrans_);
		vpBrowserTrans_.setTransform(viewInfo.browserTrans_);
		vp_ = viewInfo.platform_;
		updateView();
	}

	public void setViewOrientTransform(Transform3D transform) {
		vpOrientTrans_.setTransform(transform);
		updateView();
	}

	/**
	 * This method returns the current view transform. This can be 
	 * stored for later use, and there is also a method to set it.
	 */
	public ViewInfo getViewInfo() {
		Transform3D worldTrans = new Transform3D();		// viewpoint scaling
		Transform3D orientTrans = new Transform3D();	// orientation
		Transform3D browserTrans = new Transform3D();	// mouse interaction

		vpWorldTrans_.getTransform(worldTrans);
		vpOrientTrans_.getTransform(orientTrans);
		vpBrowserTrans_.getTransform(browserTrans);

		ViewPlatform viewPlat = vp_;

	
		return new ViewInfo(worldTrans,
							orientTrans,
							browserTrans,
							viewPlat);
	}

    public void setFileViewpoint(int index) {
		if (debug) System.out.println("setFileViewpoint(" + index + ")");
		//Viewpoint viewpoint = fileViewpoints[index];
		vpWorldTrans_ = fileVpWorldTrans[index];
		vpOrientTrans_ = fileVpOrientTrans[index];
		vpBrowserTrans_ = fileVpTrans[index];
		vp_ = fileVp[index];
		updateView();
    }

    public void setObjViewpoint(int index) {
		if (debug) System.out.println("setObjViewpoint(" + index + ")");
		vp_ = objVp[index];
		vpBrowserTrans_ = objVpTrans[index];
		vpWorldTrans_ = objVpWorldTrans[index];
		vpOrientTrans_ = objVpOrientTrans[index];
		updateView();
    }

    void scaleViewTransForScene() {
		if (debug) System.out.println("scaleViewTransForScene()");
		Vector3d offset = new Vector3d();
		Transform3D objTrans = new Transform3D();

		if (objBounds == null) {
			viewTransformGroup_.setTransform(identity_);
		} else {

			// Scale the obj trans so that the bounding sphere has radius 1
			// in view space
			double radius = objBounds.getRadius();

			// Set the Obj trans so the bounding sphere is at the origin
			Point3d center = new Point3d();
			objBounds.getCenter(center);
			offset.x = center.x;
			offset.y = center.y;
			offset.z = center.z;
			if (debug) {
				System.out.println("Default view center:" + center +
				" distance " + radius);
			}
			objTrans.set(radius, offset);
			viewTransformGroup_.setTransform(objTrans);
		}
    }

    void setupFileViewpoints() {
		if (fileViewpoints != null) {
			fileVpWorldTrans = new TransformGroup[fileViewpoints.length];
			fileVpOrientTrans = new TransformGroup[fileViewpoints.length];
			fileVpTrans = new TransformGroup[fileViewpoints.length];
			fileVp = new ViewPlatform[fileViewpoints.length];

			for (int i = 0; i < fileViewpoints.length; i++) {
			// insert a transform above and below the Viewpoint's 
			// TransformGroup
			TransformGroup viewOri = fileViewpoints[i].getTransformGroup();
			viewOri.setCapability(BranchGroup.ALLOW_CHILDREN_READ);

			Group parent = (Group)viewOri.getParent();
			TransformGroup viewWorld = new TransformGroup();
			viewWorld.setCapability(viewWorld.ALLOW_TRANSFORM_READ);
			viewWorld.setCapability(viewWorld.ALLOW_TRANSFORM_WRITE);

			// Replace the TG by a new TG which will hold the ViewTG
			int numChildren = parent.numChildren();
			boolean found = false;
			for (int j = 0; (j < numChildren) && (!found); j++) {
				if (parent.getChild(j) == viewOri) {
					found = true;
					parent.setChild(viewWorld, j);
				}
			}

			if (!found) {
				System.err.println("Internal error, can't find viewOri");
			}
			viewWorld.addChild(viewOri);

			TransformGroup viewBrowser = new TransformGroup();
			viewBrowser.setCapability(viewBrowser.ALLOW_TRANSFORM_READ);
			viewBrowser.setCapability(viewBrowser.ALLOW_TRANSFORM_WRITE);
			viewBrowser.setCapability(viewBrowser.ALLOW_CHILDREN_READ);
			viewBrowser.setCapability(viewBrowser.ALLOW_CHILDREN_WRITE);
			viewBrowser.setCapability(viewBrowser.ALLOW_CHILDREN_EXTEND);
			viewOri.setChild(viewBrowser, 0);
			viewBrowser.addChild(fileViewpoints[i].getViewPlatform());

			fileVpWorldTrans[i] = viewWorld;
			fileVpOrientTrans[i] = viewOri;
			fileVpTrans[i] = viewBrowser;
			fileVp[i] = fileViewpoints[i].getViewPlatform();
			}
		}
    }

	public String[] getFileViewDescriptions() {
		if (fileViewpoints == null) { return null; }
		int length = fileViewpoints.length;
		String[] views = new String[length];
		for (int i=0; i<length; i++) {
			String desc;
			if (((desc = fileViewpoints[i].getDescription()) == null) ||
			    desc.equals("")) {
				desc = "Viewpoint " + i;
			}
			views[i] = desc;
		}
		return views;
	}

    public void setHeadlight(boolean state) {
		if (debug) System.out.println("setHeadlight(" + state + ")");
		ambLight.setEnable(state);
		headLight.setEnable(state);
	}
	
	// this method is overload by the MindsEye class for adding 
	// behaviors
	protected void addBehaviors(BranchGroup behaviorTree) {
	}

    private void updateBehavior() {
		if (debug) System.out.println("updateBehaviour()");
		BranchGroup bg = new BranchGroup();
		bg.setCapability(BranchGroup.ALLOW_DETACH);
		bg.setCapability(BranchGroup.ALLOW_CHILDREN_READ);
		addBehaviors(bg);
		switch (viewMode) {
		  case EXAMINE:
			MouseRotate mr = new MouseRotate(MouseRotate.INVERT_INPUT);
			mr.setTransformGroup(vpWorldTrans_);
			mr.setSchedulingBounds(browserBounds);
			bg.addChild(mr);
			MouseTranslate mt = new MouseTranslate(
				MouseTranslate.INVERT_INPUT);
			mt.setTransformGroup(vpWorldTrans_);
			mt.setSchedulingBounds(browserBounds);
			bg.addChild(mt);
			MouseZoom mz = new MouseZoom(MouseZoom.INVERT_INPUT);
			mz.setTransformGroup(vpWorldTrans_);
			mz.setSchedulingBounds(browserBounds);
			bg.addChild(mz);
			break;
		  case FLY:
			FlightBehavior fb = 
				new FlightBehavior(0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
				vpBrowserTrans_, this);
			bg.addChild(fb);
			break;
		}
		browserGroup_.setChild(bg, BEHAVIOR);
    }

    public void setMode(int mode) {
		viewMode = mode;
		updateBehavior();
    }
	
	public void updateView(	float eye_x, float eye_y, float eye_z,
							float loc_x, float loc_y, float loc_z) {
		
		System.out.println("updating view with to " + eye_x + ", " +
			eye_y + ", " + eye_z);
		Point3d eye = new Point3d(eye_x, eye_y, eye_z);
		Point3d loc = new Point3d(loc_x, loc_y, loc_z);

		// default orientation is with +y up
		Vector3d up  = new Vector3d(0.0, 1.0, 0.0);
		Transform3D trans = new Transform3D();
		trans.lookAt(eye, loc, up);
		try {
			trans.invert();
		} catch (SingularMatrixException e) {
			Control.error("could not invert specified eye point");
			return;
		}
		
		vpOrientTrans_.setTransform(trans);
		updateView();
	}
	
	/**
	 * These methods are useful for removing the root node of the 
	 * scene graph from the locale_ if you want to clone something in it.
	 */
	public BranchGroup detachRoot() {
	    visualBufferRoot_.detach();
		//locale_.removeBranchGraph(visualBufferRoot_);
		return visualBufferRoot_;
	}

	public void reattachRoot() {
		locale_.addBranchGraph(visualBufferRoot_);
	}

	synchronized void setLiveCapabilities(Node node) {
		detachRoot();
		J3DUtils.setCapabilities(node);
		reattachRoot();
	}

    // MotionNotifierInteface
    public void viewMoved( Matrix4d position ) {}
    public void velocityChanged(double velocity) {}
    public void buttonPressed(int buttonId) {};


	/**
	 * This method creates the axesBranchGroup
	 */
	private void createAxesBranchGroup() {
	
	    axesBranchGroup_ = new BranchGroup();

	    // create line for X axis
	    LineArray axisXLines = new LineArray(2, LineArray.COORDINATES  );
	    axesBranchGroup_.addChild(new Shape3D(axisXLines));

	    axisXLines.setCoordinate(0, new Point3f(-1.0f, 0.0f, 0.0f));
	    axisXLines.setCoordinate(1, new Point3f( 1.0f, 0.0f, 0.0f));

	    Color3f red   = new Color3f(1.0f, 0.0f, 0.0f);
	    Color3f green = new Color3f(0.0f, 1.0f, 0.0f);
	    Color3f blue  = new Color3f(0.0f, 0.0f, 1.0f);

	    // create line for Y axis
	    LineArray axisYLines = new LineArray(2, 
		LineArray.COORDINATES | LineArray.COLOR_3 );
	    axesBranchGroup_.addChild(new Shape3D(axisYLines));

	    axisYLines.setCoordinate(0, new Point3f( 0.0f,-1.0f, 0.0f));
	    axisYLines.setCoordinate(1, new Point3f( 0.0f, 1.0f, 0.0f));

	    axisYLines.setColor(0, green);
	    axisYLines.setColor(1, blue);

	    // create line for Z axis
	    Point3f z1 = new Point3f( 0.0f, 0.0f,-1.0f);
	    Point3f z2 = new Point3f( 0.0f, 0.0f, 1.0f);

	    LineArray axisZLines = new LineArray(10, 
			LineArray.COORDINATES  | LineArray.COLOR_3
		);
	    axesBranchGroup_.addChild(new Shape3D(axisZLines));

	    axisZLines.setCoordinate(0, z1);
	    axisZLines.setCoordinate(1, z2);
	    axisZLines.setCoordinate(2, z2);
	    axisZLines.setCoordinate(3, new Point3f( 0.1f, 0.1f, 0.9f));
	    axisZLines.setCoordinate(4, z2);
	    axisZLines.setCoordinate(5, new Point3f(-0.1f, 0.1f, 0.9f));
	    axisZLines.setCoordinate(6, z2);
	    axisZLines.setCoordinate(7, new Point3f( 0.1f,-0.1f, 0.9f));
	    axisZLines.setCoordinate(8, z2);
	    axisZLines.setCoordinate(9, new Point3f(-0.1f,-0.1f, 0.9f));

	    Color3f colors[] = new Color3f[9];

	    colors[0] = new Color3f(0.0f, 1.0f, 1.0f);
	    for(int v = 0; v < 9; v++){
			colors[v] = red;
	    }
	    axisZLines.setColors(1, colors);

		axesBranchGroup_.setCapability(BranchGroup.ALLOW_DETACH);

		axesBranchGroup_.compile();	// we can complile the axis because
									// we don't have to clone them or 
									// do any fancy tricks with this graph
	    	    
	} // end of createAxesBranchGroup 

}	// end of VisualBuffer.java

