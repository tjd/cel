// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.imagery;

import cogsci.Control;
import cogsci.noa.*;
import cogsci.imagery.gear.*;

import util.debug.Debug;
import util.j3d.J3DUtils;

import java.io.*;
import java.awt.*;
import java.awt.font.TextLayout;
import java.awt.event.*;
import java.util.*;

import javax.media.j3d.*;
import javax.vecmath.*;

import vrml.BaseNode;
import vrml.InvalidVRMLSyntaxException;

import com.sun.j3d.utils.geometry.*;
import com.sun.j3d.utils.universe.SimpleUniverse;
import com.sun.j3d.utils.behaviors.mouse.*;

import com.sun.j3d.loaders.vrml97.VrmlLoader;
import com.sun.j3d.loaders.vrml97.VrmlScene;
import com.sun.j3d.loaders.Scene;
import com.sun.j3d.loaders.Loader;
import com.sun.j3d.loaders.vrml97.node.Viewpoint;


/**
 * This class controls the basic operation of the mental imagery
 * representation and process within the DIVA computer model. As the name
 * implies (MindsEye), it acts as working memory to extract data from
 * long-term memory and process it.
 *
 * @see AnalogyEngine
 */
public class MindsEye extends VisualBuffer implements NOAInput, BlobDecoder {
	
	// the current SceneBlob being displayed (if we are displaying one!)
	public SceneBlob activeBlob_ = null;

	// Hubs that are used to organize visual memories
	public Hub IMAGE3D;	
		public Hub PRIMITIVE;
		public Hub DEFAULT3D;
		public Hub GROUP;
	public Hub MOTION;
	public Hub CONVERGE;
	public Hub PARALLEL;

	private final String[][] defaultModelNames_ = {	
		{	"SolarSystem.wrl", 					"solarsystem"	},
		{	"AluminumAtom_NoBehaviours.wrl",	"aluminumatom"	},
		{	"SimpleAtom.wrl",					"atom"			},
		{	"Arch1.wrl",						"arch1"			},
		{	"Arch2.wrl",						"arch2"			},
		{	"ConeArch.wrl", 					"conearch"		},
		{	"Snowman.wrl",						"snowman"		},
		{	"Stoneman.wrl",						"stoneman"		},
		{	"Van.wrl", 							"van"			},
		/*
		{	"Calf.wrl", 						"calf"			},
		{	"Chicken.wrl",						"chicken"		},
		{	"Trex.wrl",							"trex"			},
		{	"Farm.wrl",							"farm"			},
		{	"Horse.wrl",						"horse"			},
		{ 	"droid_PaulFlavin.wrl", 			"droid"			},
		{	"ellen_PaulFlavin.wrl",				"ellen"			},
		*/
		{	"castle.wrl",						"castle"		},
		{	"evilHead.wrl",						"head"			}
		};


	public Canvas3D getVisualBuffer() {
		// cast down to the visual buffer's parent
		return (Canvas3D)this;
	}

	public void setVerbose(boolean verbose) {
		graphDrawer_.setVerbose(verbose);
	}
	
	// this constructor requires a GraphicsConfiguration object to 
	// initialize the Canvas3D object used by the visual buffer. See
	// the note however in the VisualBuffer constructor.
	public MindsEye(GraphicsConfiguration config) {
		super(config);
		Control.setDecoder(this, Control.IMAGERY);
		
		// register Hubs that are inate to visual input
		IMAGE3D = new Hub("image3d"); 
			IMAGE3D.associate(NOA.IS, NOA.INPUT);
			PRIMITIVE = new Hub("primitive");
			PRIMITIVE.associate(NOA.IS, IMAGE3D);
			DEFAULT3D = new Hub("default3d");
			DEFAULT3D.associate(NOA.IS, IMAGE3D);
		MOTION = new Hub("motion");
		PARALLEL = new Hub("parallel");
			PARALLEL.associate(NOA.IS, MOTION);
		CONVERGE = new Hub("converge");
			CONVERGE.associate(NOA.IS, MOTION);

		registerPrimitiveObjects();
		registerVRMLDefaults();
    }

	/**
	 * This method implements the registerCommands() method from the 
	 * BlobDecoder interface. All objects that are capable of decoding Blobs
	 * from the associative memory structure (NOA), are likely to require
	 * their own user interface commands. Here are the commands required for
	 * visual memories.
	 */
	public void registerCommands() {
		// register imagery commands that can be entered on the 
		// command line interface. 
		Control.registerCommand(new MoveCommand(this));
		Control.registerCommand(new RotateCommand(this));
		Control.registerCommand(new ScaleCommand(this));
		Control.registerCommand(new LocateCommand(this));
		Control.registerCommand(new BoundsCommand(this));
		Control.registerCommand(new StartCommand(this));
		Control.registerCommand(new StopCommand(this));
		Control.registerCommand(new PrintSceneGraphCommand(this));
		Control.registerCommand(new MoveEyeCommand(this));
		Control.registerCommand(new AddObjectCommand(this));
		Control.registerCommand(new AddPathCommand(this));
		Control.registerCommand(new CompareCommand(this));
    }
	
	/**
	 * This methods loads a VRML file and returns the scene in Java3D
	 * format. This method should only be called by the MindsEye, since
	 * most of the time an interactive approach should be carried out by
	 * the user when loading VRML data. See the input(fileName) method.
	 * For example, many scenes contain named objects, and this method
	 * will just load the scene and nothing will be done with the named
	 * objects. The input method on the other hand calls this method, 
	 * but then goes through all the other objects and registers them
	 * with the NOA memory system.
	 *
	 * @param u the full name of the file containing the VRML scene.
	 */
	private VrmlScene loadVRML(String fileName) {
		VrmlScene 	scene = null;  // local var so VRML stuff will get gc'd
		long time = System.currentTimeMillis();
	
		// I had nothing but problems with URLs... however here is the code
		// I was trying to use...
		/*	
		Control.status("reading in " + fileName);
		URL url;
		try {
			url = new URL(fileName);
		} catch (MalformedURLException badURL) {
			Control.error("Malformed URL: " + badURL, badURL);
			return null;
		}
		*/

		try {
			scene = (VrmlScene)loader_.load(fileName);
		} catch (InvalidVRMLSyntaxException e) {
			Control.error("Invalid VRML Syntax: " + e);
			e.printStackTrace(System.err);
			return null;
		} catch (Exception e2) {
			Control.error("unknown error loading VRML file: " + fileName, e2);
			return null;
		}

		if (scene == null) { 
			Control.error("scene is null");
			return null; 
		}

		time = System.currentTimeMillis() - time;
		String moniker = fileName.substring(fileName.lastIndexOf('/')+1);
		Control.status("loaded " + moniker + " in " + time + " ms.");
			
		BranchGroup sceneGroup = scene.getSceneGroup();

		// Clean the scene to prepare to compile it?  This will keep the
		// scene from being pickable or collidable.
		// scene.cleanForCompile(sceneGroup);
		
		// prepare the scene for use by the minds eye and the visual 
		// buffer. Set all the capability bits as we would like them.
		J3DUtils.setBranchGroupCapabilities(sceneGroup);
		return scene;
	}

	/**
	 * This method implements the input() method from the NOAInput interface.
	 * It loads a VRML file and sets it up in the MindsEye. The user will be
	 * prompted to name the scene and any objects that contained within the 
	 * scene (objects that are DEF'ed in the VRML file.
	 */
    public void input(String fileName) {

		setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

		removeOldScene();

		VrmlScene scene = loadVRML(fileName);
		if (scene == null) {	// null will be returned if there was errors
			return;
		}
		BranchGroup sceneGroup = scene.getSceneGroup();

		// setting up file viewpoints modifies nodes in the Scene graph,
		// so we need to do this before we compile/add
		fileViewpoints = scene.getViewpoints(); 
		if (fileViewpoints.length > 0) {
			setupFileViewpoints();
		} 
		
		// display the scene - Note this makes the scene graph live !! 
		// No more setting capability bits after this point
		display(sceneGroup);

		if ((fileViewpoints.length > 0) && (initVp == FILE_VIEW)) {
			setFileViewpoint(0);
		} else {
			setObjViewpoint(OBJ_VIEW_PLUS_Z);
		}

		if (debug) {
			numTris = scene.getNumTris();
			Control.status("Scene contains " + numTris + " triangles");
		}
		registerSceneWithNOA(scene);
		
		setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
	}
	
	/**
	 * This method registers a VRML scene into the network of associations
	 * memory system (NOA). The user should be prompted to associate any
	 * objects that have been 'DEF'ed' in the VRML file. For each DEF'ed 
	 * object, a default viewpoint will be created as the perspective view
	 * for that subobject. <p>
	 *
	 * For example, when we imagine the leg of a dog,
	 * we don't bring to mind the whole dog, we are probably focused on the
	 * particular region of the dog containing the leg. The algorithm that
	 * zooms to subobjects (creates the perspective) needs some work still.
	 */
	void registerSceneWithNOA(Scene scene) {

		// The nodes that were 'DEF'ed in the VRML file
		Hashtable table = scene.getNamedObjects();

		String description = scene.getDescription();
		Control.status("Scene description is: " + description);

		// get the root node
		BranchGroup rootNode = scene.getSceneGroup();

		// in order to modify the capabilties of the scene, we have to 
		// detatch it from the active scene. So first we detach it,
		// then we set the capabilities, and then the root node is reattached
		detachRoot();
		J3DUtils.setCapabilities(rootNode);
		reattachRoot();
		
		// Get the original view - see the function getViewInfo()
		ViewInfo originalView = getViewInfo();
		
		// Prompt the user about a name for this visual blob
		String sceneName = Control.askQuestion("Enter a name for this scene");
		SceneBlob rootBlob = new SceneBlob(sceneName, this, rootNode);

		// this is a bit of hack for motions right now
		// see if we can clone the current scnene to create an animated 
		// duplicate. Note that to clone we have to unattach the root.
		detachRoot();
		Node rootClone = rootNode.cloneTree();
		reattachRoot();
		BranchGroup flyingGroup = createAnimatedSceneGraph(rootClone);
		SceneBlob motionBlob = new SceneBlob("flying_" + sceneName, this, 
		 	flyingGroup, getViewInfo());
		motionBlob.associate(NOA.HAS, rootBlob); 
		// end of hack

		sceneName = rootBlob.getName();
		NOA.markTime(rootBlob);
		rootBlob.associate(NOA.IS, IMAGE3D);
		Control.status("Added visual data for " + sceneName);
		
		// now check the 'DEF'ed nodes from this Vrml file
		// these are all the smaller named objects that are contained in 
		// the scene. For example, in a scene representing a dog, the 
		// nose and eyes might be DEF'ed as seperate objects

		Set keySet = table.keySet();
		Iterator iterator = keySet.iterator();

		while (iterator.hasNext()) {
			String name = (String)iterator.next();
			SceneGraphObject object = (SceneGraphObject)table.get(name);
			if (object == null) {
				Control.error("object named " + name + " is null");
			}
			if (object instanceof Node) {
				Node node = (Node)object;

				// add a temporary transparent sphere around the object 
				// so that the user can see what part of the scene this 
				// name corresponds to
				
				TransparencyAttributes transp = 
					new TransparencyAttributes(
						TransparencyAttributes.NICEST, 0.7f);
				Appearance app = new Appearance();
				app.setTransparencyAttributes(transp);
				Material material = new Material(
					new Color3f(1.0f,0.0f,0.0f),	// ambient color
					new Color3f(1.0f,0.0f,0.0f),	// emmissive color
					new Color3f(1.0f,0.0f,0.0f),	// diffuse color
					new Color3f(1.0f,0.0f,0.0f),	// specular color
					64.0f);							// shininess

				app.setMaterial(material);
				
				Bounds bounds = node.getBounds();
				Control.status("Bounds for " + name + " = " + bounds);
				
				Point3d center = new Point3d();
				double radius = J3DUtils.getRadiusAndCenter(node, center);
				TransformGroup tg = new TransformGroup();

				if (bounds instanceof BoundingSphere) {
					Sphere sphere = new Sphere((float)radius, app);
					tg.addChild(sphere);
				} else if (bounds instanceof BoundingBox) {
					BoundingBox boundBox = (BoundingBox)bounds;
					Point3d lower = new Point3d();
					boundBox.getLower(lower);
					Point3d upper = new Point3d();
					boundBox.getUpper(upper);
					Box box = new Box(	(float)Math.abs(upper.x - lower.x), 
										(float)Math.abs(upper.y - lower.y),
										(float)Math.abs(upper.z - lower.z), 
										app);
					tg.addChild(box);
				} else {
					Control.error("unknown bounding type");
				}

				Vector3d offset = new Vector3d(center);
				Transform3D Xform = new Transform3D();
				Xform.setTranslation(offset);
				tg.setTransform(Xform);
				BranchGroup bg = new BranchGroup();
				bg.setCapability(BranchGroup.ALLOW_DETACH);
				bg.addChild(tg);
				rootNode.addChild(bg);
				int index = rootNode.numChildren() - 1;

				// For each of the DEF'ed objects, determine a viewpoint
				// that looks at the object in relation to the whole scene.
				// For eg. if this object is the nose from the dog, then
				// create a viewpoint that looks at a close-up of the nose
				// with the rest of the dog in the background.

				Transform3D viewXform = new Transform3D();
				BoundingSphere sceneBounds = 
					(BoundingSphere)rootNode.getBounds();
				Point3d sceneCenter = new Point3d();
				sceneBounds.getCenter(sceneCenter);

				double distance = sceneCenter.distance(center); 

				// ensure that distance is positive
				if (distance < 0 ) { distance = -distance; }

				Point3d eyePoint = new Point3d(center);

				// ratio of distance to radius of the object
				eyePoint.scale(4*distance+6*radius);
				
				Vector3d frustrumUp = new Vector3d(0.0,1.0,0.0);
				viewXform.lookAt(eyePoint, center, frustrumUp);
				viewXform.invert();

				/*
				System.out.println("Trying to setup view to look at object");
				System.out.println("Eye point: " + eyePoint);
				System.out.println("Obj point: " + center);
				*/

				setViewOrientTransform(viewXform);
				
				// Ask the user whether or not this DEF'ed object should 
				// be added to the semantic network (if it is an important
				// part of the scene it probably should.

				boolean answer = Control.askBooleanQuestion(
					"Add \"" + name + "\" to memory?");
				if (answer == true) {
					name = Control.askQuestion(
						"Enter name for object in the sphere", name);
					// keep the viewpoint but remove the transparent sphere
					rootNode.removeChild(index);
					SceneSubsetBlob subBlob = new SceneSubsetBlob(	
						name, 
						this, 
						rootNode, 
						getViewInfo(),
						node);
					rootBlob.associate(NOA.HAS, subBlob);
				} else {
					rootNode.removeChild(index);
					continue;
				}
			} else {
				// object is a NodeComponent
				Control.status(
					"Cannot handle DEF'ed node components yet");
			}
		}
		setViewInfo(originalView);
		Control.viewMemory(sceneName);	// make this scene active in 
										// long-term memory
	}
	
	/**
	 * This method overloads the BlobDecoder interface decode().
	 * It extracts the SceneGraph from a Blob containing scene graph data.
	 * (Blobs that contains Java3D scene graphs are called SceneBlobs).
	 */
	public void decode(Blob blob) {
		Control.activateDecoder(this);

		if (!(blob instanceof SceneBlob)) {
			Control.error("request made to decode a non-visual blob");
			return;
		}
		SceneBlob sceneBlob = (SceneBlob)blob;
		
		// Get the root node and the viewpoint
		BranchGroup root = null;
		if (sceneBlob instanceof SceneSubsetBlob) {
			SceneSubsetBlob subBlob = (SceneSubsetBlob)sceneBlob;
			root = (BranchGroup)subBlob.getRoot();
		} else {
			root = (BranchGroup)sceneBlob.getNode();
		}

		if (root == null) {
			Control.error("Blob contains no visual memory");
			return;
		}
		activeBlob_ = sceneBlob;
		removeOldScene();
		
		// use the default view point for this object
		setViewInfo(activeBlob_.getViewInfo());
		
		display(root);
	}
	
	/**
	 * This is a work in progress. It is supposed to set the current 
	 * view as the default view for the object that we are currently 
	 * looking at.
	 */
	void markCurrentView() {
		if (activeBlob_ == null) {
			return;
		}
		activeBlob_.setViewInfo(getViewInfo());
	}

	/**
	 * Methods for animated scenes. Don't do much yet.
	 */
	public void start() {
	}
	public void stop() {
	}
	public void exit() {
		// nothing to do here yet!
	}
	
	/** 
	 * When the visual buffer updates it's behavior info this method
	 * will be called.
	 */
	protected void addBehaviors(BranchGroup behaviorTree) {
		KeyBehavior kb = new KeyBehavior(this);
		kb.setSchedulingBounds(browserBounds);
		behaviorTree.addChild(kb);
	}
	
	// U T I L I T Y  M E T H O D S
	//
	// Note that these methods are not contained in VisualBuffer.java
	// because they contain interfaces to SceneBlobs etc.. They are not
	// pure Java3D manipulations.
	
	
	/**
	 * This is a utility method, that given the name of a visual concept,
	 * returns the highest TransformGroup contained in that scene (the 
	 * TransformGroup closest to the root of the scene, usually directly 
	 * below the first BranchGroup. 
	 */
	TransformGroup findTransformGroup(String hubName) {
		SceneBlob blob = getSceneBlob(hubName);
		Node node = blob.getNode();
		return findTransformGroup(node);
	}
	
	TransformGroup findTransformGroup(Node node) {
		detachRoot();
		TransformGroup tg = J3DUtils.findTransformGroup(node);
		reattachRoot();
		return tg;
	}
	
	/**
	 * This utility method returns the visual concept (SceneBlob) that 
	 * corresponds to the given name in long-term memory. If the name does
	 * not exist in long-term memory, then null is returned.
	 */
	SceneBlob getSceneBlob(String name) {
		Hub hub = NOA.get(name);
		if (hub == null) {
			return null;
		} else if (!(hub instanceof SceneBlob)) {
			Control.error(name + " is not a visual concept");
			return null;
		}
		return (SceneBlob)hub;
	}
	
	/** 
	 * This method adds one visual concept to another. For example, the "cow"
	 * concept might be added to the "farm" concept, with the result being
	 * that "farm" now contains visual data for the cow. 
	 * <p>
	 * TODO: There is currently not much intelligence in terms of where the 
	 * visual concept is placed in the other. The cow will not necessarily be 
	 * placed in the barn, (it might end up somewhere out in the field or 
	 * worse yet, hovering in the air).
	 *
	 * TODO: The user should be prompted to rename the new combined scene.
	 * This would help keep the original concepts uncluttered, and the user 
	 * could generate new concepts by combining other concepts.
	 */
	void addObject(String objToAddName, String sceneName) {
		SceneBlob toAddHub = getSceneBlob(objToAddName);
		SceneBlob sceneHub = getSceneBlob(sceneName);
		if (sceneHub instanceof SceneSubsetBlob) {
			Control.error("cannot yet add to a subset blob");
			return;
		}
		BranchGroup rootToAdd = (BranchGroup)toAddHub.getNode().cloneTree();
		BranchGroup root = (BranchGroup)sceneHub.getNode();
		root.addChild(rootToAdd);
	}

	/**
	 * This method adds an interpolator path to a named scene. 
	 * @see AddPathCommand
	 */
	public void addPath(String objectName, 
						float knots[],
						Point3f[] positions) {

		TransformGroup tgroup = findTransformGroup(objectName);
		PositionPathInterpolator pathInterpolator = 
			new PositionPathInterpolator(
			new Alpha(),
			tgroup,
			new Transform3D(),
			knots,
			positions);
		detachRoot();
	 	BranchGroup parent = (BranchGroup)tgroup.getParent();
		parent.addChild(pathInterpolator);
		reattachRoot();
	}
	
	/**
	 * This method registers all the 'primitive' 3-D objects used in the 
	 * model of mental imagery (box, sphere, cylinder etc...
	 */
	private void registerPrimitiveObjects() {
		Control.status("--- Registering primitive object types ---");
		Control.status("creating: " + newBox());
		Control.status("creating: " + newSphere());
		Control.status("creating: " + newCylinder());
		Control.status("creating: " + newMovingBox());
	}
	
	/**
	 * Loads all the default VRML models used in the mental imagery models.
	 * This method is called by the MindsEye when the program starts so that 
	 * a number of 'visual concepts' will be present in the model that can 
	 * be examined. This is a bit of a hack that results because Java3D data
	 * structures cannot currently be persisted (Java3D limitation). If they
	 * could be persisted, then models could be loaded manually each time the
	 * program was used, and then the current long-term memory concepts could 
	 * be automatically saved and reloaded the next time the program was run. 
	 * Unfortunately, loaded scenes cannot currently be saved, so to get
	 * around this a number of defaults are loaded at start-up (examples:
	 * solar system, atom, van, calf etc...).
	 */
	private void registerVRMLDefaults() {
		Control.status("--- Registering default VRML models ---");
		VrmlScene scene = null;
		BranchGroup group = null;
		TransformGroup transgroup = null;
		SceneBlob blob = null;

		String baseFile =
			"./cogsci/imagery/models/";

		// load the entire list of models (from defaultModelNames_)
		for (int i=0; i<defaultModelNames_.length; i++) {
			scene = loadVRML(baseFile + defaultModelNames_[i][0]);
			if (scene != null) {	// null will be returned if there 
									// was an error loading the model
				group = scene.getSceneGroup();
				blob = new SceneBlob(	defaultModelNames_[i][1], this, 
										group, getViewInfo());
				blob.associate(NOA.IS, DEFAULT3D);
				Control.status("creating: " + blob.getName());
			}
		}

		// Some models are manually configured or loaded from Java3D source
		
		// Create a gear
		group = new BranchGroup();
		transgroup = new SpurGearThinBody(60, 1.0f, 0.1f, 0.05f, 0.05f, 0.3f);
		group.addChild(transgroup);
		if (group != null) {
			blob = new SceneBlob("gear", this, group);
			Control.status("creating: " + blob.getName());
			blob.associate(NOA.IS, DEFAULT3D);
		}


		// Create the gearbox example
		BranchGroup gearboxbg = GearBox.createGearBox(64); // # teeth
		if (gearboxbg != null) {
			SceneBlob gearboxBlob = new SceneBlob("gearbox", this, gearboxbg);
			Control.status("creating: " + gearboxBlob.getName());
			gearboxBlob.associate(NOA.IS, DEFAULT3D);
			gearboxBlob.associate(NOA.HAS, blob); 
		}

		// Tumor problem - siege

		SceneBlob castleBlob = getSceneBlob("castle");
		group = (BranchGroup)castleBlob.getNode();
		BranchGroup castle = (BranchGroup)group.cloneTree();

		// get the graph we are using for soldiers

		SceneBlob sphere = getSceneBlob("sphere");
		BranchGroup soldier = (BranchGroup)sphere.getNode();
		SceneBlob soldierBlob = new SceneBlob("soldier", this, soldier);
		soldierBlob.associate(NOA.IS, DEFAULT3D);
		Control.status("creating: " + soldierBlob.getName());
		
		BranchGroup siege = 
			createConvergeGraph(castle, soldier, 8, 6, 100.0d, 2.0, 10000);

		if (siege != null) {	// null will be returned if there was ... 
			blob = new SceneBlob("siege", this, siege);
			blob.associate(NOA.IS, DEFAULT3D);
			blob.associate(NOA.HAS, castleBlob);
			blob.associate(NOA.HAS, soldierBlob);
			Control.status("creating: " + blob.getName());
		}

		// Tumor Problem - tumor (head and radiation beams)

		// load the graph for the head from a file

		SceneBlob headBlob = getSceneBlob("head");
		group = (BranchGroup)headBlob.getNode();
		BranchGroup head = (BranchGroup)group.cloneTree();

		// create the graph we are using for the rays

		SceneBlob cylinder = getSceneBlob("cylinder");
		BranchGroup ray = (BranchGroup)(cylinder.getNode()).cloneTree();
		SceneBlob rayBlob = new SceneBlob("ray", this, ray);

		// Rotate the cylinder to look more like a ray
		Transform3D transform = rayBlob.getTransform();
		transform.rotX(Math.PI/2.0d);
		transform.setScale(0.05);
		rayBlob.setTransform(transform);

		rayBlob.associate(NOA.IS, DEFAULT3D);
		Control.status("creating: " + rayBlob.getName());

		// now put the head and the ray together to create the tumor problem
		
		BranchGroup tumorProblem = 
			createConvergeGraph(head, ray, 5, 6, 20.0d, 0.5, 8000);
		
		if (tumorProblem != null) {	// null will be returned if there was ... 
			blob = new SceneBlob("tumorproblem", this, tumorProblem);
			blob.associate(NOA.IS, DEFAULT3D);
			blob.associate(NOA.HAS, headBlob);
			blob.associate(NOA.HAS, rayBlob);
			Control.status("creating: " + blob.getName());
		}
	}

	/**
	 * Allocates a Java3D sphere and registers it with NOA (long-term memory).
	 * The name returned should be "sphere" however, if there is already a
	 * "sphere" concept, then a different name will be returned (eg.
	 * sphere_1). 
	 */
	private String newSphere() {
		Sphere sphere = new Sphere();
		Appearance app = new Appearance();
		Material material = new Material(
			new Color3f(0.9f,0.9f,0.1f),	// ambient color
			new Color3f(0.9f,0.9f,0.1f),	// emmissive color
			new Color3f(0.9f,0.9f,0.1f),	// diffuse color
			new Color3f(0.9f,0.9f,0.1f),	// specular color
			64.0f);							// shininess

		app.setMaterial(material);
		sphere.setAppearance(app);
		J3DUtils.setNodeCapabilities((Node)sphere);
		BranchGroup group = new BranchGroup();
		J3DUtils.setBranchGroupCapabilities(group);
		TransformGroup transform = new TransformGroup();
		J3DUtils.setTransformGroupCapabilities(transform);
		group.addChild(transform);
		transform.addChild(sphere);

		SceneBlob sphereBlob = 
			new SceneBlob("sphere", this, group);
		sphereBlob.associate(NOA.IS, PRIMITIVE);
		return sphereBlob.getName();
	}
	
	/** 
	 * Same as newSphere except for a cylinder. 
	 * @see newSphere
	 */
	private String newCylinder() {
		Cylinder cylinder = new Cylinder(0.5f, 0.5f);
		Appearance app = new Appearance();
		Material material = new Material(
			new Color3f(1.0f,1.0f,1.0f),	// ambient color
			new Color3f(1.0f,1.0f,1.0f),	// emmissive color
			new Color3f(1.0f,1.0f,1.0f),	// diffuse color
			new Color3f(1.0f,1.0f,1.0f),	// specular color
			64.0f);							// shininess

		app.setMaterial(material);
		cylinder.setAppearance(app);
		J3DUtils.setNodeCapabilities((Node)cylinder);
		BranchGroup group = new BranchGroup();
		J3DUtils.setBranchGroupCapabilities(group);
		TransformGroup transform = new TransformGroup();
		J3DUtils.setTransformGroupCapabilities(transform);
		group.addChild(transform);
		transform.addChild(cylinder);

		SceneBlob cylinderBlob = 
			new SceneBlob("cylinder", this, group);
		cylinderBlob.associate(NOA.IS, PRIMITIVE);
		return cylinderBlob.getName();
	}

	/** 
	 * Same as newSphere except for a box. 
	 * @see newSphere
	 */
	private String newBox() {
		Box box = new Box();
		TransparencyAttributes transp = 
			new TransparencyAttributes(
				TransparencyAttributes.NICEST, 0.9f);
		Appearance app = new Appearance();
		app.setTransparencyAttributes(transp);
		Material material = new Material(
			new Color3f(1.0f,1.0f,0.0f),	// ambient color
			new Color3f(1.0f,1.0f,0.0f),	// emmissive color
			new Color3f(1.0f,1.0f,0.0f),	// diffuse color
			new Color3f(1.0f,1.0f,0.0f),	// specular color
			64.0f);							// shininess

		app.setMaterial(material);
		box.setAppearance(app);
		J3DUtils.setNodeCapabilities((Node)box);
		BranchGroup group = new BranchGroup();
		J3DUtils.setBranchGroupCapabilities(group);
		TransformGroup transform = new TransformGroup();
		J3DUtils.setTransformGroupCapabilities(transform);
		group.addChild(transform);
		transform.addChild(box);
		
		SceneBlob boxBlob = new SceneBlob("box", this, group);
		boxBlob.associate(NOA.IS, PRIMITIVE);
		return boxBlob.getName();
	}

	/** 
	 * Same as newSphere except this is an animated box that demonstrates 
	 * a simple coloured box that moves around. 
	 * @see newSphere
	 */
	private String newMovingBox() {
		BranchGroup movingBox = createAnimatedSceneGraph(new ColorCube(0.4));
		SceneBlob movingBoxBlob = new SceneBlob("movingBox", this, movingBox);
		movingBoxBlob.associate(NOA.IS, PRIMITIVE);
		return movingBoxBlob.getName();
	}
	
	/**
	 * Given the basic 'pieces', this method creates the convergence motion to 
	 * test the dynamic visual analogy algorithm. Note that this is a hack
	 * that results because all of the current Java3D loaders (including the
	 * VRML loader) cannot load scenes that contain motion. As a result of
	 * this, the pieces of the scene have to be loaded from VRML models, and
	 * then the Java3D motions have to be added. This motion requires:<p>
	 *
	 * 1 - 	Center - a BranchGroup representing an object at the center of the 
	 * 		scene<br>
	 * 2 -	Converger - a BranchGroup representing the converging object
	 * 		(assumes that all the converging objects are the same - eg. 
	 * 		soldier in the siege solution to the tumor problem). <br>
	 * 3 - 	the number of converges desired (a seperate subtree will be 
	 * 		cloned for each converger.<br>
	 * 4 - 	the radius at which the convergers start their inward motion<br>
	 * 5 - 	the height that the convergers start at<br>
	 * 6 - 	the time it takes for the convergers to move from the outside to 
	 * 		the inside.
	 */
	public BranchGroup createConvergeGraph(	BranchGroup center,
											BranchGroup converger,
											int numConvergers,
											int dudNumber,
											double radius,
											double height,
											int cycleTime) {

		// Create the root of the branch graph
		BranchGroup objRoot = new BranchGroup();
		
		TransformGroup centerTransform = new TransformGroup();
		centerTransform.addChild(center);
		objRoot.addChild(centerTransform);
		
		// -1 indicates alpha loops indefinately, second parameter is duration
		// of cycle in milliseconds
        Alpha alpha = new Alpha(-1, cycleTime);

        float[] knots = {0.0f, 0.98f, 1.0f};

		Transform3D axisOfRotPos = new Transform3D();
		//AxisAngle4f axis = new AxisAngle4f(1.0f,0.0f,0.0f,0.0f);
		//axisOfRotPos.set(axis);

		for (int i=0; i<numConvergers; i++) {
			TransformGroup target = new TransformGroup();
			Point3f[] positions = new Point3f[3];
			target.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);

			double angle = ((2.0d*Math.PI)/numConvergers)*i;
			double x = radius*Math.cos(angle);
			double y = radius*Math.sin(angle);
			
			positions[0]= new Point3f((float)x, (float)height, (float)y);
			positions[1]= new Point3f(0.0f, 0.0f, 0.0f);
			positions[2]= positions[0];

			if (i==dudNumber) {
				positions[1]= new Point3f((float)x, 0.0f, (float)y);
			}

			PositionPathInterpolator posPath = new PositionPathInterpolator(
					alpha, target, axisOfRotPos, knots, positions);
			posPath.setSchedulingBounds(new BoundingSphere());

			BranchGroup convergerGroup = new BranchGroup();
			convergerGroup.addChild(target);
			convergerGroup.addChild(posPath);
			target.addChild(converger.cloneTree());
			objRoot.addChild(convergerGroup);
		}
        return objRoot;
	}
	
	/**
	 * Given a visual object, this method adds a behaviour that makes it 
	 * move around a fixed path demonstrating the animated potential of 
	 * the working memory.
	 */
    public BranchGroup createAnimatedSceneGraph(Node node) {

		// Create the root of the branch graph
		BranchGroup objRoot = new BranchGroup();

        Alpha alpha = new Alpha(-1, 10000);
        TransformGroup target = new TransformGroup();
        Transform3D axisOfRotPos = new Transform3D();
        float[] knots = {0.0f, 0.1f, 0.2f, 0.3f, 0.4f, 0.6f, 0.8f, 0.9f, 1.0f};
        Quat4f[] quats = new Quat4f[9];
        Point3f[] positions = new Point3f[9];

        target.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);

        AxisAngle4f axis = new AxisAngle4f(1.0f,0.0f,0.0f,0.0f);
        axisOfRotPos.set(axis);

        quats[0] = new Quat4f(0.0f, 1.0f, 1.0f, 0.0f);
        quats[1] = new Quat4f(1.0f, 0.0f, 0.0f, 0.0f);
        quats[2] = new Quat4f(0.0f, 1.0f, 0.0f, 0.0f);
        quats[3] = new Quat4f(0.0f, 1.0f, 1.0f, 0.0f);
        quats[4] = new Quat4f(0.0f, 0.0f, 1.0f, 0.0f);
        quats[5] = new Quat4f(0.0f, 1.0f, 1.0f, 0.0f);
        quats[6] = new Quat4f(1.0f, 1.0f, 0.0f, 0.0f);
        quats[7] = new Quat4f(1.0f, 0.0f, 0.0f, 0.0f);
        quats[8] = quats[0];

        positions[0]= new Point3f(  0.0f,  0.0f, -1.0f);
        positions[1]= new Point3f(  1.0f, -1.0f, -2.0f);
        positions[2]= new Point3f( -1.0f,  1.0f, -3.0f);
        positions[3]= new Point3f(  2.0f,  0.0f, -4.0f);
        positions[4]= new Point3f( -2.0f, -1.0f, -5.0f);
        positions[5]= new Point3f(  3.0f,  1.0f, -6.0f);
        positions[6]= new Point3f( -3.0f,  0.0f, -7.0f);
        positions[7]= new Point3f(  2.0f, -1.0f, -4.0f);
        positions[8]= positions[0];


        RotPosPathInterpolator rotPosPath = new RotPosPathInterpolator(
                alpha, target, axisOfRotPos, knots, quats, positions);
        rotPosPath.setSchedulingBounds(new BoundingSphere());

        objRoot.addChild(target);
        objRoot.addChild(rotPosPath);
        target.addChild(node);

        PointArray point_geom = new PointArray(9, GeometryArray.COORDINATES);
        point_geom.setCoordinates(0, positions);
        Appearance points_appear = new Appearance();
        ColoringAttributes points_coloring = new ColoringAttributes();
        points_coloring.setColor(1.0f, 0.0f, 0.0f);
        points_appear.setColoringAttributes(points_coloring);
        PointAttributes points_points = new PointAttributes(4.0f, true);
        points_appear.setPointAttributes(points_points);
        Shape3D points = new Shape3D(point_geom, points_appear);
        objRoot.addChild(points);

		// for the MindsEye we don't compile - you can't play with the 
		// scene graph after it's been compiled
        // objRoot.compile();

        return objRoot;
    } // end of CreateSceneGraph method of RotPosPathApp

	/**
	 * This is the controlling method of the visual analogy algorithm 
	 * to compare the static and dynamic content between two scenes. 
	 */
	final void compare(	SceneBlob scene1, SceneBlob scene2,
						int numberToReturn, int weightingScheme) {
		
		setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

		// first compare the semantic associations (NOA)
		System.out.println(
			"--- Comparing semantic associations for '" + 
			scene1.getName() + "' and '" + scene2.getName() + "' ---");

		Vector hubs = new Vector(2);
		hubs.add(scene1);
		hubs.add(scene2);

		Iterator iterator = NOA.activate(hubs.iterator());
		if (iterator == null) { 
			Control.error("spreading activation returned null");
		}
		
		String printOut = new String(
			"Most active hubs listed from highest to lowest:");
		String mostActive = "";

		for (int i=0; i<5 && iterator.hasNext(); i++) {
			Hub next = (Hub)iterator.next();
			// get the most active name to update the viewer
			if (i==0) { 
				mostActive = next.getName(); 
			}
			printOut += 
				"\n\t" + next.getName() + " (" + next.getActivation() + ")";
		}

		// set the most active concept in the HubViewer
		Control.viewMemory(mostActive);
		Control.status(printOut);
		
		// now compare the static and dynamic content of the scenes
		System.out.println(
			"--- Comparing static and dynamic content for '" + 
			scene1.getName() + "' and '" + scene2.getName() + "' ---");

		BranchGroup root1 = (BranchGroup)scene1.getNode();
		BranchGroup root2 = (BranchGroup)scene2.getNode();
		
		AnalogyEngine analogyEngine = new AnalogyEngine();

		Vector topMappings = analogyEngine.compare(	root1, root2, 
													numberToReturn,
													weightingScheme);

		setDrawGraph(true);
		removeOldScene();
		display(root1, root2, topMappings);
		setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
	}
}
