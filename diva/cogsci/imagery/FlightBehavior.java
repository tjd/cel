package cogsci.imagery;

// adopted from the demo code in:
// package com.sun.j3d.utils.ui;


import javax.media.j3d.*;
import java.util.*;
import java.awt.Event;
import java.awt.Point;
import java.awt.AWTEvent;
import java.awt.Dimension;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.WindowEvent;
import javax.vecmath.*;

/** 
 * Generalized Behavior Flying through a Universe.
 *
 * Either control via the Keyboard with left & right arrows producing yaw
 * and up/down keys producing pitch (',' and '.' roll).
 *
 * OR use press the mouse button and drag mouse for pitch and yaw
 * 
 * In both cases velocity is controlled using keys A and Z to increase and
 * decrease respectively.  Hit Space to stop.
 *
 * The mouse motion is proportional to the position of the mouse press the
 * C key to center the view
 *
 * If the shift key is held down while the mouse is moved then their will
 * be no effect on the behavior. This allows the user to move the mouse
 * out of the window without interacting with the 3D environment
 *
 * @version @(#)FlightBehavior.java 1.16.2.1 98/03/03 21:53:53
 * @author  Paul Byrne
*/

public class FlightBehavior extends Behavior implements ComponentListener {

    TransformGroup tgroup;
    WakeupCriterion criterion[] = { 
    		new WakeupOnAWTEvent( Event.KEY_PRESS ),
    		new WakeupOnAWTEvent( Event.KEY_RELEASE ),
		new WakeupOnAWTEvent( MouseEvent.MOUSE_PRESSED ),
		new WakeupOnAWTEvent( MouseEvent.MOUSE_DRAGGED ),
		new WakeupOnAWTEvent( MouseEvent.MOUSE_RELEASED ),
		new WakeupOnAWTEvent( ComponentEvent.COMPONENT_RESIZED ),
		new WakeupOnAWTEvent( ComponentEvent.COMPONENT_SHOWN ),
		new WakeupOnAWTEvent( WindowEvent.WINDOW_ACTIVATED ),
		new WakeupOnElapsedTime( 10 )
		};

    WakeupCondition conditions = new WakeupOr( criterion );

    double angleRate = degreesToRadians(0.1); 	   // radians/second per pixel 
    double acceleration=0.05;
    double keyAngleRate = degreesToRadians(20.0);  // radians/second 

    double rollAngle=0.0;
    double pitchAngle=0.0;
    double yawAngle=0.0;
    double velocity=0.0;
    double rollAngleDelta=0.0;
    double pitchAngleDelta=0.0;
    double yawAngleDelta=0.0;
    double lastUpdate = 0.0;

    Matrix4d currentMatrix;
    Transform3D currentView;		// Orientation and Translation
    Transform3D currentOrient;		// Orientation
    Vector3d currentPosn;		// Translation

    Point mousePosn;
    Point mouseBase;

    Matrix4d startOrient;
    double startX;
    double startY;
    double startZ;
    boolean debug;

    private Canvas3D canvas;		// Keep canvas so we can get Size
    private Dimension canvasSize;

    private boolean mousePressed;

    private MotionNotifierInterface notifyMe=null;

    public FlightBehavior( TransformGroup t, Canvas3D canvas) {
	this( 0,0,0, 0,0,0, t, canvas );
    }

    /** 
     *  Sets initial viewer position and direction of view to values provided
    *
    */
    public FlightBehavior( double x, double y, double z, 
			    double roll, double pitch, double yaw, 
			    TransformGroup t, Canvas3D canvas ) {
	super();
	debug = false;
	startX = x;
	startY = y;
	startZ = z;
	this.currentPosn = new Vector3d( x,y,z );

	rollRot.rotZ( roll );
	pitchRot.rotX( pitch);
	yawRot.rotY( yaw);

	startOrient = new Matrix4d();
	startOrient.setIdentity();
	startOrient.mul(rollRot);
	startOrient.mul(yawRot);

	this.canvas = canvas;
	canvas.addComponentListener(this);
	this.tgroup = t;
	currentMatrix = new Matrix4d();
    }

    public void notifyMe( MotionNotifierInterface not ) {
	this.notifyMe = not;
    }

    public void initialize() {
	initialize(new BoundingSphere( 
		new Point3d( 0.0, 0.0, 0.0 ), Double.MAX_VALUE ) );
    }
    public void initialize( BoundingSphere bounds ) {
	Transform3D initTransform = new Transform3D();
	Vector3d startPosn = new Vector3d();

	wakeupOn( conditions );
	setSchedulingBounds( bounds );

	// Setup initial Orientation of View
	currentOrient = new Transform3D( startOrient );

	currentView = new Transform3D( );
	currentView.setTranslation( currentPosn );
	currentView.mul( currentOrient );

	tgroup.setTransform( currentView );

	mouseBase = new Point();
	setCanvasSize();
    }

    public void processStimulus( Enumeration criteria ) {
	WakeupCriterion wakeup;
	AWTEvent[] evt=null;
	boolean timer=false;

	mousePressed = false;

	while( criteria.hasMoreElements() ) {
	    wakeup = (WakeupCriterion)criteria.nextElement();
	    if (wakeup instanceof WakeupOnAWTEvent) {
		 evt=((WakeupOnAWTEvent)wakeup).getAWTEvent();
	    }
	    if (wakeup instanceof WakeupOnElapsedTime) {
		timer=true;
	    }

	    if (evt!=null) {
		for(int i=0; i<evt.length; i++) {
		    if (evt[i] instanceof KeyEvent)  {
			processKeyEvent( (KeyEvent)evt[i] );
		    }
		    if (evt[i] instanceof MouseEvent)  {
			processMouseEvent( (MouseEvent)evt[i] );
		    }
		    if  (evt[i] instanceof ComponentEvent || 
			      evt[i] instanceof WindowEvent)  {
			setCanvasSize();
		    }
		}
	    }

	    if (timer) {	// Update due to timer event
		update();

		if (notifyMe!=null) {
		    currentView.get( currentMatrix );
		    notifyMe.viewMoved( currentMatrix );
		}
	    }
	}
	wakeupOn( conditions );
    }

    private void update() {
	calcTransform();
	tgroup.setTransform( currentView  );
    }

    public void processMouseEvent( MouseEvent evt ) {
	double x, y;
	int id = evt.getID();
	
	if (id == MouseEvent.MOUSE_PRESSED) {
	    mouseBase = evt.getPoint();
	    mousePressed = true;
	    if (notifyMe != null) {
		notifyMe.buttonPressed(id);
	    }
	} else if (id ==  MouseEvent.MOUSE_DRAGGED) {
	    mousePosn = evt.getPoint();

	    x = mousePosn.x - mouseBase.x;
	    y = mousePosn.y - mouseBase.y;


	    yawAngleDelta = -x * angleRate;
	    pitchAngleDelta = y * angleRate;
	    if (debug) {
		System.out.println("Mouse offset: " + x + ", " + y);
		System.out.println("Angle rate: " +radiansToDegrees(angleRate));
		System.out.println("Yaw,pitch deltas: " + 
			radiansToDegrees(yawAngleDelta) + "," + 
			radiansToDegrees(pitchAngleDelta));
	    }
	} else if (id == MouseEvent.MOUSE_RELEASED) {
	    mousePressed = false;
	    yawAngleDelta = 0.0;
	    pitchAngleDelta = 0.0;
	} else {
	    System.out.println("Unexpected mouse event: " + evt);
	}
	update();
    }

    private void processKeyEvent( KeyEvent evt ) {
	int key = evt.getKeyCode();
	if (evt.getID() == KeyEvent.KEY_RELEASED) {
	    if (debug) {
		System.out.println("Key released: " + KeyEvent.getKeyText(key));
	    }
	    pitchAngleDelta = 0.0;
	    rollAngleDelta = 0.0;
	    yawAngleDelta = 0.0;
	}  else {
	    if (debug) {
		System.out.println("Key pressed: " + KeyEvent.getKeyText(key));
	    }
	    switch (key) {
	      case KeyEvent.VK_UP:
		pitchAngleDelta -= keyAngleRate;
		break;
	      case KeyEvent.VK_DOWN:
		pitchAngleDelta += keyAngleRate;
		break;
	      case KeyEvent.VK_LEFT:
		yawAngleDelta += keyAngleRate;
		break;
	      case KeyEvent.VK_RIGHT:
		yawAngleDelta -= keyAngleRate;
		break;
	      case KeyEvent.VK_A:
		velocity -= acceleration;
		if (notifyMe!=null)
		    notifyMe.velocityChanged( velocity );
		break;
	      case KeyEvent.VK_Z:
		velocity += acceleration;
		if (notifyMe!=null)
		    notifyMe.velocityChanged( velocity );
		break;
	      case KeyEvent.VK_PERIOD:
		rollAngleDelta = keyAngleRate;
		break;
	      case KeyEvent.VK_COMMA:
		rollAngleDelta = -keyAngleRate;
		break;
	      case KeyEvent.VK_C:
		rollAngle=0.0;
		pitchAngle=0.0;
		yawAngle=0.0;
		pitchAngleDelta=0.0;
		rollAngleDelta=0.0;
		yawAngleDelta=0.0;
		velocity=0;
		currentPosn = new Vector3d( startX, startY, startZ );
		currentOrient.setIdentity();
		break;
	      case KeyEvent.VK_H:
		rollAngle=0.0;
		pitchAngle=0.0;
		yawAngle=0.0;
		pitchAngleDelta=0.0;
		rollAngleDelta=0.0;
		yawAngleDelta=0.0;
		velocity=0;
		break;
	      case KeyEvent.VK_SPACE:
		velocity = 0.0;
		if (notifyMe!=null)
		    notifyMe.velocityChanged( velocity );
		break;
	    }

	}
	update();
    }

    private final Matrix4d rot = new Matrix4d();
    private final Matrix4d tmpDirection = new Matrix4d();
    private final Matrix4d zComp = new Matrix4d();// Should be a vector but 
						  // mul not implemented yet
    private final Vector3d newDirection = new Vector3d();
    private final Transform3D velocityTrans = new Transform3D();
    private final Matrix4d rollRot = new Matrix4d();
    private final Matrix4d pitchRot = new Matrix4d();
    private final Matrix4d yawRot = new Matrix4d();
    private final Vector3d zero = new Vector3d();

    private double degreesToRadians(double angleDegrees) {
        return 2 * Math.PI * angleDegrees / 360.0; 
    }

    private double radiansToDegrees(double angleRadians) {
        return 360.0 * angleRadians / 2 * Math.PI;
    }

    private void setCanvasSize() {
	canvasSize = canvas.getSize();
    }

    private void calcTransform() {

	double curTime = System.currentTimeMillis() / 1000.0;
	if (lastUpdate > 0.0) {
	    double timeDelta = curTime - lastUpdate;

	    zComp.setRow(2, 0,0,1,0 );

	    if (debug) {
		if ((pitchAngleDelta > 0.0) ||
		    (yawAngleDelta > 0.0) ||
		    (rollAngleDelta > 0.0))  {
		    System.out.println("Time delta is " + timeDelta);
		    System.out.println("Yaw delta is " + 
			    radiansToDegrees(yawAngleDelta * timeDelta));
		    System.out.println("Pitch delta is " + 
			    radiansToDegrees(pitchAngleDelta * timeDelta));
		    System.out.println("Roll delta is " + 
			    radiansToDegrees(rollAngleDelta * timeDelta));
		}
	    }
	    pitchAngle += pitchAngleDelta * timeDelta;
	    rollAngle +=  rollAngleDelta * timeDelta;
	    yawAngle +=   yawAngleDelta * timeDelta;

	    if (pitchAngle<0) pitchAngle = Math.PI*2 - pitchAngle;
	    else if (pitchAngle>Math.PI*2) pitchAngle = pitchAngle - Math.PI*2;
	    if (rollAngle<0) rollAngle = Math.PI*2 - rollAngle;
	    else if (rollAngle>Math.PI*2) rollAngle = rollAngle - Math.PI*2;
	    if (yawAngle<0) yawAngle = Math.PI*2 - yawAngle;
	    else if (yawAngle>Math.PI*2) yawAngle = yawAngle - Math.PI*2;

	    rollRot.rotZ( rollAngleDelta * timeDelta );
	    pitchRot.rotX( pitchAngleDelta * timeDelta );
	    yawRot.rotY( yawAngleDelta * timeDelta );

	    currentOrient.get( rot );

	    velocityTrans.set( velocity * timeDelta );
	    
	    tmpDirection.mul( rot, zComp );
	    newDirection.x = tmpDirection.m02;
	    newDirection.y = tmpDirection.m12;
	    newDirection.z = tmpDirection.m22;

	    velocityTrans.transform( newDirection );

	    currentPosn.add( newDirection );

	    rot.mul( rollRot );
	    rot.mul( pitchRot );
	    rot.mul( yawRot );

	    currentOrient.set( rot );

	    currentView.set( rot );
	    currentView.setTranslation( currentPosn );


	    if (false) {
		System.out.println("Roll="+rollAngle+" Pitch="+pitchAngle+
		    " Velocity="+velocity);
		System.out.println(" RollDelta="+rollAngleDelta+
		    " PitchDelta="+pitchAngleDelta+" Coords="+currentPosn);
	    }
	}
	lastUpdate = curTime;
    }

    public void setPosition( Vector3d pos ) {
	currentPosn.x = pos.x;
	currentPosn.y = pos.y;
	currentPosn.z = pos.z;

	calcTransform();
	tgroup.setTransform( currentView  );
    }

    public void setAcceleration( double acceleration ) {
	this.acceleration = acceleration;
    }

    public final double getVelocity() {
	return velocity;
    }

    public final double getRollAngle() {
	return rollAngle;
    }

    public final double getPitchAngle() {
	return pitchAngle;
    }

    public final double getYawAngle() {
	return yawAngle;
    }

    public final boolean buttonPressed() {
	return mousePressed;
    }

    /**
     * An offset of the mouse from the initial position will rotate
     * the transform <code>angle</code> degrees per second per pixel of offset
     */
    public final void setMouseSensitivity(double angle ) {
	angleRate = degreesToRadians(angle);
    }

    public void componentHidden( ComponentEvent evt ) {
    }

    public void componentMoved( ComponentEvent evt ) {
    }

    public void componentResized( ComponentEvent evt ) {
	if (evt.getSource()==canvas) {
	    setCanvasSize();
	}
    }

    public void componentShown( ComponentEvent evt ) {
    }

}
