package cogsci.imagery;

import com.sun.j3d.utils.behaviors.*;

// adopted from the VRML97 demo code

/*
 *      @(#)MotionNotifierInterface.java 1.5 98/02/20 14:33:06
 *
 * Copyright (c) 1996 Sun Microsystems, Inc. All Rights Reserved.
 *
 *    Author Paul Byrne
 */


import javax.vecmath.*;
import javax.media.j3d.Transform3D;

/**
 * This interface can be implemented in user code and can be used
 * with @see FlightBehavior to get notification in any change of position
 * or orientation of the View
*/
public interface MotionNotifierInterface {

	/** 
   	 * This method will be called by the behavior it has been registered with
	 * whenever that behavior is executed and the Viewer has changed position
	 * or orientation
	*/
	public void viewMoved( Matrix4d position );

	/**
	* Called when the user changes the velocity
	*/
	public void velocityChanged( double newVelocity );

	/**
	* Called when a mouse button is pressed (TODO: pass whole event?)
	*/
	public void buttonPressed( int buttonId );
}
