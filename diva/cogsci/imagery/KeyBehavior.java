package cogsci.imagery;

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
 * Handles key events specific to the MindsEye.
 */
public class KeyBehavior extends Behavior {

    MindsEye mindsEye_;

    // look for released events since sometimes ESCAPE only generates a 
    // RELEASE event
    WakeupCriterion criterion[] = { 
    		new WakeupOnAWTEvent( Event.KEY_PRESS ),
    		new WakeupOnAWTEvent( Event.KEY_RELEASE ),
		};

    WakeupCondition conditions = new WakeupOr( criterion );

    public KeyBehavior(MindsEye mindsEye) {
		mindsEye_ = mindsEye;
    }

    public void initialize() {
        wakeupOn(conditions);
    }

    public void processStimulus( Enumeration criteria ) {
		WakeupCriterion wakeup;
		AWTEvent[] evt=null;

		while( criteria.hasMoreElements() ) {
			wakeup = (WakeupCriterion)criteria.nextElement();
			if (wakeup instanceof WakeupOnAWTEvent) {
			 evt=((WakeupOnAWTEvent)wakeup).getAWTEvent();
			}

			if (evt!=null) {
			for(int i=0; i<evt.length; i++) {
				if (evt[i] instanceof KeyEvent)  {
				processKeyEvent( (KeyEvent)evt[i] );
				}
			}
			}
		}
		wakeupOn( conditions );
    }

    private void processKeyEvent( KeyEvent evt ) {
		int key = evt.getKeyCode();
		//System.out.print("Key pressed: '" + 
		//	KeyEvent.getKeyText(key) + "': ");
		switch (key) {
		  case KeyEvent.VK_SLASH:
			if ((evt.getID() == KeyEvent.KEY_RELEASED) && 
				(mindsEye_.timing)) {
				//mindsEye_.outputTiming();
			}
			break;
		  case KeyEvent.VK_M:
			System.out.println("Marking current viewpoint");
			mindsEye_.markCurrentView();
			break;
		  case KeyEvent.VK_R:
		  	System.out.println("Reseting viewpoint");
			mindsEye_.resetViewpoint();
			break;
		  case KeyEvent.VK_ESCAPE:
			// do this on press or release
			//System.out.println("Escape key pressed");
			if (mindsEye_.timing) {
				//mindsEye_.outputTiming();
			}
			System.exit(0);
			break;
		  default:
		    System.out.println(" - ");
			break;
		}
    }
}
