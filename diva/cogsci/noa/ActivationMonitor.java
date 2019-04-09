// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.noa;

import java.util.*;
import cogsci.Control;

/**
 * This class is used for spreading activation within the network.<P>
 * NOTE: this class us used by the Hub.activate() method and should 
 * never be used as part of the cogsci.noa API. 
 * @see ActivateCommand
 */
public class ActivationMonitor extends java.lang.Thread {

	private static int SLEEP_TIME = 1000;

	public void run() {
		while (true) {
			try {
				sleep(SLEEP_TIME); 
			} catch (InterruptedException e) {
				Control.error("activmon: interrupted");
			}
			if (NOA.isActive()) {
				boolean stillActive = false;
				//System.out.print("activmon: decrementing activation... ");
				Hashtable hubDictionary = NOA.getHubs();
				Collection values = hubDictionary.values();
				Iterator iterator = values.iterator();
				while (iterator.hasNext()) {
					Hub next = (Hub)iterator.next();
					if (next.decrementActivation() != 0) {
						stillActive = true;
					}
				}
				NOA.setActive(stillActive);
				//System.out.println(" finished");
			}
		}
	}
}
