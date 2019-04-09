// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.imagery;

import util.cline.*;
import cogsci.Control;

/**
 * This is the base class for any mental imagery command (any command that
 * makes calls to the MindsEye class). 
 */
public abstract class MindsEyeCommand extends BaseCommand {
	
	protected MindsEye mindsEye_;

	protected MindsEyeCommand(	MindsEye mindsEye,
								String name,
								int numArgs,
								String helpDesc) {
		super(name, numArgs, helpDesc);
		mindsEye_ = mindsEye;
	}

	protected MindsEye getMindsEye() {
		return mindsEye_;
	}
	
	// should be overloaded by child class
	public void execute(String args[]) {}
}

