// DIVA (c) David Croft, 2000. University of Waterloo

package cogsci;

import cogsci.noa.BlobDecoder;
import util.cline.CommandPromptInterface;

/**
 * Interface that can be implemented to create a GUI for the Control class.
 * @see Imagine
 */
public interface ControlGUI {
	public CommandPromptInterface getPrompt();
	public void show();
	public void viewMemory(String name);
	public void activateDecoder(BlobDecoder decoder);
}
