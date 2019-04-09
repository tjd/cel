// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.noa;

import util.cline.BaseCommand;
import cogsci.Control;
import java.util.Enumeration;

/**
 * This command loads a NOA memory file.<P>
 * The user will be prompted whether they want to replace the current 
 * memory contents.
 */
public class LoadMemoryCommand extends BaseCommand {

	public LoadMemoryCommand() {
		super("loadmem", 1, "'NOA memory file'" +
			"\n - loads a NOA long term memory file");
	}

	public void execute(String args[]) {
		// make sure the user wants to replace the existing NOA
		boolean response = getPrompt().askBooleanQuestion(
			"Are you sure you want to replace the existing NOA?");
		if (response == false) {
			return;
		}
		NOA.load(args[0]);
	}
}

