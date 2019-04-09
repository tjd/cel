// DIVA (c) David Croft, 2000. University of Waterloo

package cogsci;

import util.cline.BaseCommand;
import java.util.Enumeration;

class QuestionCommand extends BaseCommand {

	QuestionCommand() {
		super("question", 1, "question" +
			"\n - test command for asking the user questions");
	}

	public void execute(String args[]) {
		String answer = Control.askQuestion(args[0]);
		Control.status("Your answer was: " + answer);
	}
}

