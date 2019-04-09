package util.cline;

import java.util.Enumeration;
import java.io.*;

/**
 * This command reads in a text file containing a list of commands. It 
 * is basically a utility for batch executions. Each line of the file 
 * should contain a command to be executed. Comment lines can be inserted
 * by placing a '#' at the start of the line. By convention, I name all my 
 * scripts with a .scr ending (good old ms-dos conventions).
 */
class RunScriptCommand extends BaseCommand {

	RunScriptCommand() {
		super("runscript", 1, "fileName" +
			"\n - reads the specified file containing a list of commands");
	}

	public void execute(String args[]) {
		String fileName = args[0];
		BufferedReader in = null;

		try {
			in = new BufferedReader(new FileReader(fileName));
		} catch (FileNotFoundException e) {
			prompt_.println("Error: RunScript file not found: " + fileName);
			return;
		}

		String nextCommand = null;
		int lineNumber=0;
		try {
			while ((nextCommand=in.readLine()) != null) {
				lineNumber++;
				if (nextCommand.equals("") || nextCommand.startsWith("#")) {
					continue;
				}

				try {
					register_.executeCommand(
						register_.parseCommand(nextCommand));
				} catch (CommandException e) {
					prompt_.println("Error: while reading script file " +
						fileName + " on line " + lineNumber);
					prompt_.println(e);
					return;
				}
			}
			in.close();
		} catch (IOException e) {
			prompt_.println("Error: IO exception reading script file " + 
				fileName);
		}
	}
}
