package util.cline;

import java.awt.*;
import java.awt.event.*;
import java.util.Hashtable;
	
/**
 * This class provides a command-line panel that can be placed in a 
 * Java GUI (Note that this is an AWT component - Swing was too damn slow).
 * There is also a DOS version for text i/o programs, however it does not 
 * contain as much functionality.
 * @see CommandPromptDOS
 */
public class CommandPromptAWT extends Panel 
	implements CommandPromptInterface, KeyListener {
	
	CommandRegister register_;
	TextArea textArea_;
	int lastPosition_;

	boolean promptPrinted_ = false;

	private Font defaultFont_ = new Font("Helvetica", Font.PLAIN, 10);
	private Font italicFont_ = new Font("Dialog", Font.ITALIC, 12);

	// to ask questions using dialog boxes, we a require a frame handle
	private Frame parent_;

	public CommandPromptAWT(Frame frame, 
							int numRows) {
		parent_ = frame;
		setLayout (new BorderLayout (5, 5));
		textArea_ = new TextArea("",numRows,80,
			TextArea.SCROLLBARS_VERTICAL_ONLY);
		textArea_.setFont(defaultFont_);
		textArea_.addKeyListener(this);
		add(textArea_, BorderLayout.CENTER);
	}

	public void setColors(Color foreground, Color background) {
		setForeground(foreground);
		setBackground(background);
		textArea_.setForeground(foreground);
		textArea_.setBackground(background);
	}

	class QuestionAsker extends Dialog implements ActionListener {
		TextField field;
		Button okButton;
		Frame frame_;

		String answer_;

		QuestionAsker(Frame frame, String title) {
			super(frame, title, true);
			frame_ = frame;
			Panel panel = new Panel();
			Label label = new Label("Answer: ");
			panel.add(label);
			field = new TextField(40);
			field.addActionListener(this);
			panel.add(field);
			add("Center", panel);
			setBackground(Color.white);

			pack();
		}

		public void actionPerformed(ActionEvent event) {
			Object source = event.getSource();
			if (source == field) {
				answer_ = field.getText();
			}
			field.selectAll();
			setVisible(false);
		}

		public String getAnswer() {
			return answer_;
		}

		public void setAnswer(String defaultAnswer) {
			field.setText(defaultAnswer);
			field.selectAll();
		}
		
		// overides the parent's show method
		public void show() {
			
			// set the location of where we want to display the dialog
			// I used to put the dialog right in the center of the frame,
			// however this obsures some of the drawing area that might 
			// be relevant to the question. So now I place the dialog in 
			// the middle of the text area.

			//Rectangle fr = frame_.getBounds();
			Rectangle dr = getBounds();
			Rectangle tr = textArea_.getBounds();
			int xcen, ycen;
			//xcen = fr.x + fr.width/2;
			//ycen = fr.y + fr.height/2;
			xcen = tr.x + tr.width/2;
			ycen = tr.y + tr.height/2;
			int cornx = xcen - dr.width/2;
			int corny = ycen - dr.height/2;

			setLocation(cornx,corny);
			super.show();
		}
	};

	public String askQuestion(String question) {
		QuestionAsker asker = new QuestionAsker(parent_, question);
		asker.show();
		return asker.getAnswer();
	}
	
	public String askQuestion(String question, String defaultAns) {
		QuestionAsker asker = new QuestionAsker(parent_, question);
		asker.setAnswer(defaultAns);
		asker.show();
		return asker.getAnswer();
	}

	public boolean askBooleanQuestion(String question) {
		String response = askQuestion(question + " (y/N)", "n");
		if (response == null) { 
			return false; 
		} else if (response.equalsIgnoreCase("yes") || 
				response.equalsIgnoreCase("y")) {
				return true;
		} 
		return false;
	}

	public void setRegister(CommandRegister cregister) {
		register_ = cregister;
	}

	public void message(String message) {
		// print the message and then display the prompt again!
		println(message);
		printPrompt();
	}

	public void print(String string) {
		int length = string.length();
		int curPosition = textArea_.getCaretPosition();
		int newPosition = curPosition + length;
		textArea_.insert(string, curPosition);
		try {
			textArea_.setCaretPosition(newPosition);
		} catch (IllegalArgumentException e) {
			System.err.println("Ilegal argument in text");
			e.printStackTrace(System.err);
		}
		lastPosition_ = newPosition;
	}

	public void println(String string) { print("\n" + string); }
	//public void println(String string) { print("\n" + string + "\n"); }
	public void print(Object object) { print(object.toString()); }
	public void println(Object object) { println(object.toString()); }
	
	private void printPrompt() { 
		//print(register_.getName() + "> "); 
		print("\n" + register_.getName() + "> "); 
		promptPrinted_ = true;
	}

	public void start() {
		printPrompt();
	}

	public void keyTyped(KeyEvent e) {
		if (e.getKeyCode() == KeyEvent.VK_ENTER) {
			System.out.println("Enter key typed");
			e.consume();
		}
	}

	public void keyPressed(KeyEvent keyEvent) {
		if (keyEvent.getKeyCode() == KeyEvent.VK_ENTER) {
			keyEvent.consume();
			promptPrinted_=false;
			String input = getInput();
			if (input != null) {
				try {
					register_.executeCommand(register_.parseCommand(input));
				} catch (CommandException e) {
					println(e);
				}
			}
			// the prompt might have been printed by the command
			if ( !promptPrinted_) { printPrompt(); }
		}
	}

	public String getInput() {
		textArea_.select(lastPosition_, textArea_.getCaretPosition());
		String input = textArea_.getSelectedText();
		if (input != null) {
			// for some reason select() causes the caret to move to 
			// the beginning of the selection. We have to reset it 
			// whenever we extract input
			textArea_.setCaretPosition(
				textArea_.getCaretPosition() + input.length());
		} 
		return input;
	}

	public void keyReleased(KeyEvent e) {
		if (e.getKeyCode() == KeyEvent.VK_ENTER) {
			e.consume();
		}
	}
}

