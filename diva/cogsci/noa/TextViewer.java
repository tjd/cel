// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.noa;

import cogsci.Control;
import java.awt.*;
import java.awt.event.*;
import java.util.Hashtable;

public class TextViewer extends Panel 
	implements BlobDecoder, ActionListener {

	Hub TEXT;

	TextArea textArea_;
	TextField hubNameField_;
	TextBlob currentBlob_;

	Button updateButton_;
	String updateAction_ = "update";

	Font defaultFont_ = new Font("Helevetica", Font.PLAIN, 12);

	private int size_;

	public TextViewer(int size) {
		size_ = size;

		TEXT = new Hub("text");
			TEXT.associate(NOA.IS, NOA.INPUT);
		currentBlob_ = null;

		Panel hubNamePanel = new Panel();
		hubNamePanel.setLayout(new FlowLayout());
		hubNamePanel.add(new Label("Hub name: "));
		hubNameField_ = new TextField("", 20);
		hubNamePanel.add(hubNameField_);

		updateButton_ = new Button(updateAction_);
		updateButton_.addActionListener(this);
		updateButton_.setActionCommand(updateAction_);
		hubNamePanel.add(updateButton_);
		add(hubNamePanel, BorderLayout.NORTH);
		
		int numRows = size_/20;
		int numCols = (size_/8)+5;
		textArea_ = new TextArea(numRows, numCols);
		textArea_.setFont(defaultFont_);
		add(textArea_, BorderLayout.CENTER);
	}

	public void setColors(Color foreground, Color background) {
		hubNameField_.setForeground(foreground);
		hubNameField_.setBackground(background);
		textArea_.setForeground(foreground);
		textArea_.setBackground(background);
	}

	public void registerCommands() {
		// no commands yet for the text viewer
	}

	public void setText(String hub, String text) {
		hubNameField_.setText(hub);
		textArea_.setText(text);
	}
	
	public void decode(Blob blob) {
		Control.activateDecoder(this);
		if (!(blob instanceof TextBlob)) {
			Control.error("TextViewer cannot decode non-text blobs");
		}
		currentBlob_ = (TextBlob)blob;
		hubNameField_.setText(currentBlob_.getName());
		textArea_.setText(currentBlob_.getText());
	}

	public void actionPerformed(ActionEvent evt) {
		if (evt.getSource() instanceof Button) {
			String arg = evt.getActionCommand();
			if (arg.equals(updateAction_)) {
				String name = hubNameField_.getText();
				if (name == null || name.equals("")) {
					Control.error("Cannot have a text hub with no name");
				}
				Hub hub = NOA.get(name);
				if (hub != null) {
					if (currentBlob_ == hub) {	// TextBlob exists
						currentBlob_.setText(textArea_.getText());
					} else {	// hub already exists, need to update
								// TextBlob
						if (hub instanceof TextBlob) {
							currentBlob_ = (TextBlob)hub;
							currentBlob_.setText(textArea_.getText());
						} else {	// hub exists, but is not a TextBlob
							// TODO: namespace important here
							currentBlob_ = new TextBlob(name, this,
									textArea_.getText());
							currentBlob_.associate(NOA.HAS, TEXT);
							// name will have changed
							hubNameField_.setText(currentBlob_.getName());
						}
					}
				} else {	// new TextBlob!
					System.out.println("new text blob");
					currentBlob_ = 
						new TextBlob(name, this, textArea_.getText());
					currentBlob_.associate(NOA.HAS, TEXT);
				}
			}
		}
	}	

	public Dimension getMinimumSize() {
		return new Dimension(50, 50);
		//return getPreferredSize();
	}

	public Dimension getMaximumSize() {
		return new Dimension(1500, 1500);
		//return getPreferredSize();
	}

	public Dimension getPreferredSize() {
		return new Dimension(size_, size_);
	}
}
