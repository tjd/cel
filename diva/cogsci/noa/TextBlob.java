// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.noa;

import cogsci.Control;
import java.io.Serializable;

public class TextBlob extends Blob implements Serializable {

	String text_;

	public TextBlob(String name, BlobDecoder decoder, String text) {
		super(name, decoder);
		text_ = text;
	}

	public TextBlob(String name, String text) {
		super(name, Control.getDecoder(Control.TEXT));
		text_ = text;
	}

	public void setText(String text) {
		text_ = text;
	}

	public String getText() {
		return text_;
	}

	public void serialize() {} 
	public void unserialize() {} 
}
