// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.noa;

import cogsci.Control;
import java.io.Serializable;
import java.io.File;

public class FileBlob extends Blob implements Serializable {

	String fileName_;

	public FileBlob(String name, BlobDecoder decoder, String fullName) {
		super(name, decoder);
		fileName_ = fullName;
	}

	public String getFileName() {
		return fileName_;
	}

	public void serialize() {} 
	public void unserialize() {} 
}
