// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.noa;

import cogsci.Control;
import java.io.*;

/**
 * A Blob represents a concept that includes additional (non-associative
 * data). This could be a blob of text, an attached Java3D scene graph, a jpg 
 * image etc... This class should be extended for any particular data type.
 *
 * @see TextBlob
 */
public abstract class Blob extends Hub implements java.io.Serializable {

	protected BlobDecoder decoder_;

	protected Blob(String name, BlobDecoder decoder) {
		super(name);
		decoder_ = decoder;
	}

	public BlobDecoder getDecoder() {
		return decoder_;
	}

	public void decode() {
		// it is possible that a serialized memory file may have 
		// specialized blobs that we do not have a decoder for,
		// in which case they cannot be decoded
		if (decoder_ == null) {
			Control.status("No decoder set for this blob");
			return;
		}
		decoder_.decode(this);
	}
	private void writeObject(ObjectOutputStream s)	throws IOException { 
		//System.out.println("Writing blob");
		serialize();
		try {
			s.defaultWriteObject();    
		} catch (Exception e) {
			System.err.println("Failed to write blob: " + e);
			e.printStackTrace();
		}
	}
    
	private void readObject(ObjectInputStream s) 
		throws IOException, ClassNotFoundException {
			unserialize();
			s.defaultReadObject(); 
	}
	
	// all blobs should overload these methods incase they have to do
	// something special with their data when they are serialized or 
	// unserialized. For example, J3DBlobs have to remove their associated
	// decoder which is not serializable and then reset this when a new 
	// memory file is loaded.
	public abstract void serialize(); 
	public abstract void unserialize(); 
}
