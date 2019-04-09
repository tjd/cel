// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.noa;

import java.io.File;
import cogsci.Control;

public class FileDecoder implements BlobDecoder {
	public void decode(Blob blob) {
		if (! (blob instanceof FileBlob)) {
			Control.error("can only decode File blobs");
			return;
		}
		FileBlob fileBlob = (FileBlob)blob;
		String command = "explorer " + fileBlob.getFileName();
		try {
			Process process = Runtime.getRuntime().exec(command);
			Control.status("Decoding " + fileBlob.getFileName());
		} catch (java.io.IOException e) {
			Control.error("could not decode file blob: " + e.getMessage());
		}
	}

	public void registerCommands() {
		// no commands yet for the FileDecoder
	}
}

