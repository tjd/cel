// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.noa;

/**
 * Public interface that should be implemented by all interfaces with 
 * the NOA system. All inputs should accept some sort of file.
 */
public interface NOAInput {
	public void input(String fileName); 
}
