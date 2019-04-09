// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.imagery;

/**
 * The requirement for this class is probably not the best software design.
 * Currently, whenever a scene is analyzed, the DynamicsAnalysis class
 * attempts to generate three motion patterns. I tried to abstract the details
 * of each motion pattern (eg. convergence) into each class (eg.
 * ConvergenceMotion). 
 *
 * @see MotionPattern.java
 * @see DynamicsAnalysis.java
 */
// TODO - replace the checkMotionPattern that is called in the constructor 
// of every MotionPattern with a static factor method that returns a motion 
// pattern object if one can be generated and null if there is no motion
// pattern.
class MotionPatternException extends Exception {
	
	String patternType_;

	MotionPatternException(String patternType) {
		patternType_ = patternType;
	}

	public String toString() {
		return "Motion pattern does not exist: " + patternType_;
	}
}
