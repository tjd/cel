// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.imagery;

import cogsci.Control;
import javax.media.j3d.*;
import javax.vecmath.*;
import java.util.*;

/**
 * Special unit used in the constraint network (for visual analogies) that is
 * always active.
 */
class SpecialUnit extends Unit {

	SpecialUnit() { 
		super(1.0, "SPECIAL", "A special unit always has 1.0 activation");
	}
}
