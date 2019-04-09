// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.imagery;

import cogsci.Control;
import cogsci.noa.*;
import util.j3d.J3DUtils;

import java.awt.event.*;
import java.awt.*;
import java.awt.geom.*;
import java.awt.print.*;
import java.awt.font.*;
import java.util.*;
import javax.media.j3d.*;
import javax.vecmath.*;
import com.sun.j3d.utils.geometry.*;

/**
 * This class draws a scene graph (upside down tree) in a graphical format. It
 * is capable of drawing two graphs on the same panel and a set of mappings
 * between the graphs (see the Vector mappings_).
 */
public class GraphDrawer extends Panel implements Printable {
	
	final int NODE_DIAMETER	= 8;
	final int RING_DIAMETER	= 16;	// Rings are used to highlight nodes
	final int BORDER_WIDTH	= 12;

    final static BasicStroke stroke_ = new BasicStroke(1.0f);
    final static BasicStroke wideStroke_ = new BasicStroke(8.0f);
    final static float dash1[] = {10.0f};
    final static BasicStroke dashStroke_ = new BasicStroke(1.0f,
		BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10.0f, dash1, 0.0f);
	final static Font nodeFont_ = new Font("Helvetica", Font.PLAIN, 9);
	final static Font titleFont_ = new Font("Helvetica", Font.BOLD, 12);
	final static Font bigFont_ = new Font("Courier", Font.BOLD, 12);
	
	// these are set by the VisualBuffer by calling the setColors() method
	Color foreground_ = Color.black;
	Color background_ = Color.white;

	int size_;		// the width and height of the drawing area
	int centerx_;	// x co-ordinate of the center of drawing area
	int centery_;	// y co-ordinate of the center of drawing area
	int graphWidth_;// max width to draw a graph

	BranchGroup graph1_ = null;
	BranchGroup graph2_ = null;
	Vector mappings_ = null;

	int numLeafs1_ = 0;
	int numLeafs2_ = 0;

	int spacingy_ = 18;

	boolean drawNames_ = true;
	
	private final int UNKNOWN = 18;
	private final int NUM_NODE_INFOS = UNKNOWN+1;

	private NodeInfo nodeInfo[] = new NodeInfo[NUM_NODE_INFOS];

	private class NodeInfo {
		Color color;
		Class ref;
		String name;
		boolean present;

		private NodeInfo(int code) {
			ref = getReference(code);
			String fullName = ref.getName();
			name = fullName.substring(fullName.lastIndexOf('.')+1);
		}

		public String toString() {
			return name + " - " + color;
		}

		private final Class getReference(int code) {
			try {
			switch (code) {

		case  0: return Class.forName("javax.media.j3d.BranchGroup"); 
		case  1: return Class.forName("javax.media.j3d.TransformGroup"); 
		case  2: return Class.forName("javax.media.j3d.SharedGroup"); 
		case  3: return Class.forName("javax.media.j3d.Group"); 
		case  4: return Class.forName("com.sun.j3d.utils.geometry.Box"); 
		case  5: return Class.forName("com.sun.j3d.utils.geometry.Sphere"); 
		case  6: return Class.forName("com.sun.j3d.utils.geometry.Cylinder"); 
		case  7: return Class.forName("com.sun.j3d.utils.geometry.Cone");
		case  8: return Class.forName("javax.media.j3d.Behavior"); 
		case  9: return Class.forName("javax.media.j3d.Shape3D"); 
		case 10: return Class.forName("javax.media.j3d.Background"); 
		case 11: return Class.forName("javax.media.j3d.ViewPlatform"); 
		case 12: return Class.forName("javax.media.j3d.Text3D"); 
		case 13: return Class.forName("javax.media.j3d.Fog"); 
		case 14: return Class.forName("javax.media.j3d.Appearance"); 
		case 15: return Class.forName("javax.media.j3d.Link"); 
		case 16: return Class.forName("javax.media.j3d.Light"); 
		case 17: return Class.forName("javax.media.j3d.Texture"); 
		case UNKNOWN: return Class.forName("javax.media.j3d.SceneGraphObject");

			}
			return Class.forName("javax.media.j3d.SceneGraphObject");
			} catch (ClassNotFoundException e) {
				Control.error("could not find visual class: " + e);
				return null;
			}
		}
	}	// end of NodeInfo class definition
	
	private final Color getColor(int code) {
		if (code == UNKNOWN) {
			return background_;	// invisible?
		}
		switch (code%9) {	// only 9 colors are good to draw with
			case 0: return foreground_;	// primary drawing color is foreground
			case 1: return Color.blue;
			case 2: return Color.pink;
			case 3: return Color.green;
			case 4: return Color.orange;
			case 5: return Color.cyan;
			case 6: return Color.magenta;
			case 7: return Color.red;
			case 8: return Color.yellow;
		}
		return Color.black;	// should never make it here
	}

	
	/**
	 * This method can be called to set a list (vector) of nodes that 
	 * should be marked when the scene graph is drawn. This is used for 
	 * visual analogies to mark nodes from one graph that map to nodes 
	 * in another graph. Note that whenever display() is called in the 
	 * VisualBuffer, the mappings are reset.
	 */
	public void setMappings(Vector mappings) {
		mappings_ = mappings;
	}
	
	public void setVerbose(boolean verbose) {
		drawNames_ = verbose;
	}

	public GraphDrawer() {
		addMouseListener(new MouseAdapter() {
			public void mousePressed(MouseEvent e) {
				update(e.getX(), e.getY());
			}
		});
		updateSize(getWidth());
		for (int i=0; i<NUM_NODE_INFOS; i++) {
			nodeInfo[i] = new NodeInfo(i);
			nodeInfo[i].color = getColor(i);
		}
	}

	private void updateSize(int size) {
		size_ = size;
		centerx_ = size_/2;
		centery_ = centerx_;
		if (graph2_ != null) {
			graphWidth_ = size_/2;
		} else {
			graphWidth_ = size_;
		}
	}

	public void setColors(Color foreground, Color background) {
		foreground_ = foreground;
		background_ = background;
		for (int i=0; i<NUM_NODE_INFOS; i++) {
			nodeInfo[i].color = getColor(i);
		}
	}

	private final NodeInfo getNodeInfo(Node node) {
		Class ref = node.getClass();
		for (int i=0; i<NUM_NODE_INFOS; i++) {
			if (nodeInfo[i].ref.equals(ref)) {
				return nodeInfo[i];
			}
		} 
		// if we made it here, there is not a direct class match with
		// any of the nodes defined in NodeInfo. If there is a parent 
		// class return this
		for (int i=0; i<NUM_NODE_INFOS; i++) {
			if (nodeInfo[i].ref.isInstance(node))  {
        //System.out.println("No direct match for " + ref.getName());
				return nodeInfo[i];
			}
		}
		return nodeInfo[UNKNOWN];	// should not make it here
	}

	private final void resetNodeInfo() {
		for (int i=0; i<NUM_NODE_INFOS; i++) {
			nodeInfo[i].present = false;
		}
	}

	private final void setNodePresence(Node node) {
		NodeInfo ni = getNodeInfo(node);
		ni.present = true;
	}

	public Dimension getMinimumSize() {
		return getPreferredSize();
	}
	
	public Dimension getMaximumSize() {
		return getPreferredSize();
	}

	public Dimension getPreferredSize() {
		return new Dimension(size_,size_);
	}

	public void setSecondGraph(BranchGroup graph2) {
		graph2_ = graph2;
	}

	public void update(BranchGroup graph1, int size) {
		if (graph1 == null) {
			Control.error("cannot update graph viewer with null graph");
			return;
		}

		updateSize(size);

		graph1_ = graph1;
		numLeafs1_ = J3DUtils.countLeafs((Node)graph1_);
		
		if (graph2_ != null) {
			numLeafs2_ = J3DUtils.countLeafs((Node)graph2_);
		}
		repaint();
	}

	final void update(int x, int y) {
	}

    public void paint(Graphics g) {
        super.paint(g);
        Graphics2D g2 = (Graphics2D) g;
        drawAll(g2);
    }       

    public final void drawAll(Graphics2D g2) {
		if (graph1_ == null) { 
			System.out.println("Cannot draw tree - root is null");
			return;
		}
		
		// uncomment this if you want a border
		/*
        g2.setColor(Color.lightGray);
        g2.drawRect(BORDER_WIDTH, 
					BORDER_WIDTH, 
					size_ - (2*BORDER_WIDTH+BORDER_WIDTH/2),
					size_ - 3*BORDER_WIDTH);
        */

		g2.setColor(foreground_);
		
		// draw the legend
		int height = drawLegend(g2, 2*BORDER_WIDTH, 2*BORDER_WIDTH);

		int x1cen = centerx_;
		int y1cen = 2*BORDER_WIDTH+height+spacingy_;
		int width = size_; 
		int x2cen = 0;
		int y2cen = 0;

		Hashtable graph1Points = null;
		Hashtable graph2Points = null;
		
		// if there are two graphs to draw (used for visual analogies)
		// calculate widths to fit two graphs
		if (graph2_ != null) {
			x1cen = centerx_/2;
			width = size_/2;
			x2cen = centerx_+x1cen;
			y2cen = y1cen;

			// create 2 new tables to store the co-ordinates of drawn nodes
			graph1Points = new Hashtable(50);
			graph2Points = new Hashtable(50);
		}

		// draw the first tree
		drawNode(g2, x1cen, y1cen, width, graph1_, graph1Points);
		
		// if it exists, draw the second tree and the mappings between
		// graph1 and graph2
		if (graph2_ != null) {
			drawNode(g2, x2cen, y2cen, width, graph2_, graph2Points);

			// now that we have drawn both graphs, draw the mappings
			// between them
			drawGraphMappings(g2, graph1Points, graph2Points);
		}
		
		/*	
		if (drawNames_) {
			// Print the title information
			String title = "Scene Graph";
			TextLayout titleLay = new TextLayout(title, titleFont_,
				g2.getFontRenderContext());
			titleLay.draw(g2, BORDER_WIDTH+5, 2*BORDER_WIDTH);
		}
		*/
    }
	
	/**
	 * The draw node recursively calls itself to draw a scene graph.
	 *
	 * @param xcen - the x co-ordinate to draw the node at
	 * @param ycne - the y co-ordinate to draw the node at
	 * @param width - the width available (along the x-axis) to draw 
	 * children nodes.
	 * @param node - the node to be drawm
	 */
	private final void drawNode(Graphics2D g2, 
								int xcen, 
								int ycen, 
								int width,
								Node node,
								Hashtable points) {
		if (node == null) {
			return;
		} 
		
		// if we are recording the points at which nodes are drawn
		// so that we can draw mappings between graph1 and graph2
		if (points != null) {
			points.put(node, new Point(xcen, ycen));
		}

		int x = xcen-(NODE_DIAMETER/2);
		int y = ycen-(NODE_DIAMETER/2);
        
		NodeInfo ni = getNodeInfo(node);
        g2.setColor(ni.color);
		g2.setStroke(stroke_);
		Ellipse2D.Float circle = 
			new Ellipse2D.Float(x, y, NODE_DIAMETER, NODE_DIAMETER);
        g2.draw(circle);
		g2.fill(circle);
				
		// Print the name of the node
		if (drawNames_ == true ) {
			TextLayout text = new TextLayout(ni.name, nodeFont_,
				g2.getFontRenderContext());
			//double width = (double)text.getBounds().getWidth();
			//double height = (double)text.getBounds().getHeight();
			int namePosy = ycen;
			if (node instanceof Leaf) {	// space leaf labels slightly lower
				namePosy += 10;
			}
			text.draw(g2, (xcen+NODE_DIAMETER), namePosy);
		}

		if (node instanceof Group) {
			Group parent = (Group)node;
			int num = parent.numChildren();

			if (num > 3) {	// this is a hack for graphs with lots of kids
				width = graphWidth_;
			}

			int locy = (ycen + spacingy_);
			int locx = xcen;
			int newWidth = width;
			int spacex = width/(num+1);
			//int spacex = width/(num+2);
			if (num > 1) {	// more than one child, figure out spacing
				locx = (xcen - (width/2)) + spacex;
				newWidth = spacex;
				//newWidth = spacex-5;
			}
			for (int i=0; i<num; i++) {
				Node child = parent.getChild(i);
				if ((child instanceof Leaf && num > 1) ||
					(num > 3)) {
					locy += spacingy_;	// space child nodes for labels
				}
				drawNode(g2, locx, locy, newWidth, child, points);
				g2.setColor(ni.color);
				drawConnection(g2, xcen, ycen, locx, locy);
				locx += spacex;
			}
		}
	}

	private final void drawGraphMappings(Graphics2D g2,
										 Hashtable points1,
										 Hashtable points2) {
		if (mappings_ == null) {
			return;
		}
		
		// find the top and bottom coordinates of the graph
		int miny = 1000; // something big
		int maxy = 0;

		Enumeration e = mappings_.elements();
		while (e.hasMoreElements()) {
			Node si = ((MappingUnit)e.nextElement()).getSi();
			Point next = (Point)points1.get(si);
			if (next.y > maxy) {
				maxy = next.y;
			} 
			if (next.y < miny) {
				miny = next.y;
			}
		}
		int middleMapy = (maxy+miny)/2;

		e = mappings_.elements();
		for (int i=0; e.hasMoreElements(); i++) {
			MappingUnit munit = (MappingUnit)e.nextElement();
			Node si = munit.getSi();
			Node tj = munit.getTj();
			Point pointSi = (Point)points1.get(si);
			Point pointTj = (Point)points2.get(tj);

			Color ringColor = getColor(i);
			g2.setColor(ringColor);
			
			int x1 = pointSi.x-(RING_DIAMETER/2);
			int y1 = pointSi.y-(RING_DIAMETER/2);
			int x2 = pointTj.x-(RING_DIAMETER/2);
			int y2 = pointTj.y-(RING_DIAMETER/2);

			Ellipse2D ring = new Ellipse2D.Float( 	x1, y1, 
													RING_DIAMETER, 
													RING_DIAMETER);
			g2.draw(ring);
			ring = new Ellipse2D.Float( 			x2, y2, 
													RING_DIAMETER, 
													RING_DIAMETER);
			g2.draw(ring);

			// draw an arc between the nodes
			
			// if we are in the middle top of the graph, draw an arch
			// upwards, if we are in the bottom half, draw the arc
			// curving towards the bottom
			float arcmidx = (float)(pointTj.x-pointSi.x);
			float arcmidy = 0.0f;

			if (pointSi.y < middleMapy) {
				arcmidy = pointSi.y - (20.0f + middleMapy-pointSi.y);
			} else {
				arcmidy = pointSi.y - 40.0f;
//				arcmidy = pointSi.y + 
//					(650.0f-2*pointSi.x);
//					//(400.0f-pointSi.x) + (pointSi.y-middleMapy);
			}

			QuadCurve2D arc = new QuadCurve2D.Float(
										(float)pointSi.x, 
										(float)pointSi.y,
										arcmidx, arcmidy,
										(float)pointTj.x,
										(float)pointTj.y);
			g2.draw(arc);
		}
	}

	private final int drawLegend(Graphics2D g2, int startx, int starty) {
		// currently there is no legend
		return 20;
	}
	
	public void drawConnection(	Graphics2D g2, 
								int startx, 
								int starty, 
								int endx,
								int endy ) {
		g2.setStroke(stroke_);
		g2.draw(new Line2D.Double(startx, starty, endx, endy));
	}
	
	public int print(Graphics g, PageFormat pf, int pageIndex)
		throws PrinterException {
		drawAll((Graphics2D) g);
		return Printable.PAGE_EXISTS;
	}
}
