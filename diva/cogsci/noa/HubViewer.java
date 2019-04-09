// DIVA (c) David Croft, 2000. University of Waterloo
package cogsci.noa;

import cogsci.Control;
import util.debug.Debug;
import java.awt.event.*;
import java.awt.*;
import java.awt.geom.*;
import java.awt.print.*;
import java.awt.font.*;
import java.util.*;

/**
 * This is a graphical viewer for hubs within the network.<P>
 * Use the ViewCommand to bring up a HubViewer and look at the 
 * associations for a specified hub.
 */
public class HubViewer extends Panel implements Printable {
	
	final int HUB_DIAMETER = 14;
	final int BORDER_WIDTH = 12;
	final int CROSSHAIR_DIAMETER = 80;

    final static BasicStroke stroke = new BasicStroke(1.0f);
    final static BasicStroke wideStroke = new BasicStroke(8.0f);
    final static float dash1[] = {10.0f};
    final static BasicStroke dashedStroke = new BasicStroke(1.0f,
		BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10.0f, dash1, 0.0f);
	//final static Font hubFont_ = new Font("Dialog", Font.PLAIN, 9);
	final static Font hubFont_ = new Font("Helvetica", Font.PLAIN, 9);
	final static Font titleFont_ = new Font("Helvetica", Font.BOLD, 12);
	final static Font bigFont_ = new Font("Courier", Font.BOLD, 16);
	boolean drawTitle_ = true;

	int size_;		// the width and height of the drawing area
	int centerx_;	// x co-ordinate of the center of drawing area
	int centery_;	// y co-ordinate of the center of drawing area
	int radius_;	// radius of a the outer ring in which the hubs are drawn
	int halfRadius_;// half the radius - used to draw the inner ring of hubs
	int hubRadius_;	// radius of the individual hubs

	int minusx_;	// the coordinates of the x,y for the plus and minus
	int minusy_;	// signs used to increment the hub index
	int plusx_;
	int plusy_;

	int numHubsToDraw_;	
	int maxHubsViewable_ = 14;
	int lastIndex_;
	double angleIncrement_;


	Hub centerHub_ = null;
	Hub lastHub_ = null;

	public HubViewer(int size) {
		size_ = size;
		hubRadius_ = HUB_DIAMETER/2;
		centerx_ = size_/2;
		centery_ = centerx_;
		radius_ = (size_/2) - (3*BORDER_WIDTH) - HUB_DIAMETER;
		halfRadius_ = radius_/2;
        minusx_ = centerx_+radius_;
		minusy_ = centery_+45;
        plusx_ = centerx_-radius_;
		plusy_ = centery_-45;

		addMouseListener(new MouseAdapter() {
			public void mousePressed(MouseEvent e) {
				update(e.getX(), e.getY());
			}
		});
		update(NOA.INPUT);
	}

	public void setColors(Color foreground, Color background) {
		setForeground(foreground);
		setBackground(background);
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

	public final void update(String hubName) {
		Hub hub = NOA.get(hubName);
		if (hub != null) {
			update(hub);
		} else {
			Control.error(
				"Cannot update viewer with a non-existent memory hub");
		}
	}

	final void update(Hub hub) {
		if (hub == null) {
			return;
		} else if (hub != centerHub_) { // if we are not simply changing the 
			lastHub_ = centerHub_;		// index on the current centerHub_
			centerHub_ = hub;

			// find the index that we are coming from (default is zero)
			lastIndex_=0;
			for (int i=0; i<centerHub_.getCount(); i++) {
				if ((centerHub_.getSpoke(i) != null) && 
					(centerHub_.getOpposite(i) == lastHub_)) {
					lastIndex_ = i;
					break;
				} 
			}
		}

		// update the angle information for this hub
		numHubsToDraw_ = ((centerHub_.getCount() < maxHubsViewable_) ? 
			centerHub_.getCount() : maxHubsViewable_);
		angleIncrement_ = 2*Math.PI/((double)numHubsToDraw_);
		repaint();
		
		// if this is a Blob type Hub it will decode itself
		centerHub_.decode();
	}
	
	final void update(int x, int y) {

		// check for the plus and minus increment
		int flex=10;
		if ((minusx_-flex <= x && x <= minusx_+flex) &&
			(minusy_-flex <= y && y <= minusy_+flex)) {
			lastIndex_ = 
				(lastIndex_ == 0) ? (centerHub_.getCount()-1) : (lastIndex_-1);
			update(centerHub_);	// same center hub, different index
			return;
		} else if (	(plusx_-flex <= x && x <= plusx_+flex) &&
					(plusy_-flex <= y && y <= plusy_+flex)) {
			lastIndex_ = 
				(lastIndex_ == centerHub_.getCount()) ? 0 : (lastIndex_+1);
			update(centerHub_);	// same center hub, different index
			return;
		}

		// now see if any of the hubs have been clicked on

		double angle=0.0d;
		int index=lastIndex_;
		for (int i=0; i<numHubsToDraw_; i++) {
			if (index == centerHub_.getCount()) {
				index=0;
			}
			int xoff = (int)(((double)radius_)*Math.cos(angle));
			int yoff = (int)(((double)radius_)*Math.sin(angle));

			int x1 = centerx_ + xoff;
			int y1 = centery_ - yoff;
			int x2 = centerx_ - xoff;
			int y2= centery_ + yoff;
			
			if ((x1-hubRadius_ <= x && x <= x1+hubRadius_) && 
				(y1-hubRadius_ <= y && y <= y1+hubRadius_)) {
				update(centerHub_.getOpposite(index));
				return;
			} 

			index++;
			angle += angleIncrement_;
		}
	}

    public void paint(Graphics g) {
        super.paint(g);
        Graphics2D g2 = (Graphics2D) g;
        drawAll(g2);
    }       

    final void drawAll(Graphics2D g2) {

		Debug.ASSERT(centerHub_);
		
		// uncomment this if you want a border in the hub viewer
		/*
        g2.setPaint(Color.lightGray);
        g2.drawRect(BORDER_WIDTH, 
					BORDER_WIDTH, 
					size_ - (2*BORDER_WIDTH+BORDER_WIDTH/2),
					size_ - 3*BORDER_WIDTH);
		*/

		g2.setPaint(getForeground());
		
		// draw the center hub
		drawHub(g2, centerx_, centery_, centerHub_, lastIndex_);

		// draw the crosshair around the center hub
		drawCrossHair(g2, centerx_, centery_);
		
		double angle=0.0d;
		int index = lastIndex_;	
		for (int j=0; j<numHubsToDraw_; j++) {
			if (index >= centerHub_.getCount()) {
				index=0;
			}

			Hub next = centerHub_.getOpposite(index);

			int xoff = (int)(((double)radius_)*Math.cos(angle));
			int yoff = (int)(((double)radius_)*Math.sin(angle));
			
			int x,y,xarrow,yarrow,xlab,ylab;

			boolean arrowTowardsCenter = true;
			if (centerHub_.getSpoke(index).a() ==  centerHub_) {
				arrowTowardsCenter = false;
			}
			
			if (next != null) {
				x = centerx_ + xoff;
				y = centery_ - yoff;
				drawHub(g2, x, y, next, index);
				xarrow = centerx_ + (xoff/3);
				yarrow = centery_ - (yoff/3);
				xlab = centerx_ + (xoff/2);
				ylab = centery_ - (yoff/2);
				Hub assocHub = centerHub_.getAssociation(index);
				String assocLabel = assocHub.getName(); 
				drawSpoke(g2, x, y, xarrow, yarrow, xlab, ylab, assocLabel,
					angle, arrowTowardsCenter);
			}
			index++;
			angle += angleIncrement_;
		}
		
		/*
		// Print the title information
		String title = "Associative Memory";
		TextLayout titleLay = new TextLayout(title, titleFont_,
			g2.getFontRenderContext());
		titleLay.draw(g2, BORDER_WIDTH+5, 2*BORDER_WIDTH);

		// print the hub and index info
		String string = "Centered on \"" + centerHub_.toString() + 
			"\" (range " + lastIndex_ + " to " + (index-1) +
			" of " + centerHub_.getCount() + ")";
		TextLayout indexInfo = new TextLayout(string, hubFont_,
			g2.getFontRenderContext());
		indexInfo.draw(g2, BORDER_WIDTH+5, 2*BORDER_WIDTH+16);
		*/

		// Print the plus and minus signs for incrementing the hub index
		String sign = "-";
        TextLayout minus = new TextLayout(sign, bigFont_,
			g2.getFontRenderContext());
        minus.draw(g2, minusx_, minusy_);
		sign = "+";
        TextLayout plus = new TextLayout(sign, bigFont_,
			g2.getFontRenderContext());
        plus.draw(g2, plusx_, plusy_);

    }

	final void drawSpoke(	Graphics2D g2, 
								int x, 
								int y, 
								int xarrow,
								int yarrow,
								int xlab,
								int ylab,
								String assocLabel,
								double angle, 
								boolean arrowTowardsCenter) {

		g2.setStroke(stroke);
		g2.draw(new Line2D.Double(centerx_, centery_, x, y));
		
		int cosoffset = (int)(10.0d*Math.cos(((Math.PI/4.0d) + angle)));
		int sinoffset = (int)(10.0d*Math.sin(((Math.PI/4.0d) + angle)));

		int head1x;
		int head1y;
		int head2x;
		int head2y;

		if (arrowTowardsCenter) {
			head1x = xarrow+sinoffset;
			head1y = yarrow+cosoffset;
			head2x = xarrow+cosoffset;
			head2y = yarrow-sinoffset;
		} else {	// arrow head points away from center
			head1x = xarrow-cosoffset;
			head1y = yarrow+sinoffset;
			head2x = xarrow-sinoffset;
			head2y = yarrow-cosoffset;
		}

		g2.draw(new Line2D.Double(xarrow, yarrow, head1x, head1y));
		g2.draw(new Line2D.Double(xarrow, yarrow, head2x, head2y));


		TextLayout assocLabelLay = new TextLayout(assocLabel, hubFont_,
				g2.getFontRenderContext());
		assocLabelLay.draw(g2, xlab, ylab);
	}

	final void drawHub(Graphics2D g2, 
						int xcen, 
						int ycen, 
						Hub hub, 
						int index) {

		if (hub == null) {
			return;
		} 
		String name = hub.toString();
		if (name == null) {
			name = "nameless";
		} else if (name.length() >= 15) {
			name = name.substring(0, 12) + "...";
		}
		name += "[" + index + "]";
			
		Color color = hub == lastHub_ ? Color.green : Color.red;
		if (hub == centerHub_) {
			color = Color.blue;
		}

		int x = xcen-hubRadius_;
		int y = ycen-hubRadius_;
        
		g2.setStroke(stroke);
		Ellipse2D.Float circle = 
			new Ellipse2D.Float(x, y, HUB_DIAMETER, HUB_DIAMETER);
        g2.draw(circle);
        g2.setPaint(color);
		g2.fill(circle);
        g2.setPaint(getForeground());
        
		// Print the name of the hub
        TextLayout text = new TextLayout(name, hubFont_,
			g2.getFontRenderContext());
        //double width = (double)text.getBounds().getWidth();
		//double height = (double)text.getBounds().getHeight();
        text.draw(g2, x, y+HUB_DIAMETER+15);
	}

	final void drawCrossHair(	Graphics2D g2,
								int xcen,
								int ycen) {
		
		int radius = CROSSHAIR_DIAMETER/2;
		int x = xcen-radius;
		int y = ycen-radius;
        
		g2.setStroke(stroke);
        g2.setPaint(Color.white);
		Ellipse2D.Double circle = 
			new Ellipse2D.Double(x, y, CROSSHAIR_DIAMETER, CROSSHAIR_DIAMETER);
        g2.draw(circle);
	}
	
	public int print(Graphics g, PageFormat pf, int pageIndex)
		throws PrinterException {
		drawAll((Graphics2D) g);
		return Printable.PAGE_EXISTS;
	}
}
