// DIVA (c) David Croft, 2000. University of Waterloo

package cogsci;

import util.cline.*;

// GUI imports
import java.awt.*;
import java.awt.event.*;
import java.io.File;

import cogsci.noa.*;
import cogsci.imagery.*;

import javax.media.j3d.Canvas3D;

/**
 * This class implements the basic GUI for the Control object. It 
 * contains a command prompt interface, a working memory area where data 
 * can be decoded and manipulated, and a long-term memory viewer that 
 * displays the concepts contained in the associative network.
 * @see util.cline.CommandPromptAWT
 * @see imagery.ui.MindsEye
 */
public class Imagine extends Frame 
	implements ControlGUI, ActionListener, ItemListener {

	private static TextViewer	textViewer_;
	private static MindsEye		mindsEye_;
	private static Canvas3D		visualBufferCanvas_; 
	private static HubViewer	hubViewer_;
	private static FileDecoder	fileDecoder_;

	
	// The command prompt from the util.cline package
	private static CommandPromptAWT prompt_;
    
	public Font defaultFont = 	new Font("Dialog", Font.PLAIN, 12);
	
	public boolean defaultDarkColors_ = true;	// default color setting
	public boolean defaultVerbose_ = true;	

	// Colors I like for the light colour setting
	// currently I use just black on white for screen shots
	//public Color LIGHT_BLUE = 	new Color(0.5f, 0.5f, 0.9f);
	//public Color GOLD_YELLOW = 	new Color(0.99f, 0.64f, 0.4f);

	public boolean startWithGraph_ = false;	// this determines whether 
											// the graph viewer is default
											// at startup

	// GUI stuff
	FileDialog 	fd;
    MenuBar		menuBar_;
    Menu 		mindsEyeMenu_;
    Menu 		textViewerMenu_;
    Menu 		m;
    Menu		fileVpMenu_;
    Menu		objVpMenu_;
    MenuItem	mi;
    CheckboxMenuItem hlCheck;
    CheckboxMenuItem viewGraphCheck_;
    CheckboxMenuItem viewAxesCheck_;
    CheckboxMenuItem darkColorsCheck_;
    CheckboxMenuItem verboseCheck_;
    String		fileVpAction = "setFileVp ";
    String		objVpAction = "setObjVp ";
    String		modeAction = "setMode ";
    String		resetViewpoint = "Reset Viewpoint";
	String 		openVRML = "Open VRML";
	String 		addText = "Add text";
	String 		addFile = "Add File";
	String 		ABOUT = "About";
	String 		VIEW_GRAPH = "View Graph";
	String 		VIEW_AXES = "View Axes";
	String 		HEADLIGHT = "Headlight";
	String 		DARK_COLORS = "Dark Colors";
	String 		VERBOSE = "Verbose";

	/**
	 * Constructor for the Imagine application.
	 */
	public Imagine() {
		super(Control.FULL_NAME);

		WindowListener l = new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				Control.exit();	
			}
			public void windowIconified(WindowEvent e) {
				mindsEye_.stop();
			}
			public void windowDeiconified(WindowEvent e) {
				mindsEye_.start();
			}
		};
		addWindowListener(l);

		// this is my attempt to change the window icon 
		// it didn't work so I stuck with that stupid coffee cup
		/*	
		ImageIcon icon = new ImageIcon("images\\eom.ico");
		Toolkit toolkit = getToolkit();
		Image icon = toolkit.getImage("images\\grover.bmp");
		toolkit.prepareImage(icon, 10, 10, 
		setIconImage(icon);
		*/
	
		setFont(defaultFont);
		setLayout(new BorderLayout(5,5));
		
		// the command prompt requires a frame handle to ask questions
		prompt_ = new CommandPromptAWT(this, 6);
		add(prompt_, BorderLayout.NORTH);	

		mindsEye_ = new MindsEye(getGraphicsConfiguration());
		mindsEye_.setDrawGraph(startWithGraph_);
		Control.setDecoder(mindsEye_, Control.IMAGERY);
		visualBufferCanvas_ = mindsEye_.getVisualBuffer();
		add(visualBufferCanvas_, BorderLayout.CENTER);
		
		textViewer_ = new TextViewer(500);
		Control.setDecoder(textViewer_, Control.TEXT);

		hubViewer_ = new HubViewer(300);
		add(hubViewer_, BorderLayout.EAST);

		// Setup the Menus
		menuBar_ = new MenuBar();

		// Control menu
		m = new Menu("Control");
		m.setShortcut(new MenuShortcut(KeyEvent.VK_ALT));
		mi = new MenuItem(openVRML);
		
		// allocate the file dialog box	
		fd = new FileDialog(this, "Load File", FileDialog.LOAD);
		fd.setDirectory(Control.LIBRARY_NAME);

		// How do you make a code for a key + the ALT key?
		// This does not work:
		MenuShortcut ms = 
			new MenuShortcut(KeyEvent.VK_O | KeyEvent.CTRL_MASK);
		mi.setShortcut(ms);
		mi.addActionListener(this);
		m.add(mi);

		mi = new MenuItem(addText);
		mi.addActionListener(this);
		m.add(mi);

		mi = new MenuItem(addFile);
		mi.addActionListener(this);
		m.add(mi);
		fileDecoder_ = new FileDecoder();
		Control.setDecoder(fileDecoder_, Control.FILE);


		mi = new MenuItem("Exit");
		mi.addActionListener(this);
		m.add(mi);
		menuBar_.add(m);
	
		// Minds Eye Menu

		mindsEyeMenu_ = new Menu("Minds Eye");

		// 3D Viewing stuff
		m = new Menu("View");
		fileVpMenu_ = new Menu("File Viewpoints");
		m.add(fileVpMenu_);

		objVpMenu_ = new Menu("Perspective Views ");
		mi = new MenuItem("+X");
		mi.addActionListener(this);
		mi.setActionCommand(objVpAction + 0);
		objVpMenu_.add(mi);
		mi = new MenuItem("+Y");
		mi.addActionListener(this);
		mi.setActionCommand(objVpAction + 1);
		objVpMenu_.add(mi);
		mi = new MenuItem("+Z");
		mi.addActionListener(this);
		mi.setActionCommand(objVpAction + 2);
		objVpMenu_.add(mi);
		m.add(objVpMenu_);
		
		mi = new MenuItem(resetViewpoint);
		mi.addActionListener(this);
		m.add(mi);
		mindsEyeMenu_.add(m);

		mi = new MenuItem("WorldInfo");
		mi.addActionListener(this);
		mindsEyeMenu_.add(mi);
		
		// J3D User interface stuff
		m = new Menu("Mode");
		mi = new MenuItem("Examine");
		mi.addActionListener(this);
		mi.setActionCommand(modeAction + MindsEye.EXAMINE);
		m.add(mi);
		mi = new MenuItem("Fly");
		mi.setActionCommand(modeAction + MindsEye.FLY);
		mi.addActionListener(this);
		m.add(mi);
		mindsEyeMenu_.add(m);
		
		// Minds eye options
		m = new Menu("Options");
		hlCheck = new CheckboxMenuItem(HEADLIGHT, true);
		hlCheck.addItemListener(this);
		m.add(hlCheck);

		// one option is to view the Graph in the right window
		viewGraphCheck_ = new CheckboxMenuItem(VIEW_GRAPH, startWithGraph_);
		viewGraphCheck_.addItemListener(this);
		m.add(viewGraphCheck_);
		mindsEyeMenu_.add(m);

		// another option is to view the co-ordinate axes system
		viewAxesCheck_ = new CheckboxMenuItem(VIEW_AXES, false);
		viewAxesCheck_.addItemListener(this);
		m.add(viewAxesCheck_);
		mindsEyeMenu_.add(m);

		menuBar_.add(mindsEyeMenu_);

		// Text Viewer Menu
		textViewerMenu_ = new Menu("Text Viewer");
		menuBar_.add(textViewerMenu_);

		Menu optionsMenu = new Menu("Options");

		// COLOURS

		m = new Menu("Colours");
		darkColorsCheck_ = 
			new CheckboxMenuItem(DARK_COLORS, defaultDarkColors_);
		darkColorsCheck_.addItemListener(this);
		m.add(darkColorsCheck_);
		optionsMenu.add(m);

		// VERBOSE?

		verboseCheck_ = 
			new CheckboxMenuItem(VERBOSE, defaultVerbose_);
		verboseCheck_.addItemListener(this);
		optionsMenu.add(verboseCheck_);
		menuBar_.add(optionsMenu);

		// Info Menu

		m = new Menu("Info");
		mi = new MenuItem(ABOUT);
		mi.addActionListener(this);
		m.add(mi);
		menuBar_.add(m);

		setMenuBar(menuBar_);

		setColors(defaultDarkColors_);
		
		activateMindsEye();
		
		// Setup the size and location to display the Imagine Frame

		// Settings for full screen size
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		//screenSize.height -= 15;
		setSize(screenSize);
		//setSize(800,600);
		setLocation(0,0);
	}

	public void setColors(boolean dark) {
		Color foreground = Color.green;
		Color background = Color.black;
		Color background2 = Color.black;

		if (! dark) {
			foreground = Color.black;
			//background = GOLD_YELLOW;
			//background2 = LIGHT_BLUE;
			background = Color.white;
			background2 = Color.white;
		}
		
		// the background panel will always be black
		setForeground(Color.black);
		setBackground(Color.black);
		
		prompt_.setColors(foreground, background);
		mindsEye_.setColors(foreground, background2);
		textViewer_.setColors(foreground, background2);
		hubViewer_.setColors(foreground, background2);
	}

	public CommandPromptInterface getPrompt() {
		return prompt_;
	}

	public void viewMemory(String name) {
		hubViewer_.update(name);
	}

	public void activateDecoder(BlobDecoder decoder) {
		if (decoder == mindsEye_) {
			activateMindsEye();
		} else if (decoder == textViewer_) {
			activateTextViewer();
		} else {
			Control.error("decoder does not exist");
		}
	}

	void activateMindsEye() {
		if (getComponent(1) != visualBufferCanvas_) {	
			remove(1);
			add(visualBufferCanvas_, BorderLayout.CENTER, 1);
		}
		textViewerMenu_.setEnabled(false);
		mindsEyeMenu_.setEnabled(true);
		pack();
	}

	void activateTextViewer() {
		if (getComponent(1) != textViewer_) {
			remove(1);
			add(textViewer_, BorderLayout.CENTER, 1);
			textViewer_.setVisible(true);
		}
		mindsEyeMenu_.setEnabled(false);
		textViewerMenu_.setEnabled(true);
		pack();
	}

    public void actionPerformed(ActionEvent evt) {
		if (evt.getSource() instanceof MenuItem) {
			String arg = evt.getActionCommand();
			if (arg.equals(openVRML)) {
				activateMindsEye();
				fd.show();
				String file = fd.getFile();
				if (file == null) {	// null if cancel button is hit
					return;
				}
				String fileName = fd.getDirectory() + file;
				loadURL(fileName);
			} else if (arg.equals("Exit")) {
				mindsEye_.exit();
				Control.exit();
			} else if (arg.equals(addText)) {
				activateTextViewer();
				textViewer_.setText("", "");
			} else if (arg.equals(addFile)) {
				fd.show();
				String file = fd.getFile();
				if (file == null) {	// null if cancel button is hit
					return;
				}
				String fileName = fd.getDirectory() + file;
				String name = Control.askQuestion(
					"Enter a name for this file", file);
				FileBlob fileBlob = 
					new FileBlob(name, fileDecoder_, fileName);
			} else if (arg.equals(ABOUT)) {
				Control.status(Control.FULL_NAME + 
					" is a cognitive model of mental imagery" + 
					"that includes dynamics and algorithms for visual " + 
					"analogy");
			} else if (arg.startsWith(fileVpAction)) {
				activateMindsEye();
				String indexString = arg.substring(fileVpAction.length());
				int index = Integer.valueOf(indexString).intValue();
				mindsEye_.setFileViewpoint(index);
			} else if (arg.startsWith(objVpAction)) {
				String indexString = arg.substring(objVpAction.length());
				int index = Integer.valueOf(indexString).intValue();
				mindsEye_.setObjViewpoint(index);
			} else if (arg.equals(resetViewpoint)) {
				mindsEye_.resetViewpoint();
			} else if (arg.startsWith(modeAction)) {
				String indexString = arg.substring(modeAction.length());
				int index = Integer.valueOf(indexString).intValue();
				mindsEye_.setMode(index);
			} else {
				Control.error("Unknown action: " + arg);
			}
		}

    }

	public void itemStateChanged(ItemEvent evt) {
		// need to add a method to PublicBrowser to allow us to
		// enable and disable the headlight/ambient light
		// see: browserDirLight.setEnabled(true);
		if (evt.getItem().equals(HEADLIGHT)) {
			mindsEye_.setHeadlight(hlCheck.getState());
		} else if (evt.getItem().equals(VIEW_GRAPH)) {
			mindsEye_.setDrawGraph(viewGraphCheck_.getState());
		} else if (evt.getItem().equals(VIEW_AXES)) {
			mindsEye_.setDrawAxes(viewAxesCheck_.getState());
		} else if (evt.getItem().equals(VERBOSE)) {
			mindsEye_.setVerbose(verboseCheck_.getState());
		} else if (evt.getItem().equals(DARK_COLORS)) {
			setColors(darkColorsCheck_.getState());
			repaint();
		} else {
			System.out.println("Unknown item changed: " + evt);
		}
    }

	public void loadURL(String fileName) {
		// remove the old viewpoints from the file menu
		fileVpMenu_.removeAll();
		// nove remove any info associated with the old URL from 
		// the VRML mindsEye_
		mindsEye_.input(fileName);
			
		String[] viewDescs = mindsEye_.getFileViewDescriptions();
		if (viewDescs == null) { return; }
		for (int i=0; i<viewDescs.length; i++) {	
			mi = new MenuItem(viewDescs[i]);
			mi.addActionListener(this);
			mi.setActionCommand(fileVpAction + i);
			fileVpMenu_.add(mi);
		}
	}

	public void show() {
		super.show();
		prompt_.start();
    }
}
