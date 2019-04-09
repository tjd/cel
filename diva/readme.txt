README for the DIVA computer model 
David Croft, Oct. 12, 2000. University of Waterloo

Contents

- Legal
- Files contained in the diva.zip file
- Hardware and Software requirements (Java version required, installing the 
  Java3D library, setting the CLASSPATH environment variable
- Setting up the path to the VRML models directory
- Quick intro to using the DIVA program
- Problems...

Legal

The (c) copyright for this software is held be David Croft. Please see the 
license at: http://zooid.org/~tforcdivad/license.txt. It was developed at
the University of Waterloo under the supervision of Paul Thagard.

Files

After unzipping the DIVA zip file the directory in which you unzip should have
the following files and folders:

readme.txt (this file)
todo.txt (limitations and where the model needs improvement)
cogsci/ (main source code tree for DIVA)
util/	(utility code required by DIVA)

Requirements

- Hardware

Much of the development for the DIVA model was done with a Pentium 120
computer with 32 megs of RAM. Anything slower than this or with less than 16
megs of RAM might have problems with JAVA's virtual machine. The DIVA model
should work with most Windows and Unix (Solaris or Linux) machines. Note that
it does not currently work for Macs due to JDK software requirement described
below.

- Software

The current version of the DIVA model was developed and tested with JDK 1.3
from JavaSoft. Unfortunately, despite Java's cross-platform claims, this is
currently only available for Windows and Unix systems, I believe MacIntosh 
(Apple) only has a 1.1 release of the JDK. Windows and Unix JDKs (Java 
Development Kits) are available from http://java.sun.com

In addition to the Java Development Kit (JDK), DIVA requires Java3D 1.2.
Again, despite Java's cross-platform claims, Java3D seems to target the
Windows platform. The Java3D library and documentation can be downloaded from
http://java.sun.com/products/java-media/3D/index.html

The Java3D library is basically two DLL's in your $JDK_HOME/jre/bin directory,
as well as 4 jar files in the folder $JDK_HOME/jre/lib/ext. The jar files are
j3daudio.jar, j3dcore.jar, j3dutils.jar, and vecmath.jar. (These are should be
automatically installed by the Java3D self-extractor that you download).

Finally, the DIVA model requires the VRML97 library to parse VRML files as
input into the model (VRML97 files are loaded into the model whenever the
program is executed). At the time I wrote this readme file, VRML97 seems 
to have been renamed X3D, and is available for download at:
http://www.web3d.org/TaskGroups/x3d/sun/cvs.html

From this download site you should obtain a file called x3d.zip, which
contains a file called x3d.jar. This jar file needs to be place in the folder 
$JDK_HOME/jre/lib/ext along with the Java3D jar files.

Note, one very common problem I have had setting up Java for command-line use
on the Windows platfrom is that located in the windows folder there is the
microsoft java interpreter (c:\windows\java.exe). Typing java invokes the
microsoft virtual machine, which of course has problems as it is not the JDK
1.3 release. I usually just delete the java.exe program, however you can also
re-arrange the PATH environment variable such that $JDK_HOME/bin is searched
before the c:\windows folder. An example PATH setting in autoexec.bat would
look something like:

SET PATH=.;c:\jdk1.3\bin;c:\windows;c:\windows\command

In order for Java to locate the DIVA class files, you will have to set the
CLASSPATH environment folder to look at the folder in which you unzipped the
DIVA zip file. For example, if you unzipped the DIVA zip file to c:\diva, then
the folder c:\diva should contain this file (readme.txt) and two more
folders, cogsci and util. The CLASSPATH setting in your autoexec.bat would
look something like:

SET CLASSPATH=.;c:\diva

The CLASSPATH environment variable tells the Java virtual machine what folders
serve as the root for Java's packaging convention.

VRML Models directory

The directory containing the path to the VRML models is hard-coded in the 
file cogsci\imagery\MindsEye.java. The path is currently set to search for
models in the directory "./cogsci/imagery/models/", so that running the 
program from the directory in which it is unzipped should work (for example,
if you unzipped into the folder c:\diva, and changing the directory to c:\diva
and using the run batch file (which basically calls java cogsci.Control)
will search for models in "c:\diva\cogsci\imagery\models", which is where they
are located.

Using the model

If everything is installed correctly, you can run the program by typing 
java cogsci.Control on the command line (MS-DOS on Windows). Or you can type
"run" if you are in the folder in which the model was unzipped into (c:\diva 
in my install example above). 

The DIVA user interface should soon appear, and you can start entering
commands in the top portion of the user-interface. For a complete list of
commands, type "help". For specific information about a command type "help"
and the name of the command. For example, for help about the compare command
(used to generate establish analogical mappings between visual concepts) you
would type "help compare").

You can inspect some of the default visual concepts with the following
commands:
"viewmem arch1" (basic scene containing an arch shape)
"viewmem gearbox" (animated set of gears)
"viewmem snowman" (a snowman)

To interact with the visual buffer portion of the user-interface (the bottom
left panel where you see the 3-dimensional scene) use the mouse. Holding down
the left mouse button rotates the scene, holding down the right button zooms
in/out when you move the mouse right/left, and moves the scene up/down when
you move the mouse up/down.

The right panel of the user-interface allows you to browse through the
concepts stored in long-term memory. Clicking on a node moves it to the center
of the long-term memory viewer, and displays all the semantic associations for
that concept. You can add semantic associations between concepts using
"associate" command. For example:

"associate dog isa mammal"

would establish a dog concept, a mammal concept and a link between them
joined by the isa concept. In the folder cogsci folder (eg. c:\diva\cogsci\)
there are a few example scripts containing semantic associations that can be
loaded into the model. The "runscript" command enables you to read in a file
containing a list of commands. This allows you to fill a file with associate
commands that can batch-executed by entering the command "runscript fileName".

Finally, you should play around with DIVA's visual analogy algorithm, which is
executed using the compare command. For example, try:
"compare arch1 arch2"
"compare snowman stoneman"
"compare siege tumorproblem"


Problems

If you are getting the following exception from the Java VM:

java.lang.UnsatisfiedLinkError: no J3D in java.library.path

You might want to try manually setting the library directory using the
java.exe's -D flag. For example:

java -Djava.library.path=c:\jdk1.3\jre\bin cogsci.Control

This tells java to look for the j3d.dll in c:\jdk1.3\jre\bin
