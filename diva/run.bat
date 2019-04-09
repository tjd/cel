@echo See note in run.bat if you get a "no J3D in java.library.path" exception

@rem Here is the promised note:
@rem If you are getting a "no J3D in java.library.path" exception
@rem you might want to try manually setting the library path like this:
@rem java -Djava.library.path=c:\java\jdk1.3\jre\bin cogsci.Control %1
@rem (here the Java installation is c:\java\jdk1.3)

@rem java cogsci.Control %1
