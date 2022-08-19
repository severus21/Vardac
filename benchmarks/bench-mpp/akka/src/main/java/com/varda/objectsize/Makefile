default: clean
	javac InstrumentationAgent.java
	jar cmf MANIFEST.MF InstrumentationAgent.jar InstrumentationAgent.class

clean:
	rm -f *.jar *.class