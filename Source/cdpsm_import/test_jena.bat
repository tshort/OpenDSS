@echo off

call make_jena_classpath.bat c:\jena-2.6.2

java -classpath .;%JENA_CP% junit.textui.TestRunner com.hp.hpl.jena.test.TestPackage
