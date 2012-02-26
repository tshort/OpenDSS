@echo off
call make_jena_classpath.bat c:\jena-2.6.2
@echo on

javac -classpath .;%JENA_CP% CDPSM_to_DSS.java
