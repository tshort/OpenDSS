@echo off

rem call with -p=c -e=u -f=60 -v=1 -s=0.001 -q=y instance.xml output_root

call make_jena_classpath.bat c:\jena-2.6.2

java -classpath .;%JENA_CP% CDPSM_to_DSS %1 %2 %3 %4 %5 %6 %7 %8

