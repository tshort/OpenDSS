@echo off
call make_jena_classpath.bat c:\jena-2.6.2
@echo on

call do_import.bat -p=c -e=i -f=50 -v=1 -s=1000      -q=y aigue_edf_v1-1-c15.xml aigue_edf
call do_import.bat -p=c -e=i -f=50 -v=1 -s=1000      -q=y c10_edf_v1-1-c15.xml c10_edf
call do_import.bat -p=c -e=u -f=60 -v=0.001 -s=0.001 -q=n circ_combined_50204_2011_03_29_1752_50.xml circ_combined
call do_import.bat -p=c -e=i -f=50 -v=1 -s=1000      -q=y rural_edf_v1-1-c15.xml rural_edf
call do_import.bat -p=c -e=i -f=50 -v=1 -s=1000      -q=y urban_edf_v1-1-c15.xml urban_edf

