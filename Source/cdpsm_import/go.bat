set JENA_HOME=c:\apache-jena-3.1.0
set CLASSPATH=.;c:/apache-jena-3.1.0/lib/*

javac CDPSM_to_DSS.java

java CDPSM_to_DSS -e=u -f=60 -v=0.001 -s=0.001 -q=y IEEE13.XML ieee13
java CDPSM_to_DSS -e=u -f=60 -v=0.001 -s=0.001 -q=y IEEE13_Assets.XML ieee13assets
java CDPSM_to_DSS -e=u -f=60 -v=0.001 -s=0.001 -q=y IEEE8500u.XML ieee8500u
java CDPSM_to_DSS -e=u -f=60 -v=0.001 -s=0.001 -q=y IEEE8500.XML ieee8500

rem java CDPSM_to_DSS -e=u -f=60 -v=0.001 -s=0.001 -q=y 4BusYYbal_CDPSM_Combined.XML ieee4yy
rem java CDPSM_to_DSS -e=u -f=60 -v=0.001 -s=0.001 -q=y 4busDYBal_CDPSM_Combined.XML ieee4dy
rem java CDPSM_to_DSS -e=u -f=60 -v=0.001 -s=0.001 -q=y 4busOYODBal_CDPSM_Combined.XML ieee4oyod
rem java CDPSM_to_DSS -e=u -f=60 -v=0.001 -s=0.001 -q=y ieee4OYOD-ubal_CDPSM_Combined.XML ieee4oyodu
rem java CDPSM_to_DSS -e=u -f=60 -v=0.001 -s=0.001 -q=y 4busYDBal_CDPSM_Combined.XML ieee4yd
rem java CDPSM_to_DSS -e=u -f=60 -v=0.001 -s=0.001 -q=y DGProtFdr_CDPSM_Combined.XML ieeeDG
rem java CDPSM_to_DSS -e=u -f=60 -v=0.001 -s=0.001 -q=y IEEE_30_CDPSM_Combined.XML ieee30
rem java CDPSM_to_DSS -e=u -f=60 -v=0.001 -s=0.001 -q=y NEV_CDPSM_Combined.XML ieeeNEV
rem java CDPSM_to_DSS -e=u -f=60 -v=0.001 -s=0.001 -q=y ieee123_CDPSM_Combined.XML ieee123
rem java CDPSM_to_DSS -e=u -f=60 -v=0.001 -s=0.001 -q=y ieee34-2_CDPSM_Combined.XML ieee34
rem java CDPSM_to_DSS -e=u -f=60 -v=0.001 -s=0.001 -q=y ieee37_CDPSM_Combined.XML ieee37
rem java CDPSM_to_DSS -e=u -f=60 -v=0.001 -s=0.001 -q=y ckt5_CDPSM_Combined.XML epri5
rem java CDPSM_to_DSS -e=u -f=60 -v=0.001 -s=0.001 -q=y ckt7_CDPSM_Combined.XML epri7
rem java CDPSM_to_DSS -e=u -f=60 -v=0.001 -s=0.001 -q=y ckt24_CDPSM_Combined.XML epri24


