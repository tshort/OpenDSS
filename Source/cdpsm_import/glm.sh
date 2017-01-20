javac -classpath "/Users/mcde601/src/apache-jena-3.1.0/lib/*:/Users/mcde601/src/commons-math3-3.6.1/*" CDPSM_to_GLM.java

java -classpath ".:/Users/mcde601/src/apache-jena-3.1.0/lib/*:/Users/mcde601/src/commons-math3-3.6.1/*" CDPSM_to_GLM -l=1 -t=y -e=u -f=60 -v=1 -s=1 -q=y ieee13.xml ieee13
java -classpath ".:/Users/mcde601/src/apache-jena-3.1.0/lib/*:/Users/mcde601/src/commons-math3-3.6.1/*" CDPSM_to_GLM -l=1 -t=y -e=u -f=60 -v=1 -s=1 -q=y ieee13_assets.xml ieee13_assets
java -classpath ".:/Users/mcde601/src/apache-jena-3.1.0/lib/*:/Users/mcde601/src/commons-math3-3.6.1/*" CDPSM_to_GLM -l=0.2 -t=y -e=u -f=60 -v=1 -s=1 -q=y ieee8500.xml ieee8500
java -classpath ".:/Users/mcde601/src/apache-jena-3.1.0/lib/*:/Users/mcde601/src/commons-math3-3.6.1/*" CDPSM_to_GLM -l=0.2 -t=n -e=u -f=60 -v=1 -s=1 -q=y ieee8500.xml ieee8500p
java -classpath ".:/Users/mcde601/src/apache-jena-3.1.0/lib/*:/Users/mcde601/src/commons-math3-3.6.1/*" CDPSM_to_GLM -l=0.2 -t=y -e=u -f=60 -v=1 -s=1 -q=y ieee8500u.xml ieee8500u

#java -classpath ".:/Users/mcde601/src/apache-jena-3.1.0/lib/*:/Users/mcde601/src/commons-math3-3.6.1/*" CDPSM_to_GLM -t=y -e=u -f=60 -v=1 -s=1 -q=y IEEE13NodecktAssets_CDPSM_Combined.XML ieee13assets

# 4BusYYbal_CDPSM_Combined.XML ieee4yy
# 4busDYBal_CDPSM_Combined.XML ieee4dy
# 4busOYODBal_CDPSM_Combined.XML ieee4oyod
# ieee4OYOD-ubal_CDPSM_Combined.XML ieee4oyodu
# 4busYDBal_CDPSM_Combined.XML ieee4yd
# DGProtFdr_CDPSM_Combined.XML ieeeDG
# IEEE_30_CDPSM_Combined.XML ieee30
# NEV_CDPSM_Combined.XML ieeeNEV
# ieee123_CDPSM_Combined.XML ieee123
# ieee34-2_CDPSM_Combined.XML ieee34
# ieee37_CDPSM_Combined.XML ieee37
# ckt5_CDPSM_Combined.XML epri5
# ckt7_CDPSM_Combined.XML epri7
# ckt24_CDPSM_Combined.XML epri24

