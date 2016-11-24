set JENA_HOME=c:\apache-jena-3.1.0
set CLASSPATH=.;c:/apache-jena-3.1.0/lib/*;c:/commons-math3-3.6.1/*

javac CDPSM_to_GLM.java

java CDPSM_to_GLM -p=c -e=u -f=60 -v=0.001 -s=0.001 -q=y IEEE13.XML ieee13
java CDPSM_to_GLM -p=c -e=u -f=60 -v=0.001 -s=0.001 -q=y IEEE13_Assets.XML ieee13assets
java CDPSM_to_GLM -p=c -e=u -f=60 -v=0.001 -s=0.001 -q=y IEEE8500u.XML ieee8500u
java CDPSM_to_GLM -p=c -e=u -f=60 -v=0.001 -s=0.001 -q=y IEEE8500.XML ieee8500

