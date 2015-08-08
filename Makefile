SBT_JAVA_OPT=-J-Xss64m -J-Xms64m -J-Xmx512m -J-XX:MaxPermSize=256m

all: icfp

icfp:
	@sbt -verbose -debug ${SBT_JAVA_OPT} assembly
	@find . -type f -name icfp-2015.jar -exec mv --force {} . \; 
clean:
	@rm -f icfp-2015.jar
	@sbt clean
