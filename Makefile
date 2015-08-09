SBT_JAVA_OPT=-J-Xss64m -J-Xms64m -J-Xmx512m -J-XX:MaxPermSize=256m

all: icfp

icfp:
	@sbt -verbose -debug ${SBT_JAVA_OPT} assembly
	@find target -type f -name icfp2015-movio.jar -exec mv --force {} . \;
clean:
	@rm -f *.jar
	@sbt clean
