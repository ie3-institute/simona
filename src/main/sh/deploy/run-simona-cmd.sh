# rung simona server side with special parameters

java -Xmx80g \
  -Xms40g \
  -Xss2048k \
  -XX:+UseParallelGC \
  -XX:+UseParallelOldGC \
  -XX:MetaspaceSize=150M \
  -Djava.awt.headless=true \
  -cp build/libs/simona-2.1-SNAPSHOT-all.jar \
  edu.ie3.simona.main.RunSimonaStandalone \
  "$@"

# cmds for remote control (requires firewall adaptions)
#  -Dcom.sun.management.jmxremote \
#  -Dcom.sun.management.jmxremote.rmi.port=9198 \
#  -Djava.rmi.server.hostname=129.217.187.244 \
#  -Dcom.sun.management.jmxremote.port=9198 \
#  -Dcom.sun.management.jmxremote.local.only=false \
#  -Dcom.sun.management.jmxremote.authenticate=false \
#  -Dcom.sun.management.jmxremote.ssl=false \

# cmd for additional logging config to be passed in
#  -Dlog4j.configurationFile=config/log4j2.xml \
