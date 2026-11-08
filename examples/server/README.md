## Build JAR.

```bash
clojure -Srepro -T:build uber
```

## Run jar for prod

```bash
java \
  -Duser.timezone=UTC \
  -Dclojure.server.repl="{:port 5555 :accept clojure.core.server/repl}" \
  -XX:+UseZGC -XX:+UseCompactObjectHeaders \
  -jar target/app.jar prod
```

## Run jar for dev 

No https/h2.

```bash
java \
  -Duser.timezone=UTC \
  -Dclojure.server.repl="{:port 5555 :accept clojure.core.server/repl :server-daemon false}" \
  -XX:+UseZGC -XX:+UseCompactObjectHeaders \
  -Djdk.attach.allowAttachSelf \
  -XX:+EnableDynamicAgentLoading \
  -jar target/app.jar dev
```

## Run jar without http-server 

Can be useful for profiling and tests.

```bash
java \
  -Duser.timezone=UTC \
  -Dclojure.server.repl="{:port 5555 :accept clojure.core.server/repl :server-daemon false}" \
  -XX:+UseZGC -XX:+UseCompactObjectHeaders \
  -Djdk.attach.allowAttachSelf \
  -XX:+EnableDynamicAgentLoading \
  -jar target/app.jar test
```

## Deploy

Move JAR to server (this will trigger a service restart).

```bash
scp target/app.jar root@checkboxes.andersmurphy.com:/home/app/
```

## After deploying first jar

Optional: the first time you move the jar onto the server you will need to reboot to trigger/test systemd is working correctly.

```
ssh root@checkboxes.andersmurphy.com "reboot"
```

## SSH into repl

```bash
ssh root@checkboxes.andersmurphy.com "nc localhost:5555"
```

## Force Restart Service (optional)

```
ssh root@checkboxes.andersmurphy.com "systemctl restart app.service"
```

## View overruns

```bash
ssh root@checkboxes.andersmurphy.com "journalctl -u app --since '5 minutes ago' | grep -A 1 'WARNING: tick overrun'"
```
