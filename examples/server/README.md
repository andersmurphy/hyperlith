## Build JAR.

```bash
clojure -Srepro -T:build uber
```

## Run jar locally

```bash
java \
  -Duser.timezone=UTC \
  -Dclojure.server.repl="{:port 5555 :accept clojure.core.server/repl}" \
  -XX:+UseZGC -XX:+UseCompactObjectHeaders \
  -jar target/app.jar -m app.main
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
