## Build JAR.

```bash
clojure -Srepro -T:build uber
```

## Run jar locally

```
java -Dclojure.server.repl="{:port 5555 :accept clojure.core.server/repl}" -jar target/app.jar -m app.main -Duser.timezone=UTC -XX:+UseZGC -XX:+ZGenerational
```

## Deploy

Move JAR to server (this will trigger a service restart).

```bash
scp target/app.jar root@example.andersmurphy.com:/home/app/
```

## After deploying first jar

Optional: the first time you move the jar onto the server you will need to reboot to trigger/test systemd is working correctly.

```
ssh root@example.andersmurphy.com "reboot"
```

## SSH into repl

```bash
ssh root@example.andersmurphy.com "nc localhost:5555"
```

