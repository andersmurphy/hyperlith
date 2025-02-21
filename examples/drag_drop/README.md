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
scp target/app.jar example.andersmurphy.com:/home/app/
```

## SSH into repl

```bash
ssh example.andersmurphy.com "nc localhost:5555"
```
