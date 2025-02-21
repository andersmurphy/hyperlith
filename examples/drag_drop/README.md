## Build JAR.

```bash
clojure -Srepro -T:build uber
```

## Run jar locally

```
java -Dclojure.server.repl="{:port 5555 :accept clojure.core.server/repl}" -jar target/app.jar -m app.main -Duser.timezone=UTC -XX:+UseZGC -XX:+ZGenerational
```
