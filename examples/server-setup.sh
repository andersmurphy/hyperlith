#!/usr/bin/env bash
set -x
set -e

# Dependencies
apt-get update
apt-get upgrade
apt-get -y install openjdk-21-jre-headless ufw caddy 

# App user (you cannot login as this user)
useradd -rms /usr/sbin/nologin app

# Systemd service
cat > /etc/systemd/system/app.service << EOD
[Unit]
Description=app
StartLimitIntervalSec=500
StartLimitBurst=5
ConditionPathExists=/home/app/app.jar

[Service]
User=app
Restart=on-failure
RestartSec=5s
WorkingDirectory=/home/app
ExecStart=/usr/bin/java --add-opens=java.base/java.nio=ALL-UNNAMED --add-opens=java.base/sun.nio.ch=ALL-UNNAMED -Dclojure.server.repl="{:port 5555 :accept clojure.core.server/repl}" -jar app.jar -m app.main -Duser.timezone=UTC -XX:+UseZGC -XX:+ZGenerational -XX:InitialRAMPercentage 75.0 -XX:MaxRAMPercentage 75.0 -XX:MinRAMPercentage 75.0

[Install]
WantedBy=multi-user.target
EOD
systemctl enable app.service

cat > /etc/systemd/system/app-watcher.service << EOD
[Unit]
Description=Restarts app on jar upload
After=network.target

[Service]
ExecStart=/usr/bin/env systemctl restart app.service

[Install]
WantedBy=multi-user.target
EOD
systemctl enable app-watcher.service

cat > /etc/systemd/system/app-watcher.path << EOD
[Unit]
Wants=app-watcher.service

[Path]
PathChanged=/home/app/app.jar

[Install]
WantedBy=multi-user.target
EOD
systemctl enable app-watcher.path

# Firewall
ufw default deny incoming
ufw default allow outgoing
ufw allow OpenSSH
ufw allow 80
ufw allow 443
ufw --force enable

# Reverse proxy
rm /etc/caddy/Caddyfile
cat > /etc/caddy/Caddyfile << EOD
example.andersmurphy.com {
  reverse_proxy localhost:8080 {
    lb_try_duration 30s
    lb_try_interval 1s
  }
}
EOD

# Let's encrypt
systemctl daemon-reload
systemctl enable --now caddy

# ssh config
cat >> /etc/ssh/sshd_config << EOD
# Setup script changes
PasswordAuthentication no
PubkeyAuthentication yes
AuthorizedKeysFile .ssh/authorized_keys
EOD
systemctl restart ssh

