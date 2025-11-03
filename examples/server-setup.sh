#!/usr/bin/env bash
set -x
set -e

# Needed because Ubuntu 24.04 doesn't have java 23+
add-apt-repository ppa:openjdk-r/ppa 
# Dependencies
apt-get update
apt-get upgrade
apt-get -y install openjdk-23-jre-headless ufw caddy

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
ExecStart=/usr/bin/java -Dclojure.server.repl="{:port 5555 :accept clojure.core.server/repl}" -jar app.jar -m app.main -Duser.timezone=UTC -XX:+UseZGC -XX:InitialRAMPercentage 75.0 -XX:MaxRAMPercentage 75.0 -XX:MinRAMPercentage 75.0 -Djdk.attach.allowAttachSelf

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
  header -Server

  handle_errors 5xx {
     abort
  }
  reverse_proxy localhost:8080 {
    
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

