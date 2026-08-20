#!/usr/bin/env bash
set -x
set -e

# Dependencies
apt-get update
apt-get upgrade
apt-get -y install openjdk-25-jre-headless ufw

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
AmbientCapabilities=CAP_NET_BIND_SERVICE
CapabilityBoundingSet=CAP_NET_BIND_SERVICE
WorkingDirectory=/home/app
ExecStart=/usr/bin/java -Dclojure.server.repl="{:port 5555 :accept clojure.core.server/repl}" -jar app.jar -m app.main -Duser.timezone=UTC -XX:+UseZGC -XX:InitialRAMPercentage 75.0 -XX:MaxRAMPercentage 75.0 -XX:MinRAMPercentage 75.0 -XX:+UseCompactObjectHeaders

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

# ssh config
cat >> /etc/ssh/sshd_config << EOD
# Setup script changes
PasswordAuthentication no
PubkeyAuthentication yes
AuthorizedKeysFile .ssh/authorized_keys
EOD
systemctl restart ssh
