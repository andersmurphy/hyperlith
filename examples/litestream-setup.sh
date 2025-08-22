wget https://github.com/benbjohnson/litestream/releases/download/v0.5.0-test.2/litestream-v0.5.0-test.2-linux-amd64.deb

sudo dpkg -i litestream-v0.5.0-test.2-linux-amd64.deb

sudo cat > /etc/litestream.yml <<EOF
access-key-id:     XXXXXXXXXXXXXX
secret-access-key: XXXXXXXXXXXXXX
dbs:
  - path: /home/app/database-new.db
    replicas:
      - type: s3
        bucket:   hyperlith
        path:     database-new.db
        endpoint: https://nbg1.your-objectstorage.com
        region:   nbg1
        sync-interval: 1s
EOF

sudo systemctl restart litestream

sudo journalctl -u litestream -f
