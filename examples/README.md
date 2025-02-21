# Server setup and management

## Add a record to DNS

Add an A record @ which points to the IPV4 address of the VPS. For IPV6 add a AAAA record @ which points to the IPV6 address of the VPS.

## Initial server setup

Move the setup script to the server:

```bash
scp server-setup.sh root@XXX.XXX.XXX.XXX:
```

ssh into server as root:

```bash
ssh root@XXX.XXX.XXX.XXX
```

run bash script:

```bash
bash server-setup.sh
```

follow instructions.

## After deploying first jar

Optional: the first time you move the jar onto the server you will need to reboot to trigger/test systemd is working correctly.

```
ssh root@XXX.XXX.XXX.XXX "reboot"
```

## Caddy service

Check status:

```bash
systemctl status caddy
```

Reload config without downtime.

```bash
systemctl reload caddy
```

Docs: https://caddyserver.com/docs/running#using-the-service

## Useful systemd commands

Check status of service.

```bash
systemctl status app.service
```

Restart service manually:

```bash
systemctl restart app.service
```
