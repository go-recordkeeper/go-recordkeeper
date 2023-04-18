# goban-deploy

Everything needed to actually deploy the app.

I only plan on very light usage, so the app is deployed using docker compose on a Raspberry Pi.

## Building

There are two major components that are deployed: the server application, and the static assets. Static assets include the bundled web app, the Django admin console, and the blog.

Building the static assets takes a disastrously long time on a raspberry pi, so instead they are build on a dev machine and committed to the repo:
```
./build.sh
git add dist
git commit
```

You can also build specific parts using `build-blog.sh`, `build-django.sh`, and `build-vue.sh`.

## First deploy
```
./generate-secret-key.sh
docker compose build
docker compose run django ./manage.py migrate
docker compose run django ./manage.py createsuperuser
docker compose up -d
```

## Redeploying
```
docker compose stop
git submodule update --remote
# If any new pip packages are required
docker compose build --no-cache
# If any database schema changes were made
docker compose run django ./manage.py migrate
docker compose up -d
```

## systemd
Rather than using `docker compose up -d` to start a backup process, you can instead use the included systemd service. This has the advantage of automatically restarting the service should it crash, and starting the service automatically when the server boots.

```
# Install the goban.service file to /etc/systemd/system/
./install-service.sh
# Force systemd to reload system configurations
sudo systemctl daemon-reload
# Run on boot
sudo systemctl enable goban
# Start the service
sudo systemctl start goban
# Check on the status
sudo systemctl status goban
```

You can use either `journalctl -fu goban` or `docker compose logs` to check the service logs.

# Backups
The `pg_dump.sh` and `pg_load_sh` scripts will, respectively, backup and restore the database to a SQL file.

```sh
# Create a backup file
./pg_dump.sh ~/my_backup.sql
# Restore that file
# This drops all data currently in the DB, so be careful
./pg_load.sh ~/my_backup.sql
```

These scripts use `docker compose` to run postgres commands, so if they are invoked in other folders, they can be used to backup/restore those docker compose configurations instead.
