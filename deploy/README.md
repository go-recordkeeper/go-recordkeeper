# goban-deploy

## Building

Building the web app takes a disastrously long time on a raspberry pi, so you should build static assets on a dev machine and commit dist to the repo:
```
git submodule update --remote
./build.sh
git add dist
git commit
```

## First deploy
```
git submodule init
git submodule update --remote
./generate-secret-key.sh
docker-compose build
docker-compose run django ./manage.py migrate
docker-compose run django ./manage.py createsuperuser
docker-compose up -d
```

## Redeploying
```
docker-compose stop
git submodule update --remote
# If any new pip packages are required
docker-compose build --no-cache
# If any database schema changes were made
docker-compose run django ./manage.py migrate
docker-compose up -d
```

## systemd
Rather than using `docker-compose up -d` to start a backup process, you can instead use the included systemd service. This has the advantage of automatically restarting the service should it crash, and starting the service automatically when the server boots.

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

You can use either `journalctl -fu goban` or `docker-compose logs` to check the service logs.
