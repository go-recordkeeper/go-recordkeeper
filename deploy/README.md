# goban-deploy

```
./pull.sh
# This takes a disastrously long time on a raspberry pi, so you can alternatively used the contents committed to the repo
./build-vue.sh
./generate-secret-key.sh
docker-compose build
docker-compose run --rm django ./manage.py migrate
docker-compose run --rm django ./manage.py createsuperuser
docker-compose up -d
```
