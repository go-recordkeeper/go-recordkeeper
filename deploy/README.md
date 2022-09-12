# goban-deploy

```
./pull.sh
./build-vue.sh
docker-compose build
docker-compose run --rm django ./manage.py migrate
docker-compose run --rm django ./manage.py createsuperuser
```