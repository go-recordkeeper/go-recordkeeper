# go-recordkeeper

## Initialize dev environment
```
docker-compose build
docker-compose run --rm django ./manage.py migrate
docker-compose run --rm django ./manage.py createsuperuser
```
