from os import environ

POSTGRES_USER = environ['POSTGRES_USER']
POSTGRES_PASSWORD = environ['POSTGRES_PASSWORD']
POSTGRES_NAME = environ['POSTGRES_NAME']
POSTGRES_HOST = environ['POSTGRES_HOST']

if environ.get('GOBAN_DEVELOPMENT', False):
    SECRET_KEY = 'django-insecure-(@ppnpk$wx_z%2^#^0sext&+%b58=%e^!_u_*yd2p#d2&9)9cj'
else:
    SECRET_KEY = environ['GOBAN_SECRET_KEY']

__all__ = [
    'POSTGRES_USER',
    'POSTGRES_PASSWORD',
    'POSTGRES_NAME',
    'POSTGRES_HOST',
    'SECRET_KEY',
]