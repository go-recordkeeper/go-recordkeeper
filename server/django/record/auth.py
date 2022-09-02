from django.conf import settings
from django.contrib.auth.models import User
import jwt
from rest_framework import authentication
from rest_framework import exceptions

class JWTAuthentication(authentication.BaseAuthentication):
    def authenticate(self, request):
        authorization = request.headers.get('Authorization')
        if (not authorization) or (not authorization.startswith('Bearer ')):
            return None
        token = authorization.removeprefix('Bearer ')

        try:
            payload = jwt.decode(token, key=settings.SECRET_KEY, algorithms='HS256')
        except jwt.InvalidTokenError:
            raise exceptions.AuthenticationFailed('invalid authorization token')
        if 'id' not in payload:
            raise exceptions.AuthenticationFailed('invalid authorization token')
        try:
            user = User.objects.get(id=payload['id'])
        except User.DoesNotExist:
            raise exceptions.AuthenticationFailed('invalid authorization token')

        return (user, None)