from datetime import datetime, timedelta, timezone

import jwt
from rest_framework import authentication, exceptions

from django.conf import settings
from django.contrib.auth.models import User


def generate_token(user: User):
    now = datetime.now(tz=timezone.utc)
    return jwt.encode(
        {
            'sub': str(user.id),
            'iat': now,
            'exp': now + timedelta(days=1),
            'iss': 'go-recordkeeper',
            'aud': 'go-recordkeeper',
        },
        key=settings.SECRET_KEY,
        algorithm='HS256',
    )


class JWTAuthentication(authentication.BaseAuthentication):
    def authenticate(self, request):
        authorization = request.headers.get('Authorization')
        if (not authorization) or (not authorization.startswith('Bearer ')):
            return None
        token = authorization.removeprefix('Bearer ')

        try:
            payload = jwt.decode(
                token,
                key=settings.SECRET_KEY,
                algorithms='HS256',
                audience='go-recordkeeper',
                issuer='go-recordkeeper',
                leeway=2,
            )
        except jwt.InvalidTokenError:
            raise exceptions.AuthenticationFailed('invalid authorization token')
        if 'sub' not in payload:
            raise exceptions.AuthenticationFailed('invalid authorization token')
        try:
            user = User.objects.get(id=payload['sub'])
        except User.DoesNotExist:
            raise exceptions.AuthenticationFailed('invalid authorization token')

        return (user, None)
