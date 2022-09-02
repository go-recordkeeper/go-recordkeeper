from django.conf import settings
from django.contrib.auth.models import User
from django.core.exceptions import PermissionDenied
import jwt

def jwt_middleware(get_response):
    def middleware(request):
        authorization = request.headers.get('Authorization', '')
        if authorization.startswith('Bearer '):
            token = authorization.removeprefix('Bearer ')
            try:
                payload = jwt.decode(token, key=settings.SECRET_KEY, algorithms='HS256')
            except jwt.InvalidTokenError:
                raise PermissionDenied('invalid authorization token')
            if 'id' not in payload:
                raise PermissionDenied('invalid authorization token')
            try:
                user = User.objects.get(id=payload['id'])
            except User.DoesNotExist:
                raise PermissionDenied('invalid authorization token')
            request.user = user
        
        response = get_response(request)
        return response
    return middleware