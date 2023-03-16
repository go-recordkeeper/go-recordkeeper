from drf_yasg import openapi
from drf_yasg.views import get_schema_view
from rest_framework import routers

from django.contrib import admin
from django.urls import include, path, re_path
from record.views import RecordViewSet, login_view, register_view, user_view

schema_view = get_schema_view(
    openapi.Info(
        title='Go Recordkeeper API',
        default_version='v1',
        contact=openapi.Contact(email='daniel.chiquito@gmail.com'),
        license=openapi.License(name='MIT'),
    ),
    public=True,
)

router = routers.DefaultRouter()
router.register(r'records', RecordViewSet)
# router.register(r'login', login_view)


urlpatterns = [
    path('api/', include(router.urls)),
    path('api/login/', login_view),
    path('api/register/', register_view),
    path('api/user/', user_view),
    path('api/admin/', admin.site.urls),
    re_path(
        r'^swagger(?P<format>\.json|\.yaml)$',
        schema_view.without_ui(cache_timeout=0),
        name='schema-json',
    ),
    re_path(
        r'^swagger/$', schema_view.with_ui('swagger', cache_timeout=0), name='schema-swagger-ui'
    ),
]
