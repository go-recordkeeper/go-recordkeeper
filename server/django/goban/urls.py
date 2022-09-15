from django.contrib import admin
from django.urls import include, path
from rest_framework import routers

from record.views import RecordViewSet, login_view, register_view, user_view

router = routers.DefaultRouter()
router.register(r'records', RecordViewSet)
# router.register(r'login', login_view)


urlpatterns = [
    path('api/', include(router.urls)),
    path('api/login/', login_view),
    path('api/register/', register_view),
    path('api/user/', user_view),
    path('api/admin/', admin.site.urls),
]
