from django.contrib import admin
from django.urls import include, path
from rest_framework import routers

from record.views import GameViewSet

router = routers.DefaultRouter()
router.register(r'games', GameViewSet)


urlpatterns = [
    path('', include(router.urls)),
    path('admin/', admin.site.urls),
]
