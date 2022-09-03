from django.contrib import admin
from django.urls import include, path
from rest_framework import routers

from record.views import GameViewSet, login_view, register_view, user_view

router = routers.DefaultRouter()
router.register(r'games', GameViewSet)
# router.register(r'login', login_view)


urlpatterns = [
    path('', include(router.urls)),
    path('login/', login_view),
    path('register/', register_view),
    path('user/', user_view),
    path('admin/', admin.site.urls),
]
