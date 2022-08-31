from rest_framework import serializers, status, viewsets
from rest_framework.decorators import action
from rest_framework.response import Response

from record.models import Game, Move


class GameSerializer(serializers.ModelSerializer):
    class Meta:
        model = Game
        fields = ['id', 'owner']


class MoveSerializer(serializers.ModelSerializer):
    class Meta:
        model = Move
        fields = ['x', 'y', 'color']


class GameViewSet(viewsets.ModelViewSet):
    queryset = Game.objects.all()
    serializer_class = GameSerializer

    @action(methods=['GET'], detail=True)
    def play(self, request, **kwargs):
        game = self.get_object()
        move_serializer = MoveSerializer(data=request.data)
        move_serializer.is_valid(raise_exception=True)
        move = Move(**move_serializer.validated_data)
        print(move)
        return Response({'foo': 'bar'}, status=status.HTTP_201_CREATED)
