from django.contrib.auth.models import User
from rest_framework import mixins, serializers, status, viewsets
from rest_framework.decorators import action
from rest_framework.response import Response

from record.go import Board, Stone
from record.models import Game, Move


class GameSerializer(serializers.ModelSerializer):
    class Meta:
        model = Game
        fields = ['id', 'owner', 'size']


class CreateGameSerializer(serializers.Serializer):
    size = serializers.IntegerField()


class MoveSerializer(serializers.ModelSerializer):
    class Meta:
        model = Move
        fields = ['x', 'y']


class GameViewSet(
    mixins.CreateModelMixin,
    mixins.RetrieveModelMixin,
    # mixins.UpdateModelMixin,
    # mixins.DestroyModelMixin,
    mixins.ListModelMixin,
    viewsets.GenericViewSet,
):
    queryset = Game.objects.all()
    serializer_class = GameSerializer

    def create(self, request, **kwargs):
        serializer = CreateGameSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        size = serializer.data['size']
        # TODO have an actual user
        game = Game(owner=User.objects.first(), size=size)
        game.save()
        serializer = GameSerializer(instance=game)
        return Response(serializer.data, status=status.HTTP_201_CREATED)

    def retrieve(self, request, **kwargs):
        game = self.get_object()
        last_move = game.last_move
        board: Board = last_move.board_state if last_move is not None else Board(game.size)
        update = {
            'add': [
                {'x': x, 'y': y, 'color': color.value} for (x, y), color in board.moves.items()
            ],
            'remove': [],
        }
        return Response(update, status=status.HTTP_200_OK)

    @action(methods=['POST'], detail=True)
    def play(self, request, **kwargs):
        game = self.get_object()
        move_serializer = MoveSerializer(data=request.data)
        move_serializer.is_valid(raise_exception=True)
        move = game.next_move(**move_serializer.validated_data)

        # TODO filter passes
        moves = (
            (Stone(color), x, y) for (color, x, y) in game.moves.values_list('color', 'x', 'y')
        )
        replay = Board(game.size)
        for (stone, x, y) in moves:
            replay.place_stone(stone, x, y)
        removals = replay.place_stone(Stone(move.color), move.x, move.y)
        print(removals)
        # No problems placing the stone, save it
        move.save()

        update = {
            'add': [{'x': move.x, 'y': move.y, 'color': move.color}],
            'remove': [{'x': x, 'y': y} for (x, y) in removals],
        }

        return Response(update, status=status.HTTP_201_CREATED)
