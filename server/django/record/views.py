from django.contrib.auth import authenticate
from django.contrib.auth.models import User
from rest_framework import mixins, serializers, status, viewsets
from rest_framework.decorators import action, api_view, permission_classes
from rest_framework.permissions import IsAuthenticated
from rest_framework.response import Response

from record.auth import generate_token
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
    mixins.DestroyModelMixin,
    mixins.ListModelMixin,
    viewsets.GenericViewSet,
):
    queryset = Game.objects.all()
    serializer_class = GameSerializer
    permission_classes = [IsAuthenticated]

    def get_queryset(self):
        return Game.objects.filter(owner=self.request.user)

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
        serializer = GameSerializer(instance=game)
        board: Board = last_move.board_state if last_move is not None else Board(game.size)
        response = {
            **serializer.data,
            'stones': [
                {'x': x, 'y': y, 'color': color.value} for (x, y), color in board.moves.items()
            ],
        }
        return Response(response, status=status.HTTP_200_OK)

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


class LoginSerializer(serializers.Serializer):
    username = serializers.CharField()
    password = serializers.CharField()


class UserSerializer(serializers.ModelSerializer):
    class Meta:
        model = User
        fields = ['id', 'username', 'email']


@api_view(['POST'])
def login_view(request):
    serializer = LoginSerializer(data=request.data)
    serializer.is_valid(raise_exception=True)
    user = authenticate(**serializer.validated_data)
    if not user:
        return Response(None, status=status.HTTP_401_UNAUTHORIZED)
    token = generate_token(user)
    return Response(token, status=status.HTTP_200_OK)


@api_view(['GET'])
@permission_classes([IsAuthenticated])
def user_view(request):
    serializer = UserSerializer(instance=request.user)
    return Response(serializer.data, status=status.HTTP_200_OK)
