from drf_yasg.utils import swagger_auto_schema
from rest_framework import mixins, serializers, status, viewsets
from rest_framework.decorators import action, api_view, permission_classes
from rest_framework.pagination import PageNumberPagination
from rest_framework.permissions import IsAuthenticated
from rest_framework.response import Response
from rest_framework.views import exception_handler

from django.contrib.auth import authenticate
from django.contrib.auth.models import User
from django.http import HttpResponse
from record.auth import generate_token
from record.go import Board, IllegalMoveError, Stone
from record.models import Record
from record.sgf import export_sgf


def custom_exception_handler(exc, context):
    if isinstance(exc, IllegalMoveError):
        return Response('Illegal move.', status=status.HTTP_403_FORBIDDEN)
    else:
        return exception_handler(exc, context)


class RecordSerializer(serializers.ModelSerializer):
    class Meta:
        model = Record
        fields = [
            'id',
            'owner',
            'board_size',
            'created',
            'name',
            'black_player',
            'white_player',
            'comment',
            'handicap',
            'komi',
            'ruleset',
            'winner',
        ]


class CreateRecordSerializer(serializers.ModelSerializer):
    class Meta:
        model = Record
        fields = [
            'board_size',
            'name',
            'black_player',
            'white_player',
            'comment',
            'handicap',
            'komi',
            'ruleset',
        ]


class UpdateRecordSerializer(serializers.ModelSerializer):
    class Meta:
        model = Record
        fields = [
            'name',
            'black_player',
            'white_player',
            'comment',
            'handicap',
            'komi',
            'ruleset',
            'winner',
        ]


class MoveSerializer(serializers.Serializer):
    x = serializers.IntegerField()
    y = serializers.IntegerField()


class ListRecordPagination(PageNumberPagination):
    page_size_query_param = 'page_size'
    page_size = 10

    def get_paginated_response(self, data):
        return Response(
            {
                'count': self.page.paginator.count,
                'pages': self.page.paginator.num_pages,
                'results': data,
            }
        )


class RecordViewSet(
    mixins.CreateModelMixin,
    mixins.RetrieveModelMixin,
    mixins.UpdateModelMixin,
    mixins.DestroyModelMixin,
    mixins.ListModelMixin,
    viewsets.GenericViewSet,
):
    queryset = Record.objects.all()
    serializer_class = RecordSerializer
    permission_classes = [IsAuthenticated]
    pagination_class = ListRecordPagination

    def get_queryset(self):
        return Record.objects.filter(owner=self.request.user)

    def create(self, request, **kwargs):
        serializer = CreateRecordSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        record = serializer.save(owner=request.user)
        response_serializer = RecordSerializer(instance=record)
        # return Response(response_serializer.data, status=status.HTTP_201_CREATED)
        return Response(response_serializer.data, status=status.HTTP_200_OK)

    def update(self, request, **kwargs):
        record = self.get_object()
        serializer = UpdateRecordSerializer(record, data=request.data)
        serializer.is_valid(raise_exception=True)
        record = serializer.save()
        response_serializer = RecordSerializer(instance=record)
        return Response(response_serializer.data, status=status.HTTP_200_OK)

    def retrieve(self, request, **kwargs):
        record = self.get_object()
        last_move = record.last_move
        serializer = RecordSerializer(instance=record)
        board: Board = last_move.board_state if last_move is not None else Board(record.board_size)
        sorted_stones = sorted(board.moves.items(), key=lambda x: tuple(reversed(x[0])))
        response = {
            **serializer.data,
            'stones': [{'x': x, 'y': y, 'color': color.value} for (x, y), color in sorted_stones],
            'moves': [
                {
                    'position': {'x': move.x, 'y': move.y} if move.position is not None else None,
                    'color': move.color,
                    'captures': [{'x': x, 'y': y} for (x, y) in captures],
                }
                for move, captures in zip(record.moves.all(), board.removals)
            ],
        }
        return Response(response, status=status.HTTP_200_OK)

    @action(methods=['POST'], detail=True)
    def play(self, request, **kwargs):
        record = self.get_object()
        move_serializer = MoveSerializer(data=request.data)
        move_serializer.is_valid(raise_exception=True)
        move = record.next_move(**move_serializer.validated_data)

        moves = (
            (Stone(color), position % record.board_size, position // record.board_size)
            for (color, position) in record.moves.values_list('color', 'position')
            if position is not None
        )
        replay = Board(record.board_size)
        for stone, x, y in moves:
            replay.place_stone(stone, x, y)
        removals = replay.place_stone(Stone(move.color), move.x, move.y)
        # No problems placing the stone, save it
        move.save()

        update = {
            'add': [{'x': move.x, 'y': move.y, 'color': move.color}],
            'remove': [{'x': x, 'y': y} for (x, y) in removals],
        }

        return Response(update, status=status.HTTP_201_CREATED)

    @action(url_path='pass', methods=['POST'], detail=True)
    def pass_turn(self, request, **kwargs):
        record = self.get_object()
        move = record.pass_turn()
        move.save()

        return Response(None, status=status.HTTP_201_CREATED)

    @action(methods=['POST'], detail=True)
    def undo(self, request, **kwargs):
        record = self.get_object()
        # hack to get the color of the captured stones being restored
        removal_color = record.next_move_color
        move = record.last_move
        if move is None:
            return Response('no moves to undo', status=status.HTTP_403_FORBIDDEN)
        move.delete()

        if move.position is not None:
            moves = (
                (Stone(color), position % record.board_size, position // record.board_size)
                for (color, position) in record.moves.values_list('color', 'position')
                if position is not None
            )
            replay = Board(record.board_size)
            for stone, x, y in moves:
                replay.place_stone(stone, x, y)
            removals = replay.place_stone(Stone(move.color), move.x, move.y)

            # Re-add the stones that would have been captured by playing the undo'd move
            update = {
                'add': [{'x': x, 'y': y, 'color': removal_color} for (x, y) in removals],
                'remove': [{'x': move.x, 'y': move.y}],
            }
        else:
            update = {'add': [], 'remove': []}

        return Response(update, status=status.HTTP_200_OK)

    @action(methods=['GET'], detail=True)
    def download(self, request, **kwargs):
        record = self.get_object()
        content = export_sgf(record)
        return HttpResponse(
            content,
            content_type='application/x-go-sgf',
            headers={'Content-Disposition': f'attachment; filename="{record.sgf_name}"'},
        )


class LoginSerializer(serializers.Serializer):
    username = serializers.CharField()
    password = serializers.CharField()


class RegisterSerializer(serializers.ModelSerializer):
    class Meta:
        model = User
        fields = ['username', 'email', 'password']


class UserSerializer(serializers.ModelSerializer):
    class Meta:
        model = User
        fields = ['id', 'username', 'email']


@swagger_auto_schema(method='POST', request_body=LoginSerializer, responses={201: ''})
@api_view(['POST'])
def login_view(request):
    serializer = LoginSerializer(data=request.data)
    serializer.is_valid(raise_exception=True)
    user = authenticate(**serializer.validated_data)
    if not user:
        return Response(None, status=status.HTTP_401_UNAUTHORIZED)
    token = generate_token(user)
    return Response(token, status=status.HTTP_200_OK)


@swagger_auto_schema(method='POST', request_body=RegisterSerializer)
@api_view(['POST'])
def register_view(request):
    serializer = RegisterSerializer(data=request.data)
    serializer.is_valid(raise_exception=True)
    user = User(**serializer.validated_data)
    user.set_password(serializer.validated_data['password'])
    user.save()
    response_serializer = UserSerializer(instance=user)
    return Response(response_serializer.data, status=status.HTTP_201_CREATED)


@api_view(['GET'])
@permission_classes([IsAuthenticated])
def user_view(request):
    serializer = UserSerializer(instance=request.user)
    return Response(serializer.data, status=status.HTTP_200_OK)
