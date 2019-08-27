import math
import random
import time
from copy import deepcopy
from functools import partial

from kivy.uix.boxlayout import BoxLayout

import chess_logic

from kivy.clock import Clock
from kivy.graphics.context_instructions import Color
from kivy.graphics.vertex_instructions import Rectangle
from kivy.graphics.vertex_instructions import Ellipse
from kivy.uix.behaviors.button import ButtonBehavior
from kivy.uix.button import Button
from kivy.uix.gridlayout import GridLayout
from kivy.uix.image import Image
from kivy.app import App
from kivy.uix.behaviors import DragBehavior
from kivy.uix.floatlayout import FloatLayout
from kivy.config import Config
from kivy.uix.label import Label
from kivy.uix.popup import Popup

width = 600
height = 600

# Config.set('graphics', 'width', str(width))
# Config.set('graphics', 'height', str(height))
# Config.set('input', 'mouse', 'mouse,multitouch_on_demand')
# Config.write()

constants = {
    'lp': 0,
    'lr': 1,
    'lb': 2,
    'ln': 3,
    'lq': 4,
    'lk': 5,
    'dp': 6,
    'dr': 7,
    'db': 8,
    'dn': 9,
    'dq': 10,
    'dk': 11,
    'e': 12,
}


def random_bitstring():
    return random.randint(2 ** 63, 2 ** 64 - 1)
    # return random.getrandbits(64)


def init_zobrist():
    arr = []
    for i in xrange(64):
        arr.append([])
        for j in xrange(14):
            while True:
                rand = random_bitstring()
                for k in xrange(len(arr)):
                    if rand in arr[k]:
                        continue
                break
            arr[i].append(rand)
    return arr

#   check regex: (\d{15,25})L, (\d+L, )+\1


def write_zobrist():
    zobrist_table = init_zobrist()

    # method 1:
    with open('zobrist_table.txt', 'w') as f:
        for item in zobrist_table:
            f.write("%s\n" % item)

    # method 2:
    # f = open('zobrist_table.txt', 'w')
    # f.writelines("%s\n" % line for line in zobrist_table)
    # f.flush()
    # f.close()

    return zobrist_table


def read_zobrist():
    zobrist_table = []
    # method 1:
    with open("zobrist_table.txt", "r") as f:
        lines = f.readlines()
        for line in lines:
            line = line[1:len(line) - 3]
            zobrist_table.append([long(x) for x in line.split(",")])

    # method 2:
    # f = open('zobrist_table.txt', 'r')
    # f.readlines()
    # f.flush()
    # f.close()
    return zobrist_table


zobrist_table = read_zobrist()


def hash_zobrist(board):
    h = 0
    for i in range(64):
        if board[i] is not None:
            j = constants.get(board[i].side + board[i].type)
            h = h ^ (zobrist_table[i][j])
    print h
    return h


def sq(x, y):
    return (int(x / 60 + 0.5)) - 1 + ((int(y / 60 + 0.5)) - 1) * 8


class PromoteButton(ButtonBehavior, Image):
    def __init__(self, p_type, side, **kwargs):
        super(PromoteButton, self).__init__(**kwargs)
        self.type = p_type
        self.side = side
        self.source = 'images\chess\\Chess_%s%st60.png' % (self.type, self.side)


class Piece(DragBehavior, Image):
    def __init__(self, p_type, side, rank, p_file, **kwargs):
        super(Piece, self).__init__(**kwargs)
        self.type = p_type
        self.side = side
        self.rank = rank
        self.file = p_file
        self.source = 'images\chess\\Chess_%s%st60.png' % (self.type, self.side)
        self.size_hint = (None, None)
        self.size = (60, 60)
        self.x = self.file * 60
        self.y = self.rank * 60
        self.drag_distance = 1
        self.drag_timeout = 10000000
        self.drag_rectangle = self.x, self.y, self.width, self.height

    def update_sq(self, file, rank):
        """

        :param file:
        :param rank:
        """
        self.file = file
        self.rank = rank
        app.chess_board.board[(self.rank - 1) * 8 + self.file - 1] = self

    def update_sq_to(self, to_sq):
        """

        :param to_sq:
        """
        self.file = to_sq % 8 + 1
        self.rank = to_sq / 8 + 1
        app.chess_board.board[to_sq] = self

    def move(self, m):
        san = app.chess_board.l_board.san(m)
        print san
        
        length = len(san)
        
        if length != 4:
            if 'o' == san[0].lower():
                app.chess_board.castle(m, length)
                return
        app.chess_board.recalc_hash(m)
        app.chess_board.clear_sq(self.rank, self.file)
        app.chess_board.l_board.push(m)
        self.update_sq(m.to_square % 8 + 1, m.to_square / 8 + 1)
        self.x = 60 * self.file
        self.y = 60 * self.rank
        self.calc_drag()

    def promote(self, m):
        layout = GridLayout(cols=1, padding=10)
        popup = Popup(title='Promote Dialogue', content=layout, auto_dismiss=False)

        layout.add_widget(Label(text="Choose new type to promote pawn to"))
        buttons_layout = GridLayout(cols=4, padding=10)

        def pb_on_press(instance):
            app.chess_board.hash ^= zobrist_table[m.from_square][constants.get(self.side + self.type)]
            self.type = instance.type
            app.chess_board.hash ^= zobrist_table[m.from_square][constants.get(self.side + self.type)]
            m.promotion = self.type
            self.source = 'images\chess\\Chess_%s%st60.png' % (self.type, self.side)
            popup.dismiss()

        buttons_layout.add_widget(PromoteButton('q', self.side, on_press=pb_on_press))
        buttons_layout.add_widget(PromoteButton('r', self.side, on_press=pb_on_press))
        buttons_layout.add_widget(PromoteButton('n', self.side, on_press=pb_on_press))
        buttons_layout.add_widget(PromoteButton('b', self.side, on_press=pb_on_press))
        layout.add_widget(buttons_layout)

        popup.open()

    def calc_drag(self):
        self.drag_rectangle = self.x, self.y, self.width, self.height

    def on_touch_down(self, touch):
        if self.collide_point(touch.x, touch.y) and app.chess_board.player_side == self.side and \
                app.chess_board.turn % 2 == (self.side == 'd'):
            super(Piece, self).on_touch_down(touch)
            app.chess_board.remove_widget(self)
            app.chess_board.highlight(self.rank, self.file)
            app.chess_board.highlight_legal_moves(self.file, self.rank)
            app.chess_board.add_widget(self)

    def on_touch_move(self, touch):
        if 60 <= self.x + touch.dx <= 480 and 60 <= self.y + touch.dy <= 480:
            super(Piece, self).on_touch_move(touch)

    @staticmethod
    def san(m):
        return app.chess_board.l_board.san(m)

    def on_touch_up(self, touch):
        if self._drag_touch and self in [x() for x in touch.grab_list]:
            if not any(app.chess_board.l_board.legal_moves):
                self.x = self.file * 60
                self.y = self.rank * 60
                self.calc_drag()
                super(Piece, self).on_touch_up(touch)
                app.chess_board.endgame((app.chess_ai.side+1)%2)
                return
            else:
                for m in app.chess_board.l_board.legal_moves:
                    if self.san(m)[0].lower == 'o':
                        super(Piece, self).on_touch_up(touch)
                        app.chess_board.refresh(m)
                        return
                        # handle castling
                    elif m.from_square == (self.rank - 1) * 8 + self.file - 1 and m.to_square == sq(self.x, self.y):
                        if m.promotion:
                            print 'promo!', m.promotion
                            self.promote(m)
                            while m.promotion == 'p':
                                pass
                        self.move(m)
                        super(Piece, self).on_touch_up(touch)
                        app.chess_board.refresh(m)
                        return

                self.x = self.file * 60
                self.y = self.rank * 60
                app.chess_board.refresh()
                self.calc_drag()
                super(Piece, self).on_touch_up(touch)


class Board(FloatLayout):
    def __init__(self, **kwargs):
        super(Board, self).__init__(**kwargs)
        self.l_board = chess_logic.Board()
        self.size = (width, height)
        self.board = []
        self.pieces = []
        self.turn = 0
        self.player_side = ''
        self.hash = 0

        self.choose_side()

    def initialize_board(self):
        for sq_rank in range(1, 9):
            for sq_file in range(1, 9):
                pos = (sq_file * 60, sq_rank * 60)
                self.board.append(None)
                with self.canvas:
                    if sq_file % 2 != sq_rank % 2:
                        Color(244 / 255.0, 220 / 255.0, 184 / 255.0, mode='rgb')
                    else:
                        Color(182 / 255.0, 142 / 255.0, 96 / 255.0, mode='rgb')
                    Rectangle(pos=pos, size=(60, 60))

        init_arr = [('p', 'd', 7, 1),
                    ('p', 'd', 7, 2),
                    ('p', 'd', 7, 3),
                    ('p', 'd', 7, 4),
                    ('p', 'd', 7, 5),
                    ('p', 'd', 7, 6),
                    ('p', 'd', 7, 7),
                    ('p', 'd', 7, 8),
                    ('r', 'd', 8, 1),
                    ('r', 'd', 8, 8),
                    ('n', 'd', 8, 2),
                    ('n', 'd', 8, 7),
                    ('b', 'd', 8, 3),
                    ('b', 'd', 8, 6),
                    ('k', 'd', 8, 5),
                    ('q', 'd', 8, 4),
                    ('p', 'l', 2, 1),
                    ('p', 'l', 2, 2),
                    ('p', 'l', 2, 3),
                    ('p', 'l', 2, 4),
                    ('p', 'l', 2, 5),
                    ('p', 'l', 2, 6),
                    ('p', 'l', 2, 7),
                    ('p', 'l', 2, 8),
                    ('r', 'l', 1, 1),
                    ('r', 'l', 1, 8),
                    ('n', 'l', 1, 2),
                    ('n', 'l', 1, 7),
                    ('b', 'l', 1, 3),
                    ('b', 'l', 1, 6),
                    ('k', 'l', 1, 5),
                    ('q', 'l', 1, 4)
                    ]
        for init_piece in init_arr:
            p = Piece(*init_piece)
            self.pieces.append(p)
            self.add_widget(p)
            self.put_piece(p)

        self.hash = hash_zobrist(self.board)

    def choose_side(self):
        layout = GridLayout(cols=1, padding=10)
        popup = Popup(title='Choose Side Dialogue', content=layout)
        layout.add_widget(Label(text="Choose your side (white always starts)"))
        buttons_layout = GridLayout(cols=2, padding=10)
        layout.add_widget(buttons_layout)

        def choose_on_press(instance):
            self.player_side = instance.text[0].lower()
            app.chess_ai.side = app.chess_board.player_side == 'd'
            popup.dismiss()
            Clock.schedule_once(lambda dt: self.initialize_board())
            if self.player_side == 'd':
                Clock.schedule_once(lambda dt: app.chess_ai.computer_move())

        buttons_layout.add_widget(Button(text='Light', on_press=choose_on_press))
        buttons_layout.add_widget(Button(text='Dark', on_press=choose_on_press))

        popup.open()

    def recalc_hash(self, m):
        p = self.board[m.to_square]
        if self.l_board.ep_square:
            self.hash ^= zobrist_table[self.l_board.ep_square][constants.get('e')]
        if p is not None:
            self.hash ^= zobrist_table[m.to_square][constants.get(p.side + p.type)]

        p = self.board[m.from_square]
        if p.type == 'p' and math.fabs(m.to_square - m.from_square) == 2*8:
            self.hash ^= zobrist_table[(m.to_square + m.from_square)/2][constants.get('e')]
        self.hash ^= zobrist_table[m.to_square][constants.get(p.side + p.type)]
        self.hash ^= zobrist_table[m.from_square][constants.get(p.side + p.type)]

    def recalc_logic_hash(self, m):
        p_type = self.l_board.piece_type_at(m.to_square)
        if self.l_board.ep_square:
            self.hash ^= zobrist_table[self.l_board.ep_square][constants.get('e')]
        if p_type is not None:
            p_type = chess_logic.PIECE_SYMBOLS[p_type]
            p_side = 'd' if self.l_board.turn % 2 == 0 else 'l'
            self.hash ^= zobrist_table[m.to_square][constants.get(p_side + p_type)]

        p_type = chess_logic.PIECE_SYMBOLS[self.l_board.piece_type_at(m.from_square)]
        p_side = 'l' if self.l_board.turn % 2 == 0 else 'd'
        if p_type == 'p' and math.fabs(m.to_square - m.from_square) == 2*8:
            self.hash ^= zobrist_table[(m.to_square + m.from_square)/2][constants.get('e')]
        self.hash ^= zobrist_table[m.to_square][constants.get(p_side + p_type)]
        self.hash ^= zobrist_table[m.from_square][constants.get(p_side + p_type)]

    def undo(self):
        if self.turn != 0:
            self.undo_single()
            if self.turn % 2 != app.chess_ai.side:
                self.undo_single()

    def undo_single(self):
        m = self.l_board.pop()
        p_type = self.l_board.piece_type_at(m.to_square)
        if p_type is not None:
            p_type = chess_logic.PIECE_SYMBOLS[p_type]
            p_side = 'd' if self.turn % 2 == 0 else 'l'
            p = Piece(p_type,p_side,m.from_square/8+1,m.from_square%8+1)
            self.pieces.append(p)
            self.add_widget(p)
            self.put_piece(p)
        self.recalc_logic_hash(m)
        self.turn -= 1


    def refresh(self, change=None, computer=False):
        self.clear_widgets()
        self.canvas.clear()
        for sq_file in range(1, 9):
            for sq_rank in range(1, 9):
                self.paint_square(sq_file, sq_rank)
        for piece in self.board:
            if piece is not None:
                self.add_widget(piece)
        #self.init_undo()
        if change != None:
            self.turn += 1
            print self.hash
            if not computer:
                Clock.schedule_once(lambda dt: app.chess_ai.computer_move())
        print self.l_board.legal_moves

    def put_piece(self, p):
        self.board[(p.rank - 1) * 8 + p.file - 1] = p

    def castle(self, m, length):
        print m, self.l_board.san(m)
        self.recalc_hash(m)

        king = self.board[m.from_square]
        rook = self.board[m.to_square]

        self.clear_sq(king.rank, king.file)
        self.clear_sq(rook.rank, rook.file)
        self.l_board.push(m)
        if length == 3:
            offset = -2
        else:
            offset = 3
        print 'king place', m.from_square % 8 + 1 - offset / 2 * 2
        king.update_sq(m.from_square % 8 + 1 - offset / 2 * 2, m.from_square / 8 + 1)
        rook.update_sq(m.to_square % 8 + 1 + offset, m.to_square / 8 + 1)
        king.x = 60 * king.file
        king.y = 60 * king.rank
        rook.x = 60 * rook.file
        rook.y = 60 * rook.rank
        king.calc_drag()
        rook.calc_drag()

    def clear_sq(self, p_rank, p_file):
        self.board[((p_rank - 1) * 8 + p_file - 1)] = None

    def endgame(self, winner):
        if self.l_board.is_seventyfive_moves():
            self.endgame_pop(side='', draw_msg='Seventy five moves limit reached')
        elif self.l_board.is_insufficient_material():
            self.endgame_pop(side='', draw_msg='Insufficient material')
        elif self.l_board.is_fivefold_repetition():
            self.endgame_pop(side='', draw_msg='Fivefold repetition')
        elif not self.l_board.is_checkmate() and not self.l_board.is_check():
            self.endgame_pop(side='', draw_msg='Stalemate')
        else:
            self.endgame_pop(winner)  # loss

    def endgame_pop(self, side, draw_msg=None):
        msg = ''
        if draw_msg is not None:
            msg = 'It\'s a Tie!: ' + draw_msg
        else:
            side = self.player_side if side > 0 else chr(ord('l') + ord('d') - ord(self.player_side))
            msg = '%s side won' % ('Light' if side == 'd' else 'Dark')

        layout = GridLayout(cols=1, padding=10)
        popup = Popup(title='Endgame Dialogue', content=layout, auto_dismiss=False)
        layout.add_widget(Label(text=msg))

        def endgame_on_press(instance):
            popup.dismiss()
            Clock.schedule_once(lambda dt: self.__init__())

        layout.add_widget(Button(text='Start new game', on_press=endgame_on_press))
        popup.open()

    def highlight(self, sq_rank, sq_file):
        with self.canvas:
            Color(1, 219 / 255.0, 88 / 255.0, mode='rgb')
            Rectangle(pos=(sq_file * 60, sq_rank * 60), size=(60, 60))

    def draw_dot(self, square_to):
        with self.canvas:
            Color(112 / 255.0, 128 / 255.0, 144 / 255.0, mode='rgb')
            Ellipse(pos=((square_to%8+1) * 60 + 22.5, (square_to/8+1) * 60 + 22.5), size=(15, 15))

    def highlight_legal_moves(self, p_file, p_rank):
        gen = (x.to_square for x in self.l_board.legal_moves if x.from_square == (p_rank - 1) * 8 + p_file - 1)
        for to_square in gen:
            self.draw_dot(to_square)

    def paint_square(self, sq_rank, sq_file):
        with self.canvas:
            if (sq_file % 2) != (sq_rank % 2):
                Color(244 / 255.0, 220 / 255.0, 184 / 255.0, mode='rgb')
            else:
                Color(182 / 255.0, 142 / 255.0, 96 / 255.0, mode='rgb')
            Rectangle(pos=(sq_file * 60, sq_rank * 60), size=(60, 60))


class ChessAI():
    def __init__(self, chess_board=None):
        self.chess_board = chess_board
        self.moves = self.chess_board.l_board.legal_moves
        self.p_vals_d = {
            'p': 10,
            'n': 30,
            'b': 30,
            'r': 50,
            'q': 90,
            'k': 9999
        }
        self.p_vals_l = [0, 10, 30, 30, 50, 90, 9999]
        self.side = self.chess_board.player_side is 'd'
        self.score = 0
        self.zobrist_dict = {}
        self.zobrist_dict_size = 40000
        pawnEvalWhite = [
            [5.5, 5.5, 5.5, 5.5, 5.5, 5.5, 5.5, 5.5],
            [5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0],
            [1.0, 1.0, 2.0, 3.0, 3.0, 2.0, 1.0, 1.0],
            [0.5, 0.5, 1.0, 2.5, 2.5, 1.0, 0.5, 0.5],
            [0.0, 0.0, 0.0, 2.0, 2.0, 0.0, 0.0, 0.0],
            [0.5, -0.5, -1.0, 0.0, 0.0, -1.0, -0.5, 0.5],
            [0.5, 1.0, 1.0, -2.0, -2.0, 1.0, 1.0, 0.5],
            [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
        ]
        pawnEvalBlack = deepcopy(pawnEvalWhite)
        pawnEvalBlack.reverse()
        knightEval = [
            [-5.0, -4.0, -3.0, -3.0, -3.0, -3.0, -4.0, -5.0],
            [-4.0, -2.0, 0.0, 0.0, 0.0, 0.0, -2.0, -4.0],
            [-3.0, 0.0, 1.0, 1.5, 1.5, 1.0, 0.0, -3.0],
            [-3.0, 0.5, 1.5, 2.0, 2.0, 1.5, 0.5, -3.0],
            [-3.0, 0.0, 1.5, 2.0, 2.0, 1.5, 0.0, -3.0],
            [-3.0, 0.5, 1.0, 1.5, 1.5, 1.0, 0.5, -3.0],
            [-4.0, -2.0, 0.0, 0.5, 0.5, 0.0, -2.0, -4.0],
            [-5.0, -4.0, -3.0, -3.0, -3.0, -3.0, -4.0, -5.0]
        ]
        bishopEvalWhite = [
            [-2.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -2.0],
            [-1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -1.0],
            [-1.0, 0.0, 0.5, 1.0, 1.0, 0.5, 0.0, -1.0],
            [-1.0, 0.5, 0.5, 1.0, 1.0, 0.5, 0.5, -1.0],
            [-1.0, 0.0, 1.0, 1.0, 1.0, 1.0, 0.0, -1.0],
            [-1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, -1.0],
            [-1.0, 0.5, 0.0, 0.0, 0.0, 0.0, 0.5, -1.0],
            [-2.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -2.0]
        ]
        bishopEvalBlack = deepcopy(bishopEvalWhite)
        bishopEvalBlack.reverse()
        rookEvalWhite = [
            [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
            [0.5, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.5],
            [-0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -0.5],
            [-0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -0.5],
            [-0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -0.5],
            [-0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -0.5],
            [-0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -0.5],
            [0.0, 0.0, 0.0, 0.5, 0.5, 0.0, 0.0, 0.0]
        ]
        rookEvalBlack = deepcopy(rookEvalWhite)
        rookEvalBlack.reverse()
        evalQueen = [
            [-2.0, -1.0, -1.0, -0.5, -0.5, -1.0, -1.0, -2.0],
            [-1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -1.0],
            [-1.0, 0.0, 0.5, 0.5, 0.5, 0.5, 0.0, -1.0],
            [-0.5, 0.0, 0.5, 0.5, 0.5, 0.5, 0.0, -0.5],
            [0.0, 0.0, 0.5, 0.5, 0.5, 0.5, 0.0, -0.5],
            [-1.0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.0, -1.0],
            [-1.0, 0.0, 0.5, 0.0, 0.0, 0.0, 0.0, -1.0],
            [-2.0, -1.0, -1.0, -0.5, -0.5, -1.0, -1.0, -2.0]
        ]
        kingEvalWhite = [
            [-3.0, -4.0, -4.0, -5.0, -5.0, -4.0, -4.0, -3.0],
            [-3.0, -4.0, -4.0, -5.0, -5.0, -4.0, -4.0, -3.0],
            [-3.0, -4.0, -4.0, -5.0, -5.0, -4.0, -4.0, -3.0],
            [-3.0, -4.0, -4.0, -5.0, -5.0, -4.0, -4.0, -3.0],
            [-2.0, -3.0, -3.0, -4.0, -4.0, -3.0, -3.0, -2.0],
            [-1.0, -2.0, -2.0, -2.0, -2.0, -2.0, -2.0, -1.0],
            [2.0, 2.0, 0.0, 0.0, 0.0, 0.0, 2.0, 2.0],
            [2.0, 3.0, 1.0, 0.0, 0.0, 1.0, 3.0, 2.0]
        ]
        kingEvalBlack = deepcopy(kingEvalWhite)
        kingEvalBlack.reverse()

        self.white_pieces_arrays = [
            pawnEvalWhite,
            knightEval,
            bishopEvalWhite,
            rookEvalWhite,
            evalQueen,
            kingEvalWhite
        ]
        self.black_pieces_arrays = [
            pawnEvalBlack,
            knightEval,
            bishopEvalBlack,
            rookEvalBlack,
            evalQueen,
            kingEvalBlack
        ]

    def computer_move(self):
        #   print 'thinking'
        self.zobrist_dict = {}
        start_t = time.time()
        res = self.minimax_root(4)
        end_t = time.time()
        print end_t - start_t, 's'
        print res, self.chess_board.l_board.san(res[0])
        self.score = res[1]


        if res[0] == None:
            self.chess_board.endgame(self.chess_board.player_side)
            return

        self.move_piece(res[0])    

    def move_piece(self, m):
        p = self.chess_board.board[m.from_square]
        if m.promotion:
            self.chess_board.hash ^= zobrist_table[m.from_square][constants.get(p.side + p.type)]
            p.type = chess_logic.PIECE_SYMBOLS[m.promotion]
            self.chess_board.hash ^= zobrist_table[m.from_square][constants.get(p.side + p.type)]
            p.source = 'images\chess\\Chess_%s%st60.png' % (p.type, p.side)

            p.move(m)
            self.chess_board.refresh(change=m, computer=True)

        else:
            san = self.chess_board.l_board.san(m)

            if 'o' == san[0]:   # castling
                self.chess_board.castle(m, len(san))
                self.chess_board.refresh(change=m, computer=True)
            else:
                p.move(m)
                self.chess_board.refresh(change=m, computer=True)

    def make_move(self, m):
        self.chess_board.recalc_logic_hash(m)
        self.chess_board.l_board.push(m)

    def undo_move(self):
        m = self.chess_board.l_board.pop()
        self.chess_board.recalc_logic_hash(m)

    def det_score_mk2(self, m):
        p_type = self.chess_board.l_board.piece_type_at(m.to_square)
        return self.p_vals_l[p_type] if p_type is not None else 0

    # def det_score(self, m):
    #     if m[len(m)-2] == 'x':
    #         return self.p_vals.get(m[len(m)-1])
    #     else:
    #         return 0

    def eval_mk2(self):
        if self.chess_board.l_board.is_checkmate():
            return float('-inf') if self.chess_board.l_board.turn%2 == self.side else float('inf')
        elif not any(self.chess_board.l_board.generate_legal_moves()):
            return 0

        score = 0
        pieces = self.chess_board.l_board.piece_map()
        for key in pieces:
            p = pieces[key]
            arr = self.white_pieces_arrays if p.color == 0 else self.black_pieces_arrays
            if p.color == self.side:
                score = score + self.p_vals_l[p.piece_type] + arr[p.piece_type - 1][key % 8][key / 8]
            else:
                score = score - self.p_vals_l[p.piece_type] - arr[p.piece_type - 1][key % 8][key / 8]
        return score

    def zobrist_lookup(self):
        try:
            val = self.zobrist_dict[self.chess_board.hash]
            return val
        except:
            return False

    def zobrist_mem_man(self, val):
        if len(self.zobrist_dict) == self.zobrist_dict_size:
            self.zobrist_dict.clear()
        self.zobrist_dict[self.chess_board.hash] = val

    def repetition_check(self, m):
        repetitions = 0
        switchyard = []

        for x in xrange(8):
            move = self.chess_board.l_board.pop()
            switchyard.append(move)

            if self.chess_board.l_board.is_irreversible(move):
                break

            if move == m:
                repetitions += 1

            if repetitions > 2:
                break

        while switchyard:
            self.chess_board.l_board.push(switchyard.pop())

        return repetitions == 3

    def minimax_root(self, depth):
        alpha = float('-inf')
        beta = float('inf')
        moves = list(self.chess_board.l_board.generate_legal_moves())
        best_score = float('-inf')
        best_move = moves[0] if any(moves) else None
        for m in moves:
            self.zobrist_dict = {}
            self.make_move(m)
            score = self.minimax(depth - 1, alpha, beta, False)
            if score > 2000 and self.repetition_check(m):
                score = 0
            self.undo_move()
            if score > best_score:
                best_move = m
                best_score = score
            alpha = max(alpha, best_score)
        return best_move, best_score

    def minimax(self, depth, alpha, beta, is_maximizing_player):
        if depth is 0:
            res = self.zobrist_lookup()
            if res:
                return res
            else:
                res = self.eval_mk2()
                self.zobrist_mem_man(res)
                return res
        if is_maximizing_player:
            best_score = -2000
            for m in self.chess_board.l_board.generate_legal_moves():
                self.make_move(m)
                best_score = max(best_score, self.minimax(depth - 1, alpha, beta, not is_maximizing_player))
                self.undo_move()
                alpha = max(alpha, best_score)
                if beta <= alpha:
                    return best_score
            return best_score
        else:
            best_score = 2000
            for m in self.chess_board.l_board.generate_legal_moves():
                self.make_move(m)
                best_score = min(best_score, self.minimax(depth - 1, alpha, beta, not is_maximizing_player))
                self.undo_move()
                beta = min(beta, best_score)
                if beta <= alpha:
                    return best_score
            return best_score


class ChessApp(App):
    def __init__(self, **kwargs):
        super(ChessApp, self).__init__(**kwargs)
        self.chess_board = Board()
        self.chess_ai = ChessAI(self.chess_board)

    def build(self):
        return self.chess_board


app = ChessApp()
app.run()
