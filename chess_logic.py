# -*- coding: utf-8 -*-
#
# This file is part of the python-chess library.
# Copyright (C) 2012-2019 Niklas Fiekas <niklas.fiekas@backscattering.de>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.


import collections
import copy
import enum
import re
import itertools


Color = bool
COLORS = [WHITE, BLACK] = [True, False]
COLOR_NAMES = ["black", "white"]

PieceType = int
PIECE_TYPES = [PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING] = range(1, 7)
PIECE_SYMBOLS = [None, "p", "n", "b", "r", "q", "k"]
PIECE_NAMES = [None, "pawn", "knight", "bishop", "rook", "queen", "king"]

FILE_NAMES = ["a", "b", "c", "d", "e", "f", "g", "h"]

RANK_NAMES = ["1", "2", "3", "4", "5", "6", "7", "8"]

STARTING_FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
"""The FEN for the standard chess starting position."""

STARTING_BOARD_FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
"""The board part of the FEN for the standard chess starting position."""

_IntFlag = enum.IntEnum

class Status(_IntFlag):
    VALID = 0
    NO_WHITE_KING = 1
    NO_BLACK_KING = 2
    TOO_MANY_KINGS = 4
    TOO_MANY_WHITE_PAWNS = 8
    TOO_MANY_BLACK_PAWNS = 16
    PAWNS_ON_BACKRANK = 32
    TOO_MANY_WHITE_PIECES = 64
    TOO_MANY_BLACK_PIECES = 128
    BAD_CASTLING_RIGHTS = 256
    INVALID_EP_SQUARE = 512
    OPPOSITE_CHECK = 1024
    EMPTY = 2048
    RACE_CHECK = 4096
    RACE_OVER = 8192
    RACE_MATERIAL = 16384

STATUS_VALID = Status.VALID
STATUS_NO_WHITE_KING = Status.NO_WHITE_KING
STATUS_NO_BLACK_KING = Status.NO_BLACK_KING
STATUS_TOO_MANY_KINGS = Status.TOO_MANY_KINGS
STATUS_TOO_MANY_WHITE_PAWNS = Status.TOO_MANY_WHITE_PAWNS
STATUS_TOO_MANY_BLACK_PAWNS = Status.TOO_MANY_BLACK_PAWNS
STATUS_PAWNS_ON_BACKRANK = Status.PAWNS_ON_BACKRANK
STATUS_TOO_MANY_WHITE_PIECES = Status.TOO_MANY_WHITE_PIECES
STATUS_TOO_MANY_BLACK_PIECES = Status.TOO_MANY_BLACK_PIECES
STATUS_BAD_CASTLING_RIGHTS = Status.BAD_CASTLING_RIGHTS
STATUS_INVALID_EP_SQUARE = Status.INVALID_EP_SQUARE
STATUS_OPPOSITE_CHECK = Status.OPPOSITE_CHECK
STATUS_EMPTY = Status.EMPTY
STATUS_RACE_CHECK = Status.RACE_CHECK
STATUS_RACE_OVER = Status.RACE_OVER
STATUS_RACE_MATERIAL = Status.RACE_MATERIAL


Square = int
SQUARES = [
    A1, B1, C1, D1, E1, F1, G1, H1,
    A2, B2, C2, D2, E2, F2, G2, H2,
    A3, B3, C3, D3, E3, F3, G3, H3,
    A4, B4, C4, D4, E4, F4, G4, H4,
    A5, B5, C5, D5, E5, F5, G5, H5,
    A6, B6, C6, D6, E6, F6, G6, H6,
    A7, B7, C7, D7, E7, F7, G7, H7,
    A8, B8, C8, D8, E8, F8, G8, H8,
] = range(64)

SQUARE_NAMES = [f + r for r in RANK_NAMES for f in FILE_NAMES]

def square(file_index, rank_index):
    """Gets a square number by file and rank index."""
    return rank_index * 8 + file_index

def square_file(square):
    """Gets the file index of the square where ``0`` is the a-file."""
    return square & 7

def square_rank(square):
    """Gets the rank index of the square where ``0`` is the first rank."""
    return square >> 3

def square_name(square):
    """Gets the name of the square, like ``a3``."""
    return SQUARE_NAMES[square]

def square_distance(a, b):
    """
    Gets the distance (i.e., the number of king steps) from square *a* to *b*.
    """
    return max(abs(square_file(a) - square_file(b)), abs(square_rank(a) - square_rank(b)))

def square_mirror(square):
    """Mirrors the square vertically."""
    return square ^ 0x38

SQUARES_180 = [square_mirror(sq) for sq in SQUARES]


Bitboard = int
BB_EMPTY = 0
BB_ALL = 0xffffffffffffffff

BB_SQUARES = [
    BB_A1, BB_B1, BB_C1, BB_D1, BB_E1, BB_F1, BB_G1, BB_H1,
    BB_A2, BB_B2, BB_C2, BB_D2, BB_E2, BB_F2, BB_G2, BB_H2,
    BB_A3, BB_B3, BB_C3, BB_D3, BB_E3, BB_F3, BB_G3, BB_H3,
    BB_A4, BB_B4, BB_C4, BB_D4, BB_E4, BB_F4, BB_G4, BB_H4,
    BB_A5, BB_B5, BB_C5, BB_D5, BB_E5, BB_F5, BB_G5, BB_H5,
    BB_A6, BB_B6, BB_C6, BB_D6, BB_E6, BB_F6, BB_G6, BB_H6,
    BB_A7, BB_B7, BB_C7, BB_D7, BB_E7, BB_F7, BB_G7, BB_H7,
    BB_A8, BB_B8, BB_C8, BB_D8, BB_E8, BB_F8, BB_G8, BB_H8
] = [1 << sq for sq in SQUARES]

BB_CORNERS = BB_A1 | BB_H1 | BB_A8 | BB_H8
BB_CENTER = BB_D4 | BB_E4 | BB_D5 | BB_E5

BB_LIGHT_SQUARES = 0x55aa55aa55aa55aa
BB_DARK_SQUARES = 0xaa55aa55aa55aa55

BB_FILES = [
    BB_FILE_A,
    BB_FILE_B,
    BB_FILE_C,
    BB_FILE_D,
    BB_FILE_E,
    BB_FILE_F,
    BB_FILE_G,
    BB_FILE_H
] = [0x0101010101010101 << i for i in range(8)]

BB_RANKS = [
    BB_RANK_1,
    BB_RANK_2,
    BB_RANK_3,
    BB_RANK_4,
    BB_RANK_5,
    BB_RANK_6,
    BB_RANK_7,
    BB_RANK_8
] = [0xff << (8 * i) for i in range(8)]

BB_BACKRANKS = BB_RANK_1 | BB_RANK_8


def lsb(bb):
    return (bb & -bb).bit_length() - 1

def scan_forward(bb):
    while bb:
        r = bb & -bb
        yield r.bit_length() - 1
        bb ^= r

def msb(bb):
    return bb.bit_length() - 1

def scan_reversed(bb, _BB_SQUARES=BB_SQUARES, *args):
    while bb:
        r = bb.bit_length() - 1
        yield r
        bb ^= _BB_SQUARES[r]

def popcount(bb, _bin=bin ,*args):
    return _bin(bb).count("1")

def flip_vertical(bb):
    # https://www.chessprogramming.org/Flipping_Mirroring_and_Rotating#FlipVertically
    bb = ((bb >> 8) & 0x00ff00ff00ff00ff) | ((bb & 0x00ff00ff00ff00ff) << 8)
    bb = ((bb >> 16) & 0x0000ffff0000ffff) | ((bb & 0x0000ffff0000ffff) << 16)
    bb = (bb >> 32) | ((bb & 0x00000000ffffffff) << 32)
    return bb

def flip_horizontal(bb):
    # https://www.chessprogramming.org/Flipping_Mirroring_and_Rotating#MirrorHorizontally
    bb = ((bb >> 1) & 0x5555555555555555) | ((bb & 0x5555555555555555) << 1)
    bb = ((bb >> 2) & 0x3333333333333333) | ((bb & 0x3333333333333333) << 2)
    bb = ((bb >> 4) & 0x0f0f0f0f0f0f0f0f) | ((bb & 0x0f0f0f0f0f0f0f0f) << 4)
    return bb

def flip_diagonal(bb):
    # https://www.chessprogramming.org/Flipping_Mirroring_and_Rotating#FlipabouttheDiagonal
    t = (bb ^ (bb << 28)) & 0x0f0f0f0f00000000
    bb = bb ^ (t ^ (t >> 28))
    t = (bb ^ (bb << 14)) & 0x3333000033330000
    bb = bb ^ (t ^ (t >> 14))
    t = (bb ^ (bb << 7)) & 0x5500550055005500
    bb = bb ^ (t ^ (t >> 7))
    return bb

def flip_anti_diagonal(bb):
    # https://www.chessprogramming.org/Flipping_Mirroring_and_Rotating#FlipabouttheAntidiagonal
    t = bb ^ (bb << 36)
    bb = bb ^ ((t ^ (bb >> 36)) & 0xf0f0f0f00f0f0f0f)
    t = (bb ^ (bb << 18)) & 0xcccc0000cccc0000
    bb = bb ^ (t ^ (t >> 18))
    t = (bb ^ (bb << 9)) & 0xaa00aa00aa00aa00
    bb = bb ^ (t ^ (t >> 9))
    return bb


def shift_down(b):
    return b >> 8

def shift_2_down(b):
    return b >> 16

def shift_up(b):
    return (b << 8) & BB_ALL

def shift_2_up(b):
    return (b << 16) & BB_ALL

def shift_right(b):
    return (b << 1) & ~BB_FILE_A & BB_ALL

def shift_2_right(b):
    return (b << 2) & ~BB_FILE_A & ~BB_FILE_B & BB_ALL

def shift_left(b):
    return (b >> 1) & ~BB_FILE_H

def shift_2_left(b):
    return (b >> 2) & ~BB_FILE_G & ~BB_FILE_H

def shift_up_left(b):
    return (b << 7) & ~BB_FILE_H & BB_ALL

def shift_up_right(b):
    return (b << 9) & ~BB_FILE_A & BB_ALL

def shift_down_left(b):
    return (b >> 9) & ~BB_FILE_H

def shift_down_right(b):
    return (b >> 7) & ~BB_FILE_A


def _sliding_attacks(square, occupied, deltas):
    attacks = BB_EMPTY

    for delta in deltas:
        sq = square

        while True:
            sq += delta
            if not (0 <= sq < 64) or square_distance(sq, sq - delta) > 2:
                break

            attacks |= BB_SQUARES[sq]

            if occupied & BB_SQUARES[sq]:
                break

    return attacks

def _step_attacks(square, deltas):
    return _sliding_attacks(square, BB_ALL, deltas)

BB_KNIGHT_ATTACKS = [_step_attacks(sq, [17, 15, 10, 6, -17, -15, -10, -6]) for sq in SQUARES]
BB_KING_ATTACKS = [_step_attacks(sq, [9, 8, 7, 1, -9, -8, -7, -1]) for sq in SQUARES]
BB_PAWN_ATTACKS = [[_step_attacks(sq, deltas) for sq in SQUARES] for deltas in [[-7, -9], [7, 9]]]


def _edges(square):
    return (((BB_RANK_1 | BB_RANK_8) & ~BB_RANKS[square_rank(square)]) |
            ((BB_FILE_A | BB_FILE_H) & ~BB_FILES[square_file(square)]))

def _carry_rippler(mask):
    # Carry-Rippler trick to iterate subsets of mask.
    subset = BB_EMPTY
    while True:
        yield subset
        subset = (subset - mask) & mask
        if not subset:
            break

def _attack_table(deltas):
    mask_table = []
    attack_table = []

    for square in SQUARES:
        attacks = {}

        mask = _sliding_attacks(square, 0, deltas) & ~_edges(square)
        for subset in _carry_rippler(mask):
            attacks[subset] = _sliding_attacks(square, subset, deltas)

        attack_table.append(attacks)
        mask_table.append(mask)

    return mask_table, attack_table

BB_DIAG_MASKS, BB_DIAG_ATTACKS = _attack_table([-9, -7, 7, 9])
BB_FILE_MASKS, BB_FILE_ATTACKS = _attack_table([-8, 8])
BB_RANK_MASKS, BB_RANK_ATTACKS = _attack_table([-1, 1])


def _rays():
    rays = []
    between = []
    for a, bb_a in enumerate(BB_SQUARES):
        rays_row = []
        between_row = []
        for b, bb_b in enumerate(BB_SQUARES):
            if BB_DIAG_ATTACKS[a][0] & bb_b:
                rays_row.append((BB_DIAG_ATTACKS[a][0] & BB_DIAG_ATTACKS[b][0]) | bb_a | bb_b)
                between_row.append(BB_DIAG_ATTACKS[a][BB_DIAG_MASKS[a] & bb_b] & BB_DIAG_ATTACKS[b][BB_DIAG_MASKS[b] & bb_a])
            elif BB_RANK_ATTACKS[a][0] & bb_b:
                rays_row.append(BB_RANK_ATTACKS[a][0] | bb_a)
                between_row.append(BB_RANK_ATTACKS[a][BB_RANK_MASKS[a] & bb_b] & BB_RANK_ATTACKS[b][BB_RANK_MASKS[b] & bb_a])
            elif BB_FILE_ATTACKS[a][0] & bb_b:
                rays_row.append(BB_FILE_ATTACKS[a][0] | bb_a)
                between_row.append(BB_FILE_ATTACKS[a][BB_FILE_MASKS[a] & bb_b] & BB_FILE_ATTACKS[b][BB_FILE_MASKS[b] & bb_a])
            else:
                rays_row.append(BB_EMPTY)
                between_row.append(BB_EMPTY)
        rays.append(rays_row)
        between.append(between_row)
    return rays, between

BB_RAYS, BB_BETWEEN = _rays()


SAN_REGEX = re.compile(r"^([NBKRQ])?([a-h])?([1-8])?[\-x]?([a-h][1-8])(=?[nbrqkNBRQK])?(\+|#)?\Z")

FEN_CASTLING_REGEX = re.compile(r"^(?:-|[KQABCDEFGH]{0,2}[kqabcdefgh]{0,2})\Z")


class Piece:
    """A piece with type and color."""

    def __init__(self, piece_type, color):
        self.piece_type = piece_type
        self.color = color

    def __hash__(self):
        return hash(self.piece_type * (self.color + 1))

    def __eq__(self, other):
        try:
            return (self.piece_type, self.color) == (other.piece_type, other.color)
        except AttributeError:
            return NotImplemented


class Move:
    """
    Represents a move from a square to a square and possibly the promotion
    piece type.
    Drops and null moves are supported.
    """

    def __init__(self, from_square, to_square, promotion=None, drop=None):
        self.from_square = from_square
        self.to_square = to_square
        self.promotion = promotion
        self.drop = drop

    def uci(self):
        """
        Gets an UCI string for the move.
        For example, a move from a7 to a8 would be ``a7a8`` or ``a7a8q``
        (if the latter is a promotion to a queen).
        The UCI representation of a null move is ``0000``.
        """
        if self.drop:
            return PIECE_SYMBOLS[self.drop].upper() + "@" + SQUARE_NAMES[self.to_square]
        elif self.promotion:
            return SQUARE_NAMES[self.from_square] + SQUARE_NAMES[self.to_square] + PIECE_SYMBOLS[self.promotion]
        elif self:
            return SQUARE_NAMES[self.from_square] + SQUARE_NAMES[self.to_square]
        else:
            return "0000"

    def xboard(self):
        return self.uci() if self else "@@@@"

    def __bool__(self):
        return bool(self.from_square or self.to_square or self.promotion or self.drop)

    def __eq__(self, other):
        try:
            return (
                self.from_square == other.from_square and
                self.to_square == other.to_square and
                self.promotion == other.promotion and
                self.drop == other.drop)
        except AttributeError:
            return NotImplemented

    def __repr__(self):
        return "Move.from_uci({!r})".format(self.uci())

    def __str__(self):
        return self.uci()

    def __hash__(self):
        return hash((self.to_square, self.from_square, self.promotion, self.drop))

    def __copy__(self):
        return type(self)(self.from_square, self.to_square, self.promotion, self.drop)

    def __deepcopy__(self, memo):
        move = self.__copy__()
        memo[id(self)] = move
        return move

    @classmethod
    def from_uci(cls, uci):
        """
        Parses an UCI string.
        :raises: :exc:`ValueError` if the UCI string is invalid.
        """
        if uci == "0000":
            return cls.null()
        elif len(uci) == 4 and "@" == uci[1]:
            drop = PIECE_SYMBOLS.index(uci[0].lower())
            square = SQUARE_NAMES.index(uci[2:])
            return cls(square, square, drop=drop)
        elif len(uci) == 4:
            return cls(SQUARE_NAMES.index(uci[0:2]), SQUARE_NAMES.index(uci[2:4]))
        elif len(uci) == 5:
            promotion = PIECE_SYMBOLS.index(uci[4])
            return cls(SQUARE_NAMES.index(uci[0:2]), SQUARE_NAMES.index(uci[2:4]), promotion=promotion)
        else:
            raise ValueError("expected uci string to be of length 4 or 5: {!r}".format(uci))

    @classmethod
    def null(cls):
        """
        Gets a null move.
        A null move just passes the turn to the other side (and possibly
        forfeits en passant capturing). Null moves evaluate to ``False`` in
        boolean contexts.
        bool(chess.Move.null())
        False
        """
        return cls(0, 0)


class BaseBoard:
    """
    A board representing the position of chess pieces. See
    :class:`~chess.Board` for a full board with move generation.
    The board is initialized with the standard chess starting position, unless
    otherwise specified in the optional *board_fen* argument. If *board_fen*
    is ``None``, an empty board is created.
    """

    def __init__(self, board_fen=STARTING_BOARD_FEN):
        self.occupied_co = [BB_EMPTY, BB_EMPTY]

        if board_fen is None:
            self._clear_board()
        elif board_fen == STARTING_BOARD_FEN:
            self._reset_board()

    def _reset_board(self):
        self.pawns = BB_RANK_2 | BB_RANK_7
        self.knights = BB_B1 | BB_G1 | BB_B8 | BB_G8
        self.bishops = BB_C1 | BB_F1 | BB_C8 | BB_F8
        self.rooks = BB_CORNERS
        self.queens = BB_D1 | BB_D8
        self.kings = BB_E1 | BB_E8

        self.promoted = BB_EMPTY

        self.occupied_co[WHITE] = BB_RANK_1 | BB_RANK_2
        self.occupied_co[BLACK] = BB_RANK_7 | BB_RANK_8
        self.occupied = BB_RANK_1 | BB_RANK_2 | BB_RANK_7 | BB_RANK_8

    def reset_board(self):
        self._reset_board()

    def _clear_board(self):
        self.pawns = BB_EMPTY
        self.knights = BB_EMPTY
        self.bishops = BB_EMPTY
        self.rooks = BB_EMPTY
        self.queens = BB_EMPTY
        self.kings = BB_EMPTY

        self.promoted = BB_EMPTY

        self.occupied_co[WHITE] = BB_EMPTY
        self.occupied_co[BLACK] = BB_EMPTY
        self.occupied = BB_EMPTY

    def clear_board(self):
        """Clears the board."""
        self._clear_board()

    def pieces_mask(self, piece_type, color):
        if piece_type == PAWN:
            bb = self.pawns
        elif piece_type == KNIGHT:
            bb = self.knights
        elif piece_type == BISHOP:
            bb = self.bishops
        elif piece_type == ROOK:
            bb = self.rooks
        elif piece_type == QUEEN:
            bb = self.queens
        elif piece_type == KING:
            bb = self.kings

        return bb & self.occupied_co[color]

    def pieces(self, piece_type, color):
        """
        Gets pieces of the given type and color.
        Returns a :class:`set of squares <chess.SquareSet>`.
        """
        return SquareSet(self.pieces_mask(piece_type, color))

    def piece_at(self, square):
        """Gets the :class:`piece <chess.Piece>` at the given square."""
        piece_type = self.piece_type_at(square)
        if piece_type:
            mask = BB_SQUARES[square]
            color = bool(self.occupied_co[WHITE] & mask)
            return Piece(piece_type, color)

    def piece_type_at(self, square):
        """Gets the piece type at the given square."""
        mask = BB_SQUARES[square]

        if not self.occupied & mask:
            return None
        elif self.pawns & mask:
            return PAWN
        elif self.knights & mask:
            return KNIGHT
        elif self.bishops & mask:
            return BISHOP
        elif self.rooks & mask:
            return ROOK
        elif self.queens & mask:
            return QUEEN
        elif self.kings & mask:
            return KING

    def king(self, color):
        """
        Finds the king square of the given side. Returns ``None`` if there
        is no king of that color.
        In variants with king promotions, only non-promoted kings are
        considered.
        """
        king_mask = self.occupied_co[color] & self.kings & ~self.promoted
        if king_mask:
            return msb(king_mask)

    def attacks_mask(self, square):
        bb_square = BB_SQUARES[square]

        if bb_square & self.pawns:
            color = bool(bb_square & self.occupied_co[WHITE])
            return BB_PAWN_ATTACKS[color][square]
        elif bb_square & self.knights:
            return BB_KNIGHT_ATTACKS[square]
        elif bb_square & self.kings:
            return BB_KING_ATTACKS[square]
        else:
            attacks = 0
            if bb_square & self.bishops or bb_square & self.queens:
                attacks = BB_DIAG_ATTACKS[square][BB_DIAG_MASKS[square] & self.occupied]
            if bb_square & self.rooks or bb_square & self.queens:
                attacks |= (BB_RANK_ATTACKS[square][BB_RANK_MASKS[square] & self.occupied] |
                            BB_FILE_ATTACKS[square][BB_FILE_MASKS[square] & self.occupied])
            return attacks

    def attacks(self, square):
        """
        Gets a set of attacked squares from a given square.
        There will be no attacks if the square is empty. Pinned pieces are
        still attacking other squares.
        Returns a :class:`set of squares <chess.SquareSet>`.
        """
        return SquareSet(self.attacks_mask(square))

    def _attackers_mask(self, color, square, occupied):
        rank_pieces = BB_RANK_MASKS[square] & occupied
        file_pieces = BB_FILE_MASKS[square] & occupied
        diag_pieces = BB_DIAG_MASKS[square] & occupied

        queens_and_rooks = self.queens | self.rooks
        queens_and_bishops = self.queens | self.bishops

        attackers = (
            (BB_KING_ATTACKS[square] & self.kings) |
            (BB_KNIGHT_ATTACKS[square] & self.knights) |
            (BB_RANK_ATTACKS[square][rank_pieces] & queens_and_rooks) |
            (BB_FILE_ATTACKS[square][file_pieces] & queens_and_rooks) |
            (BB_DIAG_ATTACKS[square][diag_pieces] & queens_and_bishops) |
            (BB_PAWN_ATTACKS[not color][square] & self.pawns))

        return attackers & self.occupied_co[color]

    def attackers_mask(self, color, square):
        return self._attackers_mask(color, square, self.occupied)

    def is_attacked_by(self, color, square):
        """
        Checks if the given side attacks the given square.
        Pinned pieces still count as attackers. Pawns that can be captured
        en passant are **not** considered attacked.
        """
        return bool(self.attackers_mask(color, square))

    def attackers(self, color, square):
        """
        Gets a set of attackers of the given color for the given square.
        Pinned pieces still count as attackers.
        Returns a :class:`set of squares <chess.SquareSet>`.
        """
        return SquareSet(self.attackers_mask(color, square))

    def pin_mask(self, color, square):
        king = self.king(color)
        if king is None:
            return BB_ALL

        square_mask = BB_SQUARES[square]

        for attacks, sliders in [(BB_FILE_ATTACKS, self.rooks | self.queens),
                                 (BB_RANK_ATTACKS, self.rooks | self.queens),
                                 (BB_DIAG_ATTACKS, self.bishops | self.queens)]:
            rays = attacks[king][0]
            if rays & square_mask:
                snipers = rays & sliders & self.occupied_co[not color]
                for sniper in scan_reversed(snipers):
                    if BB_BETWEEN[sniper][king] & (self.occupied | square_mask) == square_mask:
                        return BB_RAYS[king][sniper]

                break

        return BB_ALL

    def pin(self, color, square):
        """
        Detects an absolute pin (and its direction) of the given square to
        the king of the given color.
        board = chess.Board("rnb1k2r/ppp2ppp/5n2/3q4/1b1P4/2N5/PP3PPP/R1BQKBNR w KQkq - 3 7")
        board.is_pinned(chess.WHITE, chess.C3)
        True
        direction = board.pin(chess.WHITE, chess.C3)
        direction
        SquareSet(0x0000000102040810)
        print(direction)
        . . . . . . . .
        . . . . . . . .
        . . . . . . . .
        1 . . . . . . .
        . 1 . . . . . .
        . . 1 . . . . .
        . . . 1 . . . .
        . . . . 1 . . .
        Returns a :class:`set of squares <chess.SquareSet>` that mask the rank,
        file or diagonal of the pin. If there is no pin, then a mask of the
        entire board is returned.
        """
        return SquareSet(self.pin_mask(color, square))

    def is_pinned(self, color, square):
        """
        Detects if the given square is pinned to the king of the given color.
        """
        return self.pin_mask(color, square) != BB_ALL

    def _remove_piece_at(self, square):
        piece_type = self.piece_type_at(square)
        mask = BB_SQUARES[square]

        if piece_type == PAWN:
            self.pawns ^= mask
        elif piece_type == KNIGHT:
            self.knights ^= mask
        elif piece_type == BISHOP:
            self.bishops ^= mask
        elif piece_type == ROOK:
            self.rooks ^= mask
        elif piece_type == QUEEN:
            self.queens ^= mask
        elif piece_type == KING:
            self.kings ^= mask
        else:
            return

        self.occupied ^= mask
        self.occupied_co[WHITE] &= ~mask
        self.occupied_co[BLACK] &= ~mask

        self.promoted &= ~mask

        return piece_type

    def remove_piece_at(self, square):
        """
        Removes the piece from the given square. Returns the
        :class:`~chess.Piece` or ``None`` if the square was already empty.
        """
        color = bool(self.occupied_co[WHITE] & BB_SQUARES[square])
        piece_type = self._remove_piece_at(square)
        if piece_type:
            return Piece(piece_type, color)

    def _set_piece_at(self, square, piece_type, color, promoted=False):
        self._remove_piece_at(square)

        mask = BB_SQUARES[square]

        if piece_type == PAWN:
            self.pawns |= mask
        elif piece_type == KNIGHT:
            self.knights |= mask
        elif piece_type == BISHOP:
            self.bishops |= mask
        elif piece_type == ROOK:
            self.rooks |= mask
        elif piece_type == QUEEN:
            self.queens |= mask
        elif piece_type == KING:
            self.kings |= mask
        else:
            return

        self.occupied ^= mask
        self.occupied_co[color] ^= mask

        if promoted:
            self.promoted ^= mask

    def set_piece_at(self, square, piece, promoted=False):
        """
        Sets a piece at the given square.
        An existing piece is replaced. Setting *piece* to ``None`` is
        equivalent to :func:`~chess.Board.remove_piece_at()`.
        """
        if piece is None:
            self._remove_piece_at(square)
        else:
            self._set_piece_at(square, piece.piece_type, piece.color, promoted)

    def piece_map(self):
        """
        Gets a dictionary of :class:`pieces <chess.Piece>` by square index.
        """
        result = {}
        for square in scan_reversed(self.occupied):
            result[square] = self.piece_at(square)
        return result

    def _set_piece_map(self, pieces):
        self._clear_board()
        for square, piece in pieces.items():
            self._set_piece_at(square, piece.piece_type, piece.color)

    def set_piece_map(self, pieces):
        """
        Sets up the board from a dictionary of :class:`pieces <chess.Piece>`
        by square index.
        """
        self._set_piece_map(pieces)

    def _set_chess960_pos(self, sharnagl):
        if not 0 <= sharnagl <= 959:
            raise ValueError("chess960 position index not 0 <= {:d} <= 959".format(sharnagl))

        # See http://www.russellcottrell.com/Chess/Chess960.htm for
        # a description of the algorithm.
        n, bw = divmod(sharnagl, 4)
        n, bb = divmod(n, 4)
        n, q = divmod(n, 6)

        for n1 in range(0, 4):
            n2 = n + (3 - n1) * (4 - n1) // 2 - 5
            if n1 < n2 and 1 <= n2 <= 4:
                break

        # Bishops.
        bw_file = bw * 2 + 1
        bb_file = bb * 2
        self.bishops = (BB_FILES[bw_file] | BB_FILES[bb_file]) & BB_BACKRANKS

        # Queens.
        q_file = q
        q_file += int(min(bw_file, bb_file) <= q_file)
        q_file += int(max(bw_file, bb_file) <= q_file)
        self.queens = BB_FILES[q_file] & BB_BACKRANKS

        used = [bw_file, bb_file, q_file]

        # Knights.
        self.knights = BB_EMPTY
        for i in range(0, 8):
            if i not in used:
                if n1 == 0 or n2 == 0:
                    self.knights |= BB_FILES[i] & BB_BACKRANKS
                    used.append(i)
                n1 -= 1
                n2 -= 1

        # RKR.
        for i in range(0, 8):
            if i not in used:
                self.rooks = BB_FILES[i] & BB_BACKRANKS
                used.append(i)
                break
        for i in range(1, 8):
            if i not in used:
                self.kings = BB_FILES[i] & BB_BACKRANKS
                used.append(i)
                break
        for i in range(2, 8):
            if i not in used:
                self.rooks |= BB_FILES[i] & BB_BACKRANKS
                break

        # Finalize.
        self.pawns = BB_RANK_2 | BB_RANK_7
        self.occupied_co[WHITE] = BB_RANK_1 | BB_RANK_2
        self.occupied_co[BLACK] = BB_RANK_7 | BB_RANK_8
        self.occupied = BB_RANK_1 | BB_RANK_2 | BB_RANK_7 | BB_RANK_8
        self.promoted = BB_EMPTY

    def set_chess960_pos(self, sharnagl):
        """
        Sets up a Chess960 starting position given its index between 0 and 959.
        Also see :func:`~chess.BaseBoard.from_chess960_pos()`.
        """
        self._set_chess960_pos(sharnagl)

    def chess960_pos(self):
        """
        Gets the Chess960 starting position index between 0 and 959
        or ``None``.
        """
        if self.occupied_co[WHITE] != BB_RANK_1 | BB_RANK_2:
            return None
        if self.occupied_co[BLACK] != BB_RANK_7 | BB_RANK_8:
            return None
        if self.pawns != BB_RANK_2 | BB_RANK_7:
            return None
        if self.promoted:
            return None

        # Piece counts.
        brnqk = [self.bishops, self.rooks, self.knights, self.queens, self.kings]
        if [popcount(pieces) for pieces in brnqk] != [4, 4, 4, 2, 2]:
            return None

        # Symmetry.
        if any((BB_RANK_1 & pieces) << 56 != BB_RANK_8 & pieces for pieces in brnqk):
            return None

        # Algorithm from ChessX, src/database/bitboard.cpp, r2254.
        x = self.bishops & (2 + 8 + 32 + 128)
        if not x:
            return None
        bs1 = (lsb(x) - 1) // 2
        cc_pos = bs1
        x = self.bishops & (1 + 4 + 16 + 64)
        if not x:
            return None
        bs2 = lsb(x) * 2
        cc_pos += bs2

        q = 0
        qf = False
        n0 = 0
        n1 = 0
        n0f = False
        n1f = False
        rf = 0
        n0s = [0, 4, 7, 9]
        for square in range(A1, H1 + 1):
            bb = BB_SQUARES[square]
            if bb & self.queens:
                qf = True
            elif bb & self.rooks or bb & self.kings:
                if bb & self.kings:
                    if rf != 1:
                        return None
                else:
                    rf += 1

                if not qf:
                    q += 1

                if not n0f:
                    n0 += 1
                elif not n1f:
                    n1 += 1
            elif bb & self.knights:
                if not qf:
                    q += 1

                if not n0f:
                    n0f = True
                elif not n1f:
                    n1f = True

        if n0 < 4 and n1f and qf:
            cc_pos += q * 16
            krn = n0s[n0] + n1
            cc_pos += krn * 96
            return cc_pos
        else:
            return None

    def __eq__(self, board):
        try:
            return (
                self.occupied == board.occupied and
                self.occupied_co[WHITE] == board.occupied_co[WHITE] and
                self.pawns == board.pawns and
                self.knights == board.knights and
                self.bishops == board.bishops and
                self.rooks == board.rooks and
                self.queens == board.queens and
                self.kings == board.kings)
        except AttributeError:
            return NotImplemented

    def apply_transform(self, f):
        self.pawns = f(self.pawns)
        self.knights = f(self.knights)
        self.bishops = f(self.bishops)
        self.rooks = f(self.rooks)
        self.queens = f(self.queens)
        self.kings = f(self.kings)

        self.occupied_co[WHITE] = f(self.occupied_co[WHITE])
        self.occupied_co[BLACK] = f(self.occupied_co[BLACK])
        self.occupied = f(self.occupied)
        self.promoted = f(self.promoted)

    def transform(self, f):
        board = self.copy()
        board.apply_transform(f)
        return board

    def mirror(self):
        """
        Returns a mirrored copy of the board.
        The board is mirrored vertically and piece colors are swapped, so that
        the position is equivalent modulo color.
        """
        board = self.transform(flip_vertical)
        board.occupied_co[WHITE], board.occupied_co[BLACK] = board.occupied_co[BLACK], board.occupied_co[WHITE]
        return board

    def copy(self):
        """Creates a copy of the board."""
        board = type(self)(None)

        board.pawns = self.pawns
        board.knights = self.knights
        board.bishops = self.bishops
        board.rooks = self.rooks
        board.queens = self.queens
        board.kings = self.kings

        board.occupied_co[WHITE] = self.occupied_co[WHITE]
        board.occupied_co[BLACK] = self.occupied_co[BLACK]
        board.occupied = self.occupied
        board.promoted = self.promoted

        return board

    def __copy__(self):
        return self.copy()

    def __deepcopy__(self, memo):
        board = self.copy()
        memo[id(self)] = board
        return board

    @classmethod
    def empty(cls):
        """
        Creates a new empty board. Also see
        :func:`~chess.BaseBoard.clear_board()`.
        """
        return cls(None)


class _BoardState:

    def __init__(self, board):
        self.pawns = board.pawns
        self.knights = board.knights
        self.bishops = board.bishops
        self.rooks = board.rooks
        self.queens = board.queens
        self.kings = board.kings

        self.occupied_w = board.occupied_co[WHITE]
        self.occupied_b = board.occupied_co[BLACK]
        self.occupied = board.occupied

        self.promoted = board.promoted

        self.turn = board.turn
        self.castling_rights = board.castling_rights
        self.ep_square = board.ep_square
        self.halfmove_clock = board.halfmove_clock
        self.fullmove_number = board.fullmove_number

    def restore(self, board):
        board.pawns = self.pawns
        board.knights = self.knights
        board.bishops = self.bishops
        board.rooks = self.rooks
        board.queens = self.queens
        board.kings = self.kings

        board.occupied_co[WHITE] = self.occupied_w
        board.occupied_co[BLACK] = self.occupied_b
        board.occupied = self.occupied

        board.promoted = self.promoted

        board.turn = self.turn
        board.castling_rights = self.castling_rights
        board.ep_square = self.ep_square
        board.halfmove_clock = self.halfmove_clock
        board.fullmove_number = self.fullmove_number


class Board(object, BaseBoard):
    """
    A :class:`~chess.BaseBoard` and additional information representing
    a chess position.
    Provides move generation, validation, parsing, attack generation,
    game end detection, move counters and the capability to make and unmake
    moves.
    The board is initialized to the standard chess starting position,
    unless otherwise specified in the optional *fen* argument.
    If *fen* is ``None``, an empty board is created.
    Optionally supports *chess960*. In Chess960 castling moves are encoded
    by a king move to the corresponding rook square.
    Use :func:`chess.Board.from_chess960_pos()` to create a board with one
    of the Chess960 starting positions.
    It's safe to set :data:`~Board.turn`, :data:`~Board.castling_rights`,
    :data:`~Board.ep_square`, :data:`~Board.halfmove_clock` and
    :data:`~Board.fullmove_number` directly.
    """

    aliases = ["Standard", "Chess", "Classical", "Normal"]
    uci_variant = "chess"
    xboard_variant = "normal"
    starting_fen = STARTING_FEN

    tbw_suffix = ".rtbw"
    tbz_suffix = ".rtbz"
    tbw_magic = b"\x71\xe8\x23\x5d"
    tbz_magic = b"\xd7\x66\x0c\xa5"
    pawnless_tbw_suffix = pawnless_tbz_suffix = None
    pawnless_tbw_magic = pawnless_tbz_magic = None
    connected_kings = False
    one_king = True
    captures_compulsory = False

    def __init__(self, fen=STARTING_FEN, *args):
        BaseBoard.__init__(self, None)

        self.move_stack = []
        self._stack = []

        if fen is None:
            self.clear()
        elif fen == type(self).starting_fen:
            self.reset()

    @property
    def pseudo_legal_moves(self):
        return PseudoLegalMoveGenerator(self)

    @property
    def legal_moves(self):
        return LegalMoveGenerator(self)

    def reset(self):
        """Restores the starting position."""
        self.turn = WHITE
        self.castling_rights = BB_CORNERS
        self.ep_square = None
        self.halfmove_clock = 0
        self.fullmove_number = 1

        self.reset_board()

    def reset_board(self):
        super(Board, self).reset_board()
        self.clear_stack()

    def clear(self):
        """
        Clears the board.
        Resets move stack and move counters. The side to move is white. There
        are no rooks or kings, so castling rights are removed.
        In order to be in a valid :func:`~chess.Board.status()` at least kings
        need to be put on the board.
        """
        self.turn = WHITE
        self.castling_rights = BB_EMPTY
        self.ep_square = None
        self.halfmove_clock = 0
        self.fullmove_number = 1

        self.clear_board()

    def clear_board(self):
        super(Board, self).clear_board()
        self.clear_stack()

    def clear_stack(self):
        """Clears the move stack."""
        del self.move_stack[:]
        del self._stack[:]

    def root(self):
        """Returns a copy of the root position."""
        if self._stack:
            board = type(self)(None)
            self._stack[0].restore(board)
            return board
        else:
            return self.copy(stack=False)

    def remove_piece_at(self, square):
        piece = super(Board, self).remove_piece_at(square)
        self.clear_stack()
        return piece

    def set_piece_at(self, square, piece, promoted=False):
        super(Board, self).set_piece_at(square, piece, promoted=promoted)
        self.clear_stack()

    def generate_pseudo_legal_moves(self, from_mask=BB_ALL, to_mask=BB_ALL):
        our_pieces = self.occupied_co[self.turn]

        # Generate piece moves.
        non_pawns = our_pieces & ~self.pawns & from_mask
        for from_square in scan_reversed(non_pawns):
            moves = self.attacks_mask(from_square) & ~our_pieces & to_mask
            for to_square in scan_reversed(moves):
                yield Move(from_square, to_square)

        # Generate castling moves.
        if from_mask & self.kings:
            for res in self.generate_castling_moves(from_mask,to_mask):
                yield res

        # The remaining moves are all pawn moves.
        pawns = self.pawns & self.occupied_co[self.turn] & from_mask
        if not pawns:
            return

        # Generate pawn captures.
        capturers = pawns
        for from_square in scan_reversed(capturers):
            targets = (
                BB_PAWN_ATTACKS[self.turn][from_square] &
                self.occupied_co[not self.turn] & to_mask)

            for to_square in scan_reversed(targets):
                if square_rank(to_square) in [0, 7]:
                    yield Move(from_square, to_square, QUEEN)
                    yield Move(from_square, to_square, ROOK)
                    yield Move(from_square, to_square, BISHOP)
                    yield Move(from_square, to_square, KNIGHT)
                else:
                    yield Move(from_square, to_square)

        # Prepare pawn advance generation.
        if self.turn == WHITE:
            single_moves = pawns << 8 & ~self.occupied
            double_moves = single_moves << 8 & ~self.occupied & (BB_RANK_3 | BB_RANK_4)
        else:
            single_moves = pawns >> 8 & ~self.occupied
            double_moves = single_moves >> 8 & ~self.occupied & (BB_RANK_6 | BB_RANK_5)

        single_moves &= to_mask
        double_moves &= to_mask

        # Generate single pawn moves.
        for to_square in scan_reversed(single_moves):
            from_square = to_square + (8 if self.turn == BLACK else -8)

            if square_rank(to_square) in [0, 7]:
                yield Move(from_square, to_square, QUEEN)
                yield Move(from_square, to_square, ROOK)
                yield Move(from_square, to_square, BISHOP)
                yield Move(from_square, to_square, KNIGHT)
            else:
                yield Move(from_square, to_square)

        # Generate double pawn moves.
        for to_square in scan_reversed(double_moves):
            from_square = to_square + (16 if self.turn == BLACK else -16)
            yield Move(from_square, to_square)

        # Generate en passant captures.
        if self.ep_square:
            for res in self.generate_pseudo_legal_ep(from_mask, to_mask):
                yield res

    def generate_pseudo_legal_ep(self, from_mask=BB_ALL, to_mask=BB_ALL):
        if not self.ep_square or not BB_SQUARES[self.ep_square] & to_mask:
            return

        if BB_SQUARES[self.ep_square] & self.occupied:
            return

        capturers = (
            self.pawns & self.occupied_co[self.turn] & from_mask &
            BB_PAWN_ATTACKS[not self.turn][self.ep_square] &
            BB_RANKS[4 if self.turn else 3])

        for capturer in scan_reversed(capturers):
            yield Move(capturer, self.ep_square)

    def generate_pseudo_legal_captures(self, from_mask=BB_ALL, to_mask=BB_ALL):
        return itertools.chain(
            self.generate_pseudo_legal_moves(from_mask, to_mask & self.occupied_co[not self.turn]),
            self.generate_pseudo_legal_ep(from_mask, to_mask))

    def is_check(self):
        """Returns if the current side to move is in check."""
        king = self.king(self.turn)
        return king is not None and self.is_attacked_by(not self.turn, king)

    def is_into_check(self, move):
        """
        Checks if the given move would leave the king in check or put it into
        check. The move must be at least pseudo legal.
        """
        king = self.king(self.turn)
        if king is None:
            return False

        checkers = self.attackers_mask(not self.turn, king)
        if checkers:
            # If already in check, look if it is an evasion.
            if move not in self._generate_evasions(king, checkers, BB_SQUARES[move.from_square], BB_SQUARES[move.to_square]):
                return True

        return not self._is_safe(king, self._slider_blockers(king), move)

    def was_into_check(self):
        """
        Checks if the king of the other side is attacked. Such a position is not
        valid and could only be reached by an illegal move.
        """
        king = self.king(not self.turn)
        return king is not None and self.is_attacked_by(self.turn, king)

    def is_pseudo_legal(self, move):
        # Null moves are not pseudo legal.
        if not move:
            return False

        # Drops are not pseudo legal.
        if move.drop:
            return False

        # Source square must not be vacant.
        piece = self.piece_type_at(move.from_square)
        if not piece:
            return False

        # Get square masks.
        from_mask = BB_SQUARES[move.from_square]
        to_mask = BB_SQUARES[move.to_square]

        # Check turn.
        if not self.occupied_co[self.turn] & from_mask:
            return False

        # Only pawns can promote and only on the back rank.
        if move.promotion:
            if piece != PAWN:
                return False

            if self.turn == WHITE and square_rank(move.to_square) != 7:
                return False
            elif self.turn == BLACK and square_rank(move.to_square) != 0:
                return False

        # Handle castling.
        if piece == KING:
            if move in self.generate_castling_moves():
                return True

        # Destination square can not be occupied.
        if self.occupied_co[self.turn] & to_mask:
            return False

        # Handle pawn moves.
        if piece == PAWN:
            return move in self.generate_pseudo_legal_moves(from_mask, to_mask)

        # Handle all other pieces.
        return bool(self.attacks_mask(move.from_square) & to_mask)

    def is_legal(self, move):
        return self.is_pseudo_legal(move) and not self.is_into_check(move)

    def is_game_over(self, claim_draw=False, *args):
        """
        Checks if the game is over due to
        :func:`checkmate <chess.Board.is_checkmate()>`,
        :func:`stalemate <chess.Board.is_stalemate()>`,
        :func:`insufficient material <chess.Board.is_insufficient_material()>`,
        the :func:`seventyfive-move rule <chess.Board.is_seventyfive_moves()>`,
        :func:`fivefold repetition <chess.Board.is_fivefold_repetition()>`
        or a :func:`variant end condition <chess.Board.is_variant_end()>`.
        The game is not considered to be over by the
        :func:`fifty-move rule <chess.Board.can_claim_fifty_moves()>` or
        :func:`threefold repetition <chess.Board.can_claim_threefold_repetition()>`,
        unless *claim_draw* is given. Note that checking the latter can be
        slow.
        """
        # Seventyfive-move rule.
        if self.is_seventyfive_moves():
            return True

        # Insufficient material.
        if self.is_insufficient_material():
            return True

        # Stalemate or checkmate.
        if not any(self.generate_legal_moves()):
            return True

        # Fivefold repetition.
        if self.is_fivefold_repetition():
            return True

        # Claim draw.
        if claim_draw and self.can_claim_draw():
            return True

        return False

    def result(self, claim_draw=False, *args):
        """
        Gets the game result.
        ``1-0``, ``0-1`` or ``1/2-1/2`` if the
        :func:`game is over <chess.Board.is_game_over()>`. Otherwise, the
        result is undetermined: ``*``.
        """

        # Checkmate.
        if self.is_checkmate():
            return "0-1" if self.turn == WHITE else "1-0"

        # Draw claimed.
        if claim_draw and self.can_claim_draw():
            return "1/2-1/2"

        # Seventyfive-move rule or fivefold repetition.
        if self.is_seventyfive_moves() or self.is_fivefold_repetition():
            return "1/2-1/2"

        # Insufficient material.
        if self.is_insufficient_material():
            return "1/2-1/2"

        # Stalemate.
        if not any(self.generate_legal_moves()):
            return "1/2-1/2"

        # Undetermined.
        return "*"

    def is_checkmate(self):
        """Checks if the current position is a checkmate."""
        if not self.is_check():
            return False

        return not any(self.generate_legal_moves())

    def is_stalemate(self):
        """Checks if the current position is a stalemate."""
        if self.is_check():
            return False

        return not any(self.generate_legal_moves())

    def is_insufficient_material(self):
        """
        Checks if neither side has sufficient winning material
        (:func:`~chess.Board.has_insufficient_material()`).
        """
        return all(self.has_insufficient_material(color) for color in COLORS)

    def has_insufficient_material(self, color):
        """
        Checks if *color* has insufficient winning material.
        This is guaranteed to return ``False`` if *color* can still win the
        game.
        The converse does not necessarily hold:
        The implementation only looks at the material, including the colors
        of bishops, but not considering piece positions. So fortress
        positions or positions with forced lines may return ``False``, even
        though there is no possible winning line.
        """
        if self.occupied_co[color] & (self.pawns | self.rooks | self.queens):
            return False

        # Knights are only insufficient material if:
        # (1) We do not have any other pieces, including more than one knight.
        # (2) The opponent does not have pawns, knights, bishops or rooks.
        #     These would allow self mate.
        if self.occupied_co[color] & self.knights:
            return (popcount(self.occupied_co[color]) <= 2 and
                    not (self.occupied_co[not color] & ~self.kings & ~self.queens))

        # Bishops are only insufficient material if:
        # (1) We no dot have any other pieces, including bishops of the
        #     opposite color.
        # (2) The opponent does not have bishops of the opposite color,
        #     pawns or knights. These would allow self mate.
        if self.occupied_co[color] & self.bishops:
            same_color = (not self.bishops & BB_DARK_SQUARES) or (not self.bishops & BB_LIGHT_SQUARES)
            return same_color and not (self.occupied_co[not color] & ~self.kings & ~self.rooks & ~self.queens)

        return True

    def is_seventyfive_moves(self):
        """
        Since the 1st of July 2014, a game is automatically drawn (without
        a claim by one of the players) if the half-move clock since a capture
        or pawn move is equal to or grather than 150. Other means to end a game
        take precedence.
        """
        if self.halfmove_clock >= 150:
            if any(self.generate_legal_moves()):
                return True

        return False

    def is_fivefold_repetition(self):
        """
        Since the 1st of July 2014 a game is automatically drawn (without
        a claim by one of the players) if a position occurs for the fifth time.
        Originally this had to occur on consecutive alternating moves, but
        this has since been revised.
        """
        transposition_key = self._transposition_key()
        repetitions = 1
        switchyard = []

        while self.move_stack and repetitions < 5:
            move = self.pop()
            switchyard.append(move)

            if self.is_irreversible(move):
                break

            if self._transposition_key() == transposition_key:
                repetitions += 1

        while switchyard:
            self.push(switchyard.pop())

        return repetitions >= 5

    def can_claim_draw(self):
        """
        Checks if the side to move can claim a draw by the fifty-move rule or
        by threefold repetition.
        Note that checking the latter can be slow.
        """
        return self.can_claim_fifty_moves() or self.can_claim_threefold_repetition()

    def can_claim_fifty_moves(self):
        """
        Draw by the fifty-move rule can be claimed once the clock of halfmoves
        since the last capture or pawn move becomes equal or greater to 100
        and the side to move still has a legal move they can make.
        """
        # Fifty-move rule.
        if self.halfmove_clock >= 100:
            if any(self.generate_legal_moves()):
                return True

        return False

    def can_claim_threefold_repetition(self):
        """
        Draw by threefold repetition can be claimed if the position on the
        board occured for the third time or if such a repetition is reached
        with one of the possible legal moves.
        Note that checking this can be slow: In the worst case
        scenario every legal move has to be tested and the entire game has to
        be replayed because there is no incremental transposition table.
        """
        transposition_key = self._transposition_key()
        transpositions = collections.Counter()
        transpositions.update((transposition_key, ))

        # Count positions.
        switchyard = []
        while self.move_stack:
            move = self.pop()
            switchyard.append(move)

            if self.is_irreversible(move):
                break

            transpositions.update((self._transposition_key(), ))

        while switchyard:
            self.push(switchyard.pop())

        # Threefold repetition occured.
        if transpositions[transposition_key] >= 3:
            return True

        # The next legal move is a threefold repetition.
        for move in self.generate_legal_moves():
            self.push(move)

            if transpositions[self._transposition_key()] >= 2:
                self.pop()
                return True

            self.pop()

        return False

    def _board_state(self):
        return _BoardState(self)

    def _push_capture(self, move, capture_square, piece_type, was_promoted):
        pass

    def push(self, move):
        """
        Updates the position with the given move and puts it onto the
        move stack.

        board = chess.Board()

        Nf3 = chess.Move.from_uci("g1f3")
        board.push(Nf3)  # Make the move
        board.pop()  # Unmake the last move
        Move.from_uci('g1f3')
        Null moves just increment the move counters, switch turns and forfeit
        en passant capturing.
        :warning: Moves are not checked for legality.
        """
        # Push move and remember board state.
        self.move_stack.append(self._not_drop(move.from_square, move.to_square, move.promotion, move.drop))
        self._stack.append(self._board_state())

        # Reset en passant square.
        ep_square = self.ep_square
        self.ep_square = None

        # Increment move counters.
        self.halfmove_clock += 1
        if self.turn == BLACK:
            self.fullmove_number += 1

        # On a null move, simply swap turns and reset the en passant square.
        if not move:
            self.turn = not self.turn
            return

        # Drops.
        if move.drop:
            self._set_piece_at(move.to_square, move.drop, self.turn)
            self.turn = not self.turn
            return

        # Zero the half-move clock.
        if self.is_zeroing(move):
            self.halfmove_clock = 0

        from_bb = BB_SQUARES[move.from_square]
        to_bb = BB_SQUARES[move.to_square]

        promoted = self.promoted & from_bb
        piece_type = self._remove_piece_at(move.from_square)
        capture_square = move.to_square
        captured_piece_type = self.piece_type_at(capture_square)

        # Update castling rights.
        self.castling_rights = self.clean_castling_rights() & ~to_bb & ~from_bb
        if piece_type == KING and not promoted:
            if self.turn == WHITE:
                self.castling_rights &= ~BB_RANK_1
            else:
                self.castling_rights &= ~BB_RANK_8
        elif captured_piece_type == KING and not self.promoted & to_bb:
            if self.turn == WHITE and square_rank(move.to_square) == 7:
                self.castling_rights &= ~BB_RANK_8
            elif self.turn == BLACK and square_rank(move.to_square) == 0:
                self.castling_rights &= ~BB_RANK_1

        # Handle special pawn moves.
        if piece_type == PAWN:
            diff = move.to_square - move.from_square

            if diff == 16 and square_rank(move.from_square) == 1:
                self.ep_square = move.from_square + 8
            elif diff == -16 and square_rank(move.from_square) == 6:
                self.ep_square = move.from_square - 8
            elif move.to_square == ep_square and abs(diff) in [7, 9] and not captured_piece_type:
                # Remove pawns captured en passant.
                down = -8 if self.turn == WHITE else 8
                capture_square = ep_square + down
                captured_piece_type = self._remove_piece_at(capture_square)

        # Promotion.
        if move.promotion:
            promoted = True
            piece_type = move.promotion

        # Castling.
        castling = piece_type == KING and self.occupied_co[self.turn] & to_bb
        if castling:
            a_side = square_file(move.to_square) < square_file(move.from_square)

            self._remove_piece_at(move.from_square)
            self._remove_piece_at(move.to_square)

            if a_side:
                self._set_piece_at(C1 if self.turn == WHITE else C8, KING, self.turn)
                self._set_piece_at(D1 if self.turn == WHITE else D8, ROOK, self.turn)
            else:
                self._set_piece_at(G1 if self.turn == WHITE else G8, KING, self.turn)
                self._set_piece_at(F1 if self.turn == WHITE else F8, ROOK, self.turn)

        # Put the piece on the target square.
        if not castling:
            was_promoted = self.promoted & to_bb
            self._set_piece_at(move.to_square, piece_type, self.turn, promoted)

            if captured_piece_type:
                self._push_capture(move, capture_square, captured_piece_type, was_promoted)

        # Swap turn.
        self.turn = not self.turn

    def pop(self):
        """
        Restores the previous position and returns the last move from the stack.
        :raises: :exc:`IndexError` if the stack is empty.
        """
        move = self.move_stack.pop()
        self._stack.pop().restore(self)
        return move

    def peek(self):
        """
        Gets the last move from the move stack.
        :raises: :exc:`IndexError` if the move stack is empty.
        """
        return self.move_stack[-1]

    def castling_shredder_fen(self):
        castling_rights = self.clean_castling_rights()
        if not castling_rights:
            return "-"

        builder = []

        for square in scan_reversed(castling_rights & BB_RANK_1):
            builder.append(FILE_NAMES[square_file(square)].upper())

        for square in scan_reversed(castling_rights & BB_RANK_8):
            builder.append(FILE_NAMES[square_file(square)])

        return "".join(builder)

    def castling_xfen(self):
        builder = []

        for color in COLORS:
            king = self.king(color)
            if king is None:
                continue

            king_file = square_file(king)
            backrank = BB_RANK_1 if color == WHITE else BB_RANK_8

            for rook_square in scan_reversed(self.clean_castling_rights() & backrank):
                rook_file = square_file(rook_square)
                a_side = rook_file < king_file

                other_rooks = self.occupied_co[color] & self.rooks & backrank & ~BB_SQUARES[rook_square]

                if any((square_file(other) < rook_file) == a_side for other in scan_reversed(other_rooks)):
                    ch = FILE_NAMES[rook_file]
                else:
                    ch = "q" if a_side else "k"

                builder.append(ch.upper() if color == WHITE else ch)

        if builder:
            return "".join(builder)
        else:
            return "-"

    def has_pseudo_legal_en_passant(self):
        """Checks if there is a pseudo-legal en passant capture."""
        return self.ep_square and any(self.generate_pseudo_legal_ep())

    def has_legal_en_passant(self):
        """Checks if there is a legal en passant capture."""
        return self.ep_square and any(self.generate_legal_ep())

    def set_piece_map(self, pieces):
        super(Board, self).set_piece_map(pieces)
        self.clear_stack()

        self.clear_stack()

    def san(self, move):
        """
        Gets the standard algebraic notation of the given move in the context
        of the current position.
        """
        return self._algebraic(move)

    def lan(self, move):
        """
        Gets the long algebraic notation of the given move in the context of
        the current position.
        """
        return self._algebraic(move, long=True)

    def _algebraic(self, move, long=False):
        if not move:
            # Null move.
            return "--"

        # Look ahead for check or checkmate.
        self.push(move)
        is_check = self.is_check()
        is_checkmate = (is_check and self.is_checkmate())
        self.pop()

        # Drops.
        if move.drop:
            san = ""
            if move.drop != PAWN:
                san = PIECE_SYMBOLS[move.drop].upper()
            san += "@" + SQUARE_NAMES[move.to_square]

        # Castling.
        if self.is_castling(move):
            if square_file(move.to_square) < square_file(move.from_square):
                san = "O-O-O"
            else:
                san = "O-O"

        if move.drop or self.is_castling(move):
            if is_checkmate:
                return san + "#"
            elif is_check:
                return san + "+"
            else:
                return san

        piece_type = self.piece_type_at(move.from_square)
        assert piece_type, "san() and lan() expect move to be legal or null, but got {} in {}".format(move, self.fen())
        capture = self.is_capture(move)

        if piece_type == PAWN:
            san = ""
        else:
            san = PIECE_SYMBOLS[piece_type].upper()

        if long:
            san += SQUARE_NAMES[move.from_square]
        elif piece_type != PAWN:
            # Get ambiguous move candidates.
            # Relevant candidates: not exactly the current move,
            # but to the same square.
            others = 0
            from_mask = self.pieces_mask(piece_type, self.turn)
            from_mask &= ~BB_SQUARES[move.from_square]
            to_mask = BB_SQUARES[move.to_square]
            for candidate in self.generate_legal_moves(from_mask, to_mask):
                others |= BB_SQUARES[candidate.from_square]

            # Disambiguate.
            if others:
                row, column = False, False

                if others & BB_RANKS[square_rank(move.from_square)]:
                    column = True

                if others & BB_FILES[square_file(move.from_square)]:
                    row = True
                else:
                    column = True

                if column:
                    san += FILE_NAMES[square_file(move.from_square)]
                if row:
                    san += RANK_NAMES[square_rank(move.from_square)]
        elif capture:
            san += FILE_NAMES[square_file(move.from_square)]

        # Captures.
        if capture:
            san += "x"
        elif long:
            san += "-"

        # Destination square.
        san += SQUARE_NAMES[move.to_square]

        # Promotion.
        if move.promotion:
            san += "=" + PIECE_SYMBOLS[move.promotion].upper()

        # Add check or checkmate suffix.
        if is_checkmate:
            san += "#"
        elif is_check:
            san += "+"

        return san

    def variation_san(self, variation):
        """
        Given a sequence of moves, returns a string representing the sequence
        in standard algebraic notation (e.g., ``1. e4 e5 2. Nf3 Nc6`` or
        ``37...Bg6 38. fxg6``).
        The board will not be modified as a result of calling this.
        :raises: :exc:`ValueError` if any moves in the sequence are illegal.
        """
        board = self.copy(stack=False)
        san = []

        for move in variation:
            if not board.is_legal(move):
                raise ValueError("illegal move {} in position {}".format(move, board.fen()))

            if board.turn == WHITE:
                san.append("{}. {}".format(board.fullmove_number, board.san(move)))
            elif not san:
                san.append("{}...{}".format(board.fullmove_number, board.san(move)))
            else:
                san.append(board.san(move))

            board.push(move)

        return " ".join(san)

    def parse_san(self, san):
        """
        Uses the current position as the context to parse a move in standard
        algebraic notation and returns the corresponding move object.
        The returned move is guaranteed to be either legal or a null move.
        :raises: :exc:`ValueError` if the SAN is invalid or ambiguous.
        """
        # Castling.
        try:
            if san in ["O-O", "O-O+", "O-O#"]:
                return next(move for move in self.generate_castling_moves() if self.is_kingside_castling(move))
            elif san in ["O-O-O", "O-O-O+", "O-O-O#"]:
                return next(move for move in self.generate_castling_moves() if self.is_queenside_castling(move))
        except StopIteration:
            raise ValueError("illegal san: {!r} in {}".format(san, self.fen()))

        # Match normal moves.
        match = SAN_REGEX.match(san)
        if not match:
            # Null moves.
            if san in ["--", "Z0"]:
                return Move.null()

            raise ValueError("invalid san: {!r}".format(san))

        # Get target square.
        to_square = SQUARE_NAMES.index(match.group(4))
        to_mask = BB_SQUARES[to_square]

        # Get the promotion type.
        p = match.group(5)
        promotion = p and PIECE_SYMBOLS.index(p[-1].lower())

        # Filter by piece type.
        if match.group(1):
            piece_type = PIECE_SYMBOLS.index(match.group(1).lower())
            from_mask = self.pieces_mask(piece_type, self.turn)
        else:
            from_mask = self.pawns

        # Filter by source file.
        if match.group(2):
            from_mask &= BB_FILES[FILE_NAMES.index(match.group(2))]

        # Filter by source rank.
        if match.group(3):
            from_mask &= BB_RANKS[int(match.group(3)) - 1]

        # Match legal moves.
        matched_move = None
        for move in self.generate_legal_moves(from_mask, to_mask):
            if move.promotion != promotion:
                continue

            if matched_move:
                raise ValueError("ambiguous san: {!r} in {}".format(san, self.fen()))

            matched_move = move

        if not matched_move:
            raise ValueError("illegal san: {!r} in {}".format(san, self.fen()))

        return matched_move

    def push_san(self, san):
        """
        Parses a move in standard algebraic notation, makes the move and puts
        it on the the move stack.
        Returns the move.
        :raises: :exc:`ValueError` if neither legal nor a null move.
        """
        move = self.parse_san(san)
        self.push(move)
        return move

    def xboard(self, move,):
        if not self.is_castling(move):
            return move.xboard()
        elif self.is_kingside_castling(move):
            return "O-O"
        else:
            return "O-O-O"

    def parse_xboard(self, xboard):
        if xboard == "@@@@":
            return Move.null()
        elif "," in xboard:
            raise ValueError("unsupported multi-leg xboard move: {!r}".format(xboard))
        try:
            return self.parse_san(xboard)
        except ValueError:
            raise ValueError("invalid or illegal xboard move: {!r} in {}".format(xboard, self.fen()))

    def push_xboard(self, xboard):
        move = self.parse_xboard(xboard)
        self.push(move)
        return move

    def is_en_passant(self, move):
        """Checks if the given pseudo-legal move is an en passant capture."""
        return (self.ep_square == move.to_square and
                bool(self.pawns & BB_SQUARES[move.from_square]) and
                abs(move.to_square - move.from_square) in [7, 9] and
                not self.occupied & BB_SQUARES[move.to_square])

    def is_capture(self, move):
        """Checks if the given pseudo-legal move is a capture."""
        return bool(BB_SQUARES[move.to_square] & self.occupied_co[not self.turn]) or self.is_en_passant(move)

    def is_zeroing(self, move):
        """Checks if the given pseudo-legal move is a capture or pawn move."""
        return bool(BB_SQUARES[move.from_square] & self.pawns or BB_SQUARES[move.to_square] & self.occupied_co[not self.turn])

    def is_irreversible(self, move):
        """
        Checks if the given pseudo-legal move is irreversible.
        In standard chess, pawn moves, captures and moves that destroy castling
        rights are irreversible.
        """
        backrank = BB_RANK_1 if self.turn == WHITE else BB_RANK_8
        cr = self.clean_castling_rights() & backrank
        return bool(self.is_zeroing(move) or
                    cr and BB_SQUARES[move.from_square] & self.kings & ~self.promoted or
                    cr & BB_SQUARES[move.from_square] or
                    cr & BB_SQUARES[move.to_square])

    def is_castling(self, move):
        """Checks if the given pseudo-legal move is a castling move."""
        if self.kings & BB_SQUARES[move.from_square]:
            diff = square_file(move.from_square) - square_file(move.to_square)
            return abs(diff) > 1 or bool(self.rooks & self.occupied_co[self.turn] & BB_SQUARES[move.to_square])
        return False

    def is_kingside_castling(self, move):
        """
        Checks if the given pseudo-legal move is a kingside castling move.
        """
        return self.is_castling(move) and square_file(move.to_square) > square_file(move.from_square)

    def is_queenside_castling(self, move):
        """
        Checks if the given pseudo-legal move is a queenside castling move.
        """
        return self.is_castling(move) and square_file(move.to_square) < square_file(move.from_square)

    def clean_castling_rights(self):
        """
        Returns valid castling rights filtered from
        :data:`~chess.Board.castling_rights`.
        """
        if self._stack:
            # Castling rights do not change in a game, so we can assume them to
            # be filtered already.
            return self.castling_rights

        castling = self.castling_rights & self.rooks
        white_castling = castling & BB_RANK_1 & self.occupied_co[WHITE]
        black_castling = castling & BB_RANK_8 & self.occupied_co[BLACK]

        # The rooks must be on a1, h1, a8 or h8.
        white_castling &= (BB_A1 | BB_H1)
        black_castling &= (BB_A8 | BB_H8)

        # The kings must be on e1 or e8.
        if not self.occupied_co[WHITE] & self.kings & ~self.promoted & BB_E1:
            white_castling = 0
        if not self.occupied_co[BLACK] & self.kings & ~self.promoted & BB_E8:
            black_castling = 0

        return white_castling | black_castling

    def has_castling_rights(self, color):
        """Checks if the given side has castling rights."""
        backrank = BB_RANK_1 if color == WHITE else BB_RANK_8
        return bool(self.clean_castling_rights() & backrank)

    def has_kingside_castling_rights(self, color):
        """
        Checks if the given side has kingside (that is h-side in Chess960)
        castling rights.
        """
        backrank = BB_RANK_1 if color == WHITE else BB_RANK_8
        king_mask = self.kings & self.occupied_co[color] & backrank & ~self.promoted
        if not king_mask:
            return False

        castling_rights = self.clean_castling_rights() & backrank
        while castling_rights:
            rook = castling_rights & -castling_rights

            if rook > king_mask:
                return True

            castling_rights = castling_rights & (castling_rights - 1)

        return False

    def has_queenside_castling_rights(self, color):
        """
        Checks if the given side has queenside (that is a-side in Chess960)
        castling rights.
        """
        backrank = BB_RANK_1 if color == WHITE else BB_RANK_8
        king_mask = self.kings & self.occupied_co[color] & backrank & ~self.promoted
        if not king_mask:
            return False

        castling_rights = self.clean_castling_rights() & backrank
        while castling_rights:
            rook = castling_rights & -castling_rights

            if rook < king_mask:
                return True

            castling_rights = castling_rights & (castling_rights - 1)

        return False

    def status(self):
        """
        Gets a bitmask of possible problems with the position.
        Move making, generation and validation are only guaranteed to work on
        a completely valid board.
        :data:`~chess.STATUS_VALID` for a completely valid board.
        Otherwise, bitwise combinations of:
        :data:`~chess.STATUS_NO_WHITE_KING`,
        :data:`~chess.STATUS_NO_BLACK_KING`,
        :data:`~chess.STATUS_TOO_MANY_KINGS`,
        :data:`~chess.STATUS_TOO_MANY_WHITE_PAWNS`,
        :data:`~chess.STATUS_TOO_MANY_BLACK_PAWNS`,
        :data:`~chess.STATUS_PAWNS_ON_BACKRANK`,
        :data:`~chess.STATUS_TOO_MANY_WHITE_PIECES`,
        :data:`~chess.STATUS_TOO_MANY_BLACK_PIECES`,
        :data:`~chess.STATUS_BAD_CASTLING_RIGHTS`,
        :data:`~chess.STATUS_INVALID_EP_SQUARE`,
        :data:`~chess.STATUS_OPPOSITE_CHECK`,
        :data:`~chess.STATUS_EMPTY`,
        :data:`~chess.STATUS_RACE_CHECK`,
        :data:`~chess.STATUS_RACE_OVER`,
        :data:`~chess.STATUS_RACE_MATERIAL`.
        """
        errors = STATUS_VALID

        # There must be at least one piece.
        if not self.occupied:
            errors |= STATUS_EMPTY

        # There must be exactly one king of each color.
        if not self.occupied_co[WHITE] & self.kings:
            errors |= STATUS_NO_WHITE_KING
        if not self.occupied_co[BLACK] & self.kings:
            errors |= STATUS_NO_BLACK_KING
        if popcount(self.occupied & self.kings) > 2:
            errors |= STATUS_TOO_MANY_KINGS

        # There can not be more than 16 pieces of any color.
        if popcount(self.occupied_co[WHITE]) > 16:
            errors |= STATUS_TOO_MANY_WHITE_PIECES
        if popcount(self.occupied_co[BLACK]) > 16:
            errors |= STATUS_TOO_MANY_BLACK_PIECES

        # There can not be more than 8 pawns of any color.
        if popcount(self.occupied_co[WHITE] & self.pawns) > 8:
            errors |= STATUS_TOO_MANY_WHITE_PAWNS
        if popcount(self.occupied_co[BLACK] & self.pawns) > 8:
            errors |= STATUS_TOO_MANY_BLACK_PAWNS

        # Pawns can not be on the back rank.
        if self.pawns & BB_BACKRANKS:
            errors |= STATUS_PAWNS_ON_BACKRANK

        # Castling rights.
        if self.castling_rights != self.clean_castling_rights():
            errors |= STATUS_BAD_CASTLING_RIGHTS

        # En passant.
        if self.ep_square != self._valid_ep_square():
            errors |= STATUS_INVALID_EP_SQUARE

        # Side to move giving check.
        if self.was_into_check():
            errors |= STATUS_OPPOSITE_CHECK

        return errors

    def _valid_ep_square(self):
        if self.ep_square:
            if self.turn == WHITE:
                ep_rank = 5
                pawn_mask = shift_down(BB_SQUARES[self.ep_square])
                seventh_rank_mask = shift_up(BB_SQUARES[self.ep_square])
            else:
                ep_rank = 2
                pawn_mask = shift_up(BB_SQUARES[self.ep_square])
                seventh_rank_mask = shift_down(BB_SQUARES[self.ep_square])

            # The en passant square must be on the third or sixth rank.
            if square_rank(self.ep_square) != ep_rank:
                return

            # The last move must have been a double pawn push, so there must
            # be a pawn of the correct color on the fourth or fifth rank.
            if not self.pawns & self.occupied_co[not self.turn] & pawn_mask:
                return

            # And the en passant square must be empty.
            if self.occupied & BB_SQUARES[self.ep_square]:
                return

            # And the second rank must be empty.
            if self.occupied & seventh_rank_mask:
                return

            return self.ep_square

    def is_valid(self):
        """
        Checks if the board is valid.
        Move making, generation and validation are only guaranteed to work on
        a completely valid board.
        See :func:`~chess.Board.status()` for details.
        """
        return self.status() == STATUS_VALID

    def _ep_skewered(self, king, capturer):
        # Handle the special case where the king would be in check if the
        # pawn and its capturer disappear from the rank.

        # Vertical skewers of the captured pawn are not possible. (Pins on
        # the capturer are not handled here.)

        last_double = self.ep_square + (-8 if self.turn == WHITE else 8)

        occupancy = (self.occupied & ~BB_SQUARES[last_double] &
                     ~BB_SQUARES[capturer] | BB_SQUARES[self.ep_square])

        # Horizontal attack on the fifth or fourth rank.
        horizontal_attackers = self.occupied_co[not self.turn] & (self.rooks | self.queens)
        if BB_RANK_ATTACKS[king][BB_RANK_MASKS[king] & occupancy] & horizontal_attackers:
            return True

        # Diagonal skewers. These are not actually possible in a real game,
        # because if the latest double pawn move covers a diagonal attack,
        # then the other side would have been in check already.
        diagonal_attackers = self.occupied_co[not self.turn] & (self.bishops | self.queens)
        if BB_DIAG_ATTACKS[king][BB_DIAG_MASKS[king] & occupancy] & diagonal_attackers:
            return True

        return False

    def _slider_blockers(self, king):
        rooks_and_queens = self.rooks | self.queens
        bishops_and_queens = self.bishops | self.queens

        snipers = ((BB_RANK_ATTACKS[king][0] & rooks_and_queens) |
                   (BB_FILE_ATTACKS[king][0] & rooks_and_queens) |
                   (BB_DIAG_ATTACKS[king][0] & bishops_and_queens))

        blockers = 0

        for sniper in scan_reversed(snipers & self.occupied_co[not self.turn]):
            b = BB_BETWEEN[king][sniper] & self.occupied

            # Add to blockers if exactly one piece in-between.
            if b and BB_SQUARES[msb(b)] == b:
                blockers |= b

        return blockers & self.occupied_co[self.turn]

    def _is_safe(self, king, blockers, move):
        if move.from_square == king:
            if self.is_castling(move):
                return True
            else:
                return not self.is_attacked_by(not self.turn, move.to_square)
        elif self.is_en_passant(move):
            return (self.pin_mask(self.turn, move.from_square) & BB_SQUARES[move.to_square] and
                    not self._ep_skewered(king, move.from_square))
        else:
            return (not blockers & BB_SQUARES[move.from_square] or
                    BB_RAYS[move.from_square][move.to_square] & BB_SQUARES[king])

    def _generate_evasions(self, king, checkers, from_mask=BB_ALL, to_mask=BB_ALL):
        sliders = checkers & (self.bishops | self.rooks | self.queens)

        attacked = 0
        for checker in scan_reversed(sliders):
            attacked |= BB_RAYS[king][checker] & ~BB_SQUARES[checker]

        if BB_SQUARES[king] & from_mask:
            for to_square in scan_reversed(BB_KING_ATTACKS[king] & ~self.occupied_co[self.turn] & ~attacked & to_mask):
                yield Move(king, to_square)

        checker = msb(checkers)
        if BB_SQUARES[checker] == checkers:
            # Capture or block a single checker.
            target = BB_BETWEEN[king][checker] | checkers

            for res in self.generate_pseudo_legal_moves(~self.kings & from_mask, target & to_mask):
                yield res

            # Capture the checking pawn en passant (but avoid yielding
            # duplicate moves).
            if self.ep_square and not BB_SQUARES[self.ep_square] & target:
                last_double = self.ep_square + (-8 if self.turn == WHITE else 8)
                if last_double == checker:
                    for res in self.generate_pseudo_legal_ep(from_mask, to_mask):
                        yield res

    def generate_legal_moves(self, from_mask=BB_ALL, to_mask=BB_ALL):

        king_mask = self.kings & self.occupied_co[self.turn]
        if king_mask:
            king = msb(king_mask)
            blockers = self._slider_blockers(king)
            checkers = self.attackers_mask(not self.turn, king)
            if checkers:
                for move in self._generate_evasions(king, checkers, from_mask, to_mask):
                    if self._is_safe(king, blockers, move):
                        yield move
            else:
                for move in self.generate_pseudo_legal_moves(from_mask, to_mask):
                    if self._is_safe(king, blockers, move):
                        yield move
        else:
            for res in self.generate_pseudo_legal_moves(from_mask, to_mask):
                yield res

    def generate_legal_ep(self, from_mask=BB_ALL, to_mask=BB_ALL):

        for move in self.generate_pseudo_legal_ep(from_mask, to_mask):
            if not self.is_into_check(move):
                yield move

    def generate_legal_captures(self, from_mask=BB_ALL, to_mask=BB_ALL):
        return itertools.chain(
            self.generate_legal_moves(from_mask, to_mask & self.occupied_co[not self.turn]),
            self.generate_legal_ep(from_mask, to_mask))

    def _attacked_for_king(self, path, occupied):
        return any(self._attackers_mask(not self.turn, sq, occupied) for sq in scan_reversed(path))

    def _castling_uncovers_rank_attack(self, rook_bb, king_to):
        # Test the special case where we castle and our rook shielded us from
        # an attack, so castling would be into check.
        rank_pieces = BB_RANK_MASKS[king_to] & (self.occupied ^ rook_bb)
        sliders = (self.queens | self.rooks) & self.occupied_co[not self.turn]
        return BB_RANK_ATTACKS[king_to][rank_pieces] & sliders

    def generate_castling_moves(self, from_mask=BB_ALL, to_mask=BB_ALL):

        backrank = BB_RANK_1 if self.turn == WHITE else BB_RANK_8
        king = self.occupied_co[self.turn] & self.kings & ~self.promoted & backrank & from_mask
        king = king & -king
        if not king or self._attacked_for_king(king, self.occupied):
            return

        bb_c = BB_FILE_C & backrank
        bb_d = BB_FILE_D & backrank
        bb_f = BB_FILE_F & backrank
        bb_g = BB_FILE_G & backrank

        for candidate in scan_reversed(self.clean_castling_rights() & backrank & to_mask):
            rook = BB_SQUARES[candidate]

            a_side = rook < king

            empty_for_rook = 0
            empty_for_king = 0

            if a_side:
                king_to = msb(bb_c)
                if not rook & bb_d:
                    empty_for_rook = BB_BETWEEN[candidate][msb(bb_d)] | bb_d
                if not king & bb_c:
                    empty_for_king = BB_BETWEEN[msb(king)][king_to] | bb_c
            else:
                king_to = msb(bb_g)
                if not rook & bb_f:
                    empty_for_rook = BB_BETWEEN[candidate][msb(bb_f)] | bb_f
                if not king & bb_g:
                    empty_for_king = BB_BETWEEN[msb(king)][king_to] | bb_g

            if not ((self.occupied ^ king ^ rook) & (empty_for_king | empty_for_rook) or
                        self._attacked_for_king(empty_for_king, self.occupied ^ king) or
                        self._castling_uncovers_rank_attack(rook, king_to)):
                yield self._not_drop(msb(king), candidate)

    def _not_drop(self, from_square, to_square, promotion=None, drop=None):
        if drop is None:
            return Move(from_square, to_square, promotion, drop)

    def _transposition_key(self):
        return (self.pawns, self.knights, self.bishops, self.rooks,
                self.queens, self.kings,
                self.occupied_co[WHITE], self.occupied_co[BLACK],
                self.turn, self.clean_castling_rights(),
                self.ep_square if self.has_legal_en_passant() else None)

    def __eq__(self, board):
        try:
            # Compare positions (including move counters), but excluding
            # history.
            return (
                self.halfmove_clock == board.halfmove_clock and
                self.fullmove_number == board.fullmove_number and
                type(self).uci_variant == type(board).uci_variant and
                self._transposition_key() == board._transposition_key())
        except AttributeError:
            return NotImplemented

    def apply_transform(self, f):
        super(Board, self).apply_transform(f)
        self.clear_stack()

    def transform(self, f):
        board = self.copy(stack=False)
        board.apply_transform(f)
        board.ep_square = None if self.ep_square is None else msb(f(BB_SQUARES[self.ep_square]))
        board.castling_rights = f(self.castling_rights)
        return board

    def mirror(self):
        board = super(Board, self).mirror()
        board.turn = not self.turn
        return board

    def copy(self, stack=True, *args):
        """
        Creates a copy of the board.
        Defaults to copying the entire move stack. Alternatively, *stack* can
        be ``False``, or an integer to copy a limited number of moves.
        """
        board = super(Board, self).copy()

        board.ep_square = self.ep_square
        board.castling_rights = self.castling_rights
        board.turn = self.turn
        board.fullmove_number = self.fullmove_number
        board.halfmove_clock = self.halfmove_clock

        if stack:
            stack = len(self.move_stack) if stack is True else stack
            board.move_stack = [copy.copy(move) for move in self.move_stack[-stack:]]
            board._stack = self._stack[-stack:]

        return board

    @classmethod
    def empty(cls, *args):
        """Creates a new empty board. Also see :func:`~chess.Board.clear()`."""
        return cls(None)


class PseudoLegalMoveGenerator:

    def __init__(self, board):
        self.board = board

    def __bool__(self):
        return any(self.board.generate_pseudo_legal_moves())

    def count(self):
        # List conversion is faster than iterating.
        return len(list(self))

    def __iter__(self):
        return self.board.generate_pseudo_legal_moves()

    def __contains__(self, move):
        return self.board.is_pseudo_legal(move)

    def __repr__(self):
        builder = []

        for move in self:
            if self.board.is_legal(move):
                builder.append(self.board.san(move))
            else:
                builder.append(self.board.uci(move))

        sans = ", ".join(builder)
        return "<PseudoLegalMoveGenerator at {:#x} ({})>".format(id(self), sans)


class LegalMoveGenerator:

    def __init__(self, board):
        self.board = board

    def __bool__(self):
        return any(self.board.generate_legal_moves())

    def count(self):
        # List conversion is faster than iterating.
        return len(list(self))

    def __iter__(self):
        return self.board.generate_legal_moves()

    def __contains__(self, move):
        return self.board.is_legal(move)

    def __repr__(self):
        sans = ", ".join(self.board.san(move) for move in self)
        return "<LegalMoveGenerator at {:#x} ({})>".format(id(self), sans)


class SquareSet(collections.MutableSet):
    """
    A set of squares.

    squares = chess.SquareSet([chess.A8, chess.A1])
    squares
    SquareSet(0x0100000000000001)
    squares = chess.SquareSet(chess.BB_A8 | chess.BB_RANK_1)
    squares
    SquareSet(0x01000000000000ff)
    print(squares)
    1 . . . . . . .
    . . . . . . . .
    . . . . . . . .
    . . . . . . . .
    . . . . . . . .
    . . . . . . . .
    . . . . . . . .
    1 1 1 1 1 1 1 1
    len(squares)
    9
    bool(squares)
    True
    chess.B1 in squares
    True
    for square in squares:
        # 0 -- chess.A1
        # 1 -- chess.B1
        # 2 -- chess.C1
        # 3 -- chess.D1
        # 4 -- chess.E1
        # 5 -- chess.F1
        # 6 -- chess.G1
        # 7 -- chess.H1
        # 56 -- chess.A8
        print(square)
    0
    1
    2
    3
    4
    5
    6
    7
    56
    list(squares)
    [0, 1, 2, 3, 4, 5, 6, 7, 56]
    Square sets are internally represented by 64-bit integer masks of the
    included squares. Bitwise operations can be used to compute unions,
    intersections and shifts.
    int(squares)
    72057594037928191
    Also supports common set operations like
    :func:`~chess.SquareSet.issubset()`, :func:`~chess.SquareSet.issuperset()`,
    :func:`~chess.SquareSet.union()`, :func:`~chess.SquareSet.intersection()`,
    :func:`~chess.SquareSet.difference()`,
    :func:`~chess.SquareSet.symmetric_difference()` and
    :func:`~chess.SquareSet.copy()` as well as
    :func:`~chess.SquareSet.update()`,
    :func:`~chess.SquareSet.intersection_update()`,
    :func:`~chess.SquareSet.difference_update()`,
    :func:`~chess.SquareSet.symmetric_difference_update()` and
    :func:`~chess.SquareSet.clear()`.
    """

    def __init__(self, squares=BB_EMPTY):

        try:
            self.mask = squares.__int__() & BB_ALL
            return
        except AttributeError:
            self.mask = 0

        # Try squares as an iterable. Not under except clause for nicer
        # backtraces.
        for square in squares:
            self.add(square)

    # Set

    def __contains__(self, square):
        return bool(BB_SQUARES[square] & self.mask)

    def __iter__(self):
        return scan_forward(self.mask)

    def __reversed__(self):
        return scan_reversed(self.mask)

    def __len__(self):
        return popcount(self.mask)

    # MutableSet

    def add(self, square):
        """Adds a square to the set."""
        self.mask |= BB_SQUARES[square]

    def discard(self, square):
        """Discards a square from the set."""
        self.mask &= ~BB_SQUARES[square]

    # frozenset

    def isdisjoint(self, other):
        """Test if the square sets are disjoint."""
        return not bool(self & other)

    def issubset(self, other):
        """Test if this square set is a subset of another."""
        return not bool(~self & other)

    def issuperset(self, other):
        """Test if this square set is a superset of another."""
        return not bool(self & ~other)

    def union(self, other):
        return self | other

    def __or__(self, other):
        r = SquareSet(other)
        r.mask |= self.mask
        return r

    def intersection(self, other):
        return self & other

    def __and__(self, other):
        r = SquareSet(other)
        r.mask &= self.mask
        return r

    def difference(self, other):
        return self - other

    def __sub__(self, other):
        r = SquareSet(other)
        r.mask = self.mask & ~r.mask
        return r

    def symmetric_difference(self, other):
        return self ^ other

    def __xor__(self, other):
        r = SquareSet(other)
        r.mask ^= self.mask
        return r

    def copy(self):
        return SquareSet(self.mask)

    # set

    def update(self, *others):
        for other in others:
            self |= other

    def __ior__(self, other):
        self.mask |= SquareSet(other).mask
        return self

    def intersection_update(self, *others):
        for other in others:
            self &= other

    def __iand__(self, other):
        self.mask &= SquareSet(other).mask
        return self

    def difference_update(self, other):
        self -= other

    def __isub__(self, other):
        self.mask &= ~SquareSet(other).mask
        return self

    def symmetric_difference_update(self, other):
        self ^= other

    def __ixor__(self, other):
        self.mask ^= SquareSet(other).mask
        return self

    def remove(self, square):
        """
        Removes a square from the set.
        :raises: :exc:`KeyError` if the given square was not in the set.
        """
        mask = BB_SQUARES[square]
        if self.mask & mask:
            self.mask ^= mask
        else:
            raise KeyError(square)

    def pop(self):
        """
        Removes a square from the set and returns it.
        :raises: :exc:`KeyError` on an empty set.
        """
        if not self.mask:
            raise KeyError("pop from empty SquareSet")

        square = lsb(self.mask)
        self.mask &= (self.mask - 1)
        return square

    def clear(self):
        """Remove all elements from this set."""
        self.mask = BB_EMPTY

    # SquareSet

    def carry_rippler(self):
        """Iterator over the subsets of this set."""
        return _carry_rippler(self.mask)

    def mirror(self):
        """Returns a vertically mirrored copy of this square set."""
        return SquareSet(flip_vertical(self.mask))

    def tolist(self):
        """Convert the set to a list of 64 bools."""
        result = [False] * 64
        for square in self:
            result[square] = True
        return result

    def __bool__(self):
        return bool(self.mask)

    def __eq__(self, other):
        try:
            return self.mask == SquareSet(other).mask
        except (TypeError, ValueError):
            return NotImplemented

    def __lshift__(self, shift):
        return SquareSet((self.mask << shift) & BB_ALL)

    def __rshift__(self, shift):
        return SquareSet(self.mask >> shift)

    def __ilshift__(self, shift):
        self.mask = (self.mask << shift) & BB_ALL
        return self

    def __irshift__(self, shift):
        self.mask >>= shift
        return self

    def __invert__(self):
        return SquareSet(~self.mask & BB_ALL)

    def __int__(self):
        return self.mask

    def __index__(self):
        return self.mask

    def __repr__(self):
        return "SquareSet({0:#018x})".format(self.mask)

    def __str__(self):
        builder = []

        for square in SQUARES_180:
            mask = BB_SQUARES[square]
            builder.append("1" if self.mask & mask else ".")

            if not mask & BB_FILE_H:
                builder.append(" ")
            elif square != H1:
                builder.append("\n")

        return "".join(builder)

    @classmethod
    def from_square(cls, square):
        """
        Creates a :class:`~chess.SquareSet` from a single square.

        chess.SquareSet.from_square(chess.A1) == chess.BB_A1
        True
        """
        return cls(BB_SQUARES[square])
