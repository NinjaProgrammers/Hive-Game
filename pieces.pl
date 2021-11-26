:- module(pieces, [piece/1, board_name/2, color/2, type/2]).

piece(white_bee).
piece(white_beetle_1).
piece(white_beetle_2).
piece(white_grasshoper_1).
piece(white_grasshoper_2).
piece(white_grasshoper_3).
piece(white_spider_1).
piece(white_spider_2).
piece(white_ant_1).
piece(white_ant_2).
piece(white_ant_3).

%piece(white_ladybug).
%piece(white_mosquito).
%piece(white_pillbug).

piece(black_bee).
piece(black_beetle_1).
piece(black_beetle_2).
piece(black_grasshoper_1).
piece(black_grasshoper_2).
piece(black_grasshoper_3).
piece(black_spider_1).
piece(black_spider_2).
piece(black_ant_1).
piece(black_ant_2).
piece(black_ant_3).

%piece(black_ladybug).
%piece(black_mosquito).
%piece(black_pillbug).


%Board Names

board_name(white_bee,[' ','B',' ']).
board_name(white_beetle_1,['B','T','1']).
board_name(white_beetle_2,['B','T','2']).
board_name(white_grasshoper_1,['G','1',' ']).
board_name(white_grasshoper_2,['G','2',' ']).
board_name(white_grasshoper_3,['G','3',' ']).
board_name(white_spider_1,['S','1',' ']).
board_name(white_spider_2,['S','2',' ']).
board_name(white_ant_1,['A','1',' ']).
board_name(white_ant_2,['A','2',' ']).
board_name(white_ant_3,['A','3',' ']).

board_name(white_ladybug,[' ','L',' ']).
board_name(white_mosquito,[' ','M',' ']).
board_name(white_pillbug,[' ','P',' ']).

board_name(black_bee,[' ','b',' ']).
board_name(black_beetle_1,['b','t','1']).
board_name(black_beetle_2,['b','t','2']).
board_name(black_grasshoper_1,['g','1',' ']).
board_name(black_grasshoper_2,['g','2',' ']).
board_name(black_grasshoper_3,['g','3',' ']).
board_name(black_spider_1,['s','1',' ']).
board_name(black_spider_2,['s','2',' ']).
board_name(black_ant_1,['a','1',' ']).
board_name(black_ant_2,['a','2',' ']).
board_name(black_ant_3,['a','3',' ']).

board_name(black_ladybug,[' ','l',' ']).
board_name(black_mosquito,[' ','m',' ']).
board_name(black_pillbug,[' ','p',' ']).


% Colors
color(white_bee, white).
color(white_beetle_1, white).
color(white_beetle_2, white).
color(white_grasshoper_1, white).
color(white_grasshoper_2, white).
color(white_grasshoper_3, white).
color(white_spider_1, white).
color(white_spider_2, white).
color(white_ant_1, white).
color(white_ant_2, white).
color(white_ant_3, white).

color(white_ladybug, white).
color(white_mosquito, white).
color(white_pillbug, white).

color(black_bee, black).
color(black_beetle_1, black).
color(black_beetle_2, black).
color(black_grasshoper_1, black).
color(black_grasshoper_2, black).
color(black_grasshoper_3, black).
color(black_spider_1, black).
color(black_spider_2, black).
color(black_ant_1, black).
color(black_ant_2, black).
color(black_ant_3, black).

color(black_ladybug, black).
color(black_mosquito, black).
color(black_pillbug, black).


% Types
type(white_bee, bee).
type(white_beetle_1, beetle).
type(white_beetle_2, beetle).
type(white_grasshoper_1, grasshoper).
type(white_grasshoper_2, grasshoper).
type(white_grasshoper_3,grasshoper).
type(white_spider_1, spider).
type(white_spider_2, spider).
type(white_ant_1, ant).
type(white_ant_2, ant).
type(white_ant_3, ant).

type(white_ladybug, ladybug).
type(white_mosquito, mosquito).
type(white_pillbug, pillbug).

type(black_bee, bee).
type(black_beetle_1, beetle).
type(black_beetle_2, beetle).
type(black_grasshoper_1, grasshoper).
type(black_grasshoper_2, grasshoper).
type(black_grasshoper_3, grasshoper).
type(black_spider_1, spider).
type(black_spider_2, spider).
type(black_ant_1, ant).
type(black_ant_2, ant).
type(black_ant_3, ant).

type(black_ladybug, ladybug).
type(black_mosquito, mosquito).
type(black_pillbug, pillbug).