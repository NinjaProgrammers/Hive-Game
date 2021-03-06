:- module(pieces, [piece/1, board_name/2, color/2, type/2]).

:- dynamic piece/1.
piece(red_bee).
%piece(red_ladybug).
%piece(red_mosquito).
piece(red_ant_3).
piece(red_grasshoper_3).
piece(red_ant_2).
piece(red_spider_2).
piece(red_beetle_2).
piece(red_grasshoper_2).
piece(red_spider_1).
piece(red_beetle_1).
piece(red_grasshoper_1).
piece(red_ant_1).
%piece(red_pillbug).

piece(blue_bee).
%piece(blue_ladybug).
%piece(blue_mosquito).
piece(blue_ant_3).
piece(blue_grasshoper_3).
piece(blue_beetle_2).
piece(blue_grasshoper_2).
piece(blue_spider_2).
piece(blue_ant_2).
piece(blue_grasshoper_1).
piece(blue_ant_1).
piece(blue_spider_1).
piece(blue_beetle_1).
%piece(blue_pillbug).


%Board Names

board_name(red_bee,[' ','B',' ']).
board_name(red_beetle_1,['B','T','1']).
board_name(red_beetle_2,['B','T','2']).
board_name(red_grasshoper_1,['G','1',' ']).
board_name(red_grasshoper_2,['G','2',' ']).
board_name(red_grasshoper_3,['G','3',' ']).
board_name(red_spider_1,['S','1',' ']).
board_name(red_spider_2,['S','2',' ']).
board_name(red_ant_1,['A','1',' ']).
board_name(red_ant_2,['A','2',' ']).
board_name(red_ant_3,['A','3',' ']).

board_name(red_ladybug,[' ','L',' ']).
board_name(red_mosquito,[' ','M',' ']).
board_name(red_pillbug,[' ','P',' ']).

board_name(blue_bee,[' ','b',' ']).
board_name(blue_beetle_1,['b','t','1']).
board_name(blue_beetle_2,['b','t','2']).
board_name(blue_grasshoper_1,['g','1',' ']).
board_name(blue_grasshoper_2,['g','2',' ']).
board_name(blue_grasshoper_3,['g','3',' ']).
board_name(blue_spider_1,['s','1',' ']).
board_name(blue_spider_2,['s','2',' ']).
board_name(blue_ant_1,['a','1',' ']).
board_name(blue_ant_2,['a','2',' ']).
board_name(blue_ant_3,['a','3',' ']).

board_name(blue_ladybug,[' ','l',' ']).
board_name(blue_mosquito,[' ','m',' ']).
board_name(blue_pillbug,[' ','p',' ']).


% Colors
color(red_bee, red).
color(red_beetle_1, red).
color(red_beetle_2, red).
color(red_grasshoper_1, red).
color(red_grasshoper_2, red).
color(red_grasshoper_3, red).
color(red_spider_1, red).
color(red_spider_2, red).
color(red_ant_1, red).
color(red_ant_2, red).
color(red_ant_3, red).

color(red_ladybug, red).
color(red_mosquito, red).
color(red_pillbug, red).

color(blue_bee, blue).
color(blue_beetle_1, blue).
color(blue_beetle_2, blue).
color(blue_grasshoper_1, blue).
color(blue_grasshoper_2, blue).
color(blue_grasshoper_3, blue).
color(blue_spider_1, blue).
color(blue_spider_2, blue).
color(blue_ant_1, blue).
color(blue_ant_2, blue).
color(blue_ant_3, blue).

color(blue_ladybug, blue).
color(blue_mosquito, blue).
color(blue_pillbug, blue).


% Types
type(red_bee, bee).
type(red_beetle_1, beetle).
type(red_beetle_2, beetle).
type(red_grasshoper_1, grasshoper).
type(red_grasshoper_2, grasshoper).
type(red_grasshoper_3,grasshoper).
type(red_spider_1, spider).
type(red_spider_2, spider).
type(red_ant_1, ant).
type(red_ant_2, ant).
type(red_ant_3, ant).

type(red_ladybug, ladybug).
type(red_mosquito, mosquito).
type(red_pillbug, pillbug).

type(blue_bee, bee).
type(blue_beetle_1, beetle).
type(blue_beetle_2, beetle).
type(blue_grasshoper_1, grasshoper).
type(blue_grasshoper_2, grasshoper).
type(blue_grasshoper_3, grasshoper).
type(blue_spider_1, spider).
type(blue_spider_2, spider).
type(blue_ant_1, ant).
type(blue_ant_2, ant).
type(blue_ant_3, ant).

type(blue_ladybug, ladybug).
type(blue_mosquito, mosquito).
type(blue_pillbug, pillbug).
