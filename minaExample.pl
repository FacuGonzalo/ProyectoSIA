/*

  Representacion grafica de una mina irregular con 14 filas y 13 columnas:

     1   2   3   4
   _________________
1  | ~ | P | ~ | ~ |
   |___|___|___|___|
2  | ~ | ~ |   | ! |                    10  11  12  13
   |___|___|___|___|                   ________________
3  |   |   | P | ~ |                   | ~ | ~ | ~ | ~ | 3
   |___|___|___|___|                   |___|___|___|___|
4  | ~ |   | ~ | P | 5   6   7   8   9 |   |   |   |   | 4
   |___|___|___|___|___________________|___|___|___|___|
5  |   | R2| ~ | P |   | ~ | ! | L2| ~ |   | ! | P | ~ | 5
   |___|___|___|___|___|___|___|___|___|___|___|___|___|
6  |   | D | V1| ~ | P | C |   |   | ~ |   | V5|   |   | 6
   |___|___|___|___|___|___|___|___|___|___|___|___|___|
7  |   | P | V2|   | P | ~ | V2| ~ | ~ | R3|   |   | ~ | 7
   |___|___|___|___|___|___|___|___|___|___|___|___|___|
8  | ~ |   |   |   | ~ | ~ |   | P | P | P | ~ | V4|   | 8
   |___|___|___|___|___|___|___|___|___|___|___|___|___|
9  | ~ | V5|   | ~ |   | L1| ~ |   | ~ |   | ~ | V7|   | 9
   |___|___|___|___|___|___|___|___|___|___|___|___|___|
10 |   |   |   |   | ~ | # |   | ~ |
   |___|___|___|___|___|___|___|___|
11 | ~ |   |   | V3|   | ~ |   | V1|
   |___|___|___|___|___|___|___|___|____________
12 | ~ | ! | ~ | P |   | V3|   | P | L3|   | ~ | 12
   |___|___|___|___|___|___|___|___|___|___|___|
13 |   |   | ~ | P |   |   |   | R1| ! | P |   | 13
   |___|___|___|___|___|___|___|___|___|___|___|
14 |   |   | ~ |   | ~ |   |   |   | ~ | P |   | 14
   |___|___|___|___|___|___|___|___|___|___|___|

     1   2   3   4   5   6   7   8   9  10  11


-----------------------------------------------
Referencias de Suelo:

 ____
 |   | : Celda con suelo firme
 |___|

 ____
 | ~ | : Celda con suelo resbaladizo
 |___|

 ____
 | # | : Sitio de colocacion de carga explosiva
 |___|

 ____
 | ! | : Posicion para la detonacion
 |___|


-----------------------------------------------
Referencias de Objetos:

 Ri: Reja i

 Li: Llave i

 C: Carga Explosiva

 D: Detonador

 P: Pilar

 Vi: Valla con altura i

-----------------------------------------------
IMPORTANTE: Para aquellas celdas que albergan objetos (reja, llave, carga, detonador, pilar o valla)
la grilla dibujada no ilustra el tipo de suelo de la celda (debe observarse en la coleccion de hechos
definida a continuacion).

*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    Configuracion de la mina ilustrada

    Coleccion de Hechos celda/2, estaEn/2, ubicacionCarga/2, sitioDetonacion/1 y abreReja/2:
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Celdas de la mina:

celda([1,1], resbaladizo).
celda([1,2], firme).
celda([1,3], resbaladizo).
celda([1,4], resbaladizo).

celda([2,1], resbaladizo).
celda([2,2], resbaladizo).
celda([2,3], firme).
celda([2,4], resbaladizo).

celda([3,1], firme).
celda([3,2], firme).
celda([3,3], resbaladizo).
celda([3,4], resbaladizo).
celda([3,10], resbaladizo).
celda([3,11], resbaladizo).
celda([3,12], resbaladizo).
celda([3,13], resbaladizo).

celda([4,1], resbaladizo).
celda([4,2], firme).
celda([4,3], resbaladizo).
celda([4,4], firme).
celda([4,10], firme).
celda([4,11], firme).
celda([4,12], firme).
celda([4,13], firme).

celda([5,1], firme).
celda([5,2], firme).
celda([5,3], resbaladizo).
celda([5,4], firme).
celda([5,5], firme).
celda([5,6], resbaladizo).
celda([5,7], resbaladizo).
celda([5,8], firme).
celda([5,9], resbaladizo).
celda([5,10], firme).
celda([5,11], firme).
celda([5,12], firme).
celda([5,13], resbaladizo).

celda([6,1], firme).
celda([6,2], firme).
celda([6,3], firme).
celda([6,4], resbaladizo).
celda([6,5], firme).
celda([6,6], firme).
celda([6,7], firme).
celda([6,8], firme).
celda([6,9], resbaladizo).
celda([6,10], firme).
celda([6,11], resbaladizo).
celda([6,12], firme).
celda([6,13], firme).

celda([7,1], firme).
celda([7,2], firme).
celda([7,3], resbaladizo).
celda([7,4], firme).
celda([7,5], resbaladizo).
celda([7,6], resbaladizo).
celda([7,7], firme).
celda([7,8], resbaladizo).
celda([7,9], resbaladizo).
celda([7,10], resbaladizo).
celda([7,11], firme).
celda([7,12], firme).
celda([7,13], resbaladizo).

celda([8,1], resbaladizo).
celda([8,2], firme).
celda([8,3], firme).
celda([8,4], firme).
celda([8,5], resbaladizo).
celda([8,6], resbaladizo).
celda([8,7], firme).
celda([8,8], firme).
celda([8,9], firme).
celda([8,10], firme).
celda([8,11], resbaladizo).
celda([8,12], firme).
celda([8,13], firme).

celda([9,1], resbaladizo).
celda([9,2], resbaladizo).
celda([9,3], firme).
celda([9,4], resbaladizo).
celda([9,5], firme).
celda([9,6], firme).
celda([9,7], resbaladizo).
celda([9,8], firme).
celda([9,9], resbaladizo).
celda([9,10], firme).
celda([9,11], resbaladizo).
celda([9,12], firme).
celda([9,13], firme).

celda([10,1], firme).
celda([10,2], firme).
celda([10,3], firme).
celda([10,4], firme).
celda([10,5], resbaladizo).
celda([10,6], firme).
celda([10,7], firme).
celda([10,8], resbaladizo).

celda([11,1], resbaladizo).
celda([11,2], firme).
celda([11,3], firme).
celda([11,4], resbaladizo).
celda([11,5], firme).
celda([11,6], resbaladizo).
celda([11,7], firme).
celda([11,8], firme).

celda([12,1], resbaladizo).
celda([12,2], resbaladizo).
celda([12,3], resbaladizo).
celda([12,4], firme).
celda([12,5], firme).
celda([12,6], firme).
celda([12,7], firme).
celda([12,8], firme).
celda([12,9], firme).
celda([12,10], firme).
celda([12,11], resbaladizo).

celda([13,1], firme).
celda([13,2], firme).
celda([13,3], resbaladizo).
celda([13,4], firme).
celda([13,5], firme).
celda([13,6], firme).
celda([13,7], firme).
celda([13,8], firme).
celda([13,9], resbaladizo).
celda([13,10], resbaladizo).
celda([13,11], firme).

celda([14,1], firme).
celda([14,2], firme).
celda([14,3], resbaladizo).
celda([14,4], firme).
celda([14,5], resbaladizo).
celda([14,6], firme).
celda([14,7], firme).
celda([14,8], firme).
celda([14,9], resbaladizo).
celda([14,10], resbaladizo).
celda([14,11], firme).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Objetos en la mina:

% Rejas:
estaEn([r, r1], [13,8]).
estaEn([r, r2], [5,2]).
estaEn([r, r3], [7,10]).

% Llaves:
estaEn([l, l1], [9,6]).
estaEn([l, l2], [5,8]).
estaEn([l, l3], [12,9]).

% Carga:
estaEn([c, c1], [6,6]).

% Detonador:
estaEn([d, d1, no], [6,2]).

% Pilares:
estaEn([p, p1, 3], [1,2]).
estaEn([p, p2, 5], [3,3]).
estaEn([p, p3, 7], [4,4]).
estaEn([p, p4, 6], [5,4]).
estaEn([p, p5, 9], [5,12]).
estaEn([p, p6, 8], [6,5]).
estaEn([p, p7, 2], [7,2]).
estaEn([p, p8, 4], [7,5]).
estaEn([p, p9, 5], [8,8]).
estaEn([p, p10, 6], [8,9]).
estaEn([p, p11, 8], [8,10]).
estaEn([p, p12, 2], [12,4]).
estaEn([p, p13, 5], [12,8]).
estaEn([p, p14, 13], [13,4]).
estaEn([p, p15, 8], [13,10]).
estaEn([p, p16, 4], [14,10]).

% Vallas:
estaEn([v, v1, 1], [6,3]).
estaEn([v, v2, 5], [6,11]).
estaEn([v, v3, 2], [7,3]).
estaEn([v, v4, 2], [7,7]).
estaEn([v, v5, 4], [8,12]).
estaEn([v, v6, 5], [9,2]).
estaEn([v, v7, 7], [9,12]).
estaEn([v, v8, 3], [11,4]).
estaEn([v, v9, 1], [11,8]).
estaEn([v, v10, 3], [12,6]).


% Sitio donde debe ser ubicada la Carga
ubicacionCarga([10,6]).

% Sitios habilitados para efectuar la detonacion
sitioDetonacion([2,4]).
sitioDetonacion([5,7]).
sitioDetonacion([5,11]).
sitioDetonacion([12,2]).
sitioDetonacion([13,9]).

% Indicador de que llave abre que reja
abreReja([l, l1], [r, r2]).
abreReja([l, l2], [r, r3]).
abreReja([l, l3], [r, r1]).