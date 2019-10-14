%definir los limites del mapa
largo(10).
largo(12).
largo(20).
alto(5).
alto(10).
alto(20).
mapa(X,Y):- largo(X), alto(Y).

%tipos de dificultad

easy(1).
medium(2).
hard(3).
dificultad(X):- easy(X); medium(X); hard(X).

%vida(X):- X=>0, X=<100.
%crearpersonajes
agrega([],[ELEM], ELEM).
agrega([X|Xs],[X|Ys], ELEM):- agrega(Xs,Ys,ELEM).


tiene(personaje, vida).
tiene(personaje, posicionX).
tiene(personaje, etiqueta).
tiene(personaje, bando).

esPersonaje(VIDA, POS, ETIQUETA, BANDO):- tiene(personaje, VIDA), tiene(personaje, POS), tiene(personaje, ETIQUETA), tiene(personaje, BANDO).

es_estado(playing).
es_estado(draw).
es_estado(victory).
es_estado(defeat).

enemigos(2).
enemigos(4).
enemigos(5).
enemigos(6).
enemigos(8).

%ejemplo([1,2,3,4,5,6,8,apa,7,54,7,8,5,47,78,4,4,7,4,1,16,8,7,6,7,uwu,7687,68,76,7,657,6,575,64,987,9891879,81797,197,87,97,87,87,178]).

escena1([[5,10], [[0,0, 100, 1], [1,0,100, 2]], [[10, 0, 100, 1], [9, 0, 100, 2]], 30,  playing]).
escena2([[5,10], [[0,0, 100, 1], [1,0,100, 2], [2,0,100, 3], [3,0,100, 4]], 
	   [[10, 0, 100, 1], [9, 0, 100, 2],[8,0,100, 3],[7,0,100, 4]], 30,  playing]).
escena3([[5,10], [[0,0, 100, 1], [1,0,100, 2], [2,0,100, 3], [3,0,100, 4], [4,0,100, 5]], 
	   [[10, 0, 100, 1], [9, 0, 100, 2],[8,0,100, 3],[7,0,100, 4], [6,0,100, 5]], 30,  playing]).
escena4([[10,12], [[0,0, 100, 1], [1,0,100, 2], [2,0,100, 3], [3,0,100, 4]], 
	   [[12, 0, 100, 1], [11, 0, 100, 2],[10,0,100, 3],[9,0,100, 4]], 30,  playing]).
escena5([[10,12], [[0,0, 100, 1], [1,0,100, 2], [2,0,100, 3], [3,0,100, 4], [4,0,100, 5], [5,0,100, 6]], 
	   [[12, 0, 100, 1], [11, 0, 100, 2],[10,0,100, 3], [9,0,100, 4], [8,0,100, 5], [7,0,100, 6]], 30,  playing]).
escena6([[20,20], [[0,0, 100, 1], [1,0,100, 2], [2,0,100, 3], [3,0,100, 4], [4,0,100, 5], [5,0,100, 6], [6,0,100, 7], [7,0,100, 8]], 
	   [[20, 0, 100, 1], [19, 0, 100, 2], [18,0,100, 3], [17,0,100, 4], [16,0,100, 5], [15,0,100, 6], [14,0,100, 7], [13,0,100, 8]], 30,  playing]).

createScene(N,M,E,D,SCENE):- largo(N), alto(M), enemigos(E), dificultad(D), E= 2, 
							 SCENE = [[5,10], [[0,0, 100, 1], [1,0,100, 2]], [[10, 0, 100, 1], [9, 0, 100, 2]], 30,  playing].
createScene(N,M,E,D,SCENE):- largo(N), alto(M), enemigos(E), dificultad(D), E= 4, N = 10,
							 SCENE = [[5,10], [[0,0, 100, 1], [1,0,100, 2], [2,0,100, 3], [3,0,100, 4]], 
	  						 [[10, 0, 100, 1], [9, 0, 100, 2],[8,0,100, 3],[7,0,100, 4]], 30,  playing].
createScene(N,M,E,D,SCENE):- largo(N), alto(M), enemigos(E), dificultad(D), E= 5,
							 SCENE = [[5,10], [[0,0, 100, 1], [1,0,100, 2], [2,0,100, 3], [3,0,100, 4], [4,0,100, 5]], 
	   						 [[10, 0, 100, 1], [9, 0, 100, 2],[8,0,100, 3],[7,0,100, 4], [6,0,100, 5]], 30,  playing].
createScene(N,M,E,D,SCENE):- largo(N), alto(M), enemigos(E), dificultad(D), E= 4, N=12, 
							 SCENE = [[10,12], [[0,0, 100, 1], [1,0,100, 2], [2,0,100, 3], [3,0,100, 4]], 
						     [[12, 0, 100, 1], [11, 0, 100, 2],[10,0,100, 3],[9,0,100, 4]], 30,  playing].
createScene(N,M,E,D,SCENE):- largo(N), alto(M), enemigos(E), dificultad(D), E= 6,
							 SCENE = [[10,12], [[0,0, 100, 1], [1,0,100, 2], [2,0,100, 3], [3,0,100, 4], [4,0,100, 5], [5,0,100, 6]], 
	  						 [[12, 0, 100, 1], [11, 0, 100, 2],[10,0,100, 3], [9,0,100, 4], [8,0,100, 5], [7,0,100, 6]], 30,  playing].
createScene(N,M,E,D,SCENE):- largo(N), alto(M), enemigos(E), dificultad(D), E= 8, 
							 SCENE = [[20,20], [[0,0, 100, 1], [1,0,100, 2], [2,0,100, 3], [3,0,100, 4], [4,0,100, 5], [5,0,100, 6], [6,0,100, 7], [7,0,100, 8]], 
	  						 [[20, 0, 100, 1], [19, 0, 100, 2], [18,0,100, 3], [17,0,100, 4], [16,0,100, 5], [15,0,100, 6], [14,0,100, 7], [13,0,100, 8]], 30,  playing].
%createScenee([X|xs]):- ancho(y,X).