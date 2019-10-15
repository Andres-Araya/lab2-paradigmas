%set_prolog_flag(answer_write_options,[max_depth(0)])
%definir los limites del mapa
largo(10).
largo(12).
largo(20).
alto(5).
alto(10).
alto(20).

%mapa([X],[]):- alto(X).
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

	
esVida(VIDA):- VIDA > 0.
%esPosx(POS):- POS => 0.	

tiene(personaje, vida).
tiene(personaje, posicionX).
tiene(personaje, etiqueta).
tiene(personaje, bando).



%is_character([]).
%is_character([X|Xs]):- tiene().
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
escena_lil_2_3(SCENE):- SCENE = [[10,5], [[0,player, 100, 1], [1,player,100, 2]], [[10, player, 100, 1], [9, player, 100, 2]], 30,  playing].
escena_lil_2_2(SCENE):- SCENE = [[10,5], [[0,player, 100, 1], [1,player,100, 2]], [[10, player, 75, 1], [9, player, 75, 2]], 30,  playing].
escena_lil_2_1(SCENE):- SCENE = [[10,5], [[0,player, 100, 1], [1,player,100, 2]], [[10, player, 50, 1], [9, player, 50, 2]], 30,  playing].

escena_lil_4_3(SCENE):- SCENE = [[10,5], [[0,player, 100, 1], [1,player,100, 2], [2,player,100, 3], [3,player,100, 4]], [[10, cpu, 100, 1], [9, cpu, 100, 2],[8,cpu,100, 3],[7,cpu,100, 4]], 30,  playing].
escena_lil_4_2(SCENE):- SCENE = [[10,5], [[0,player, 100, 1], [1,player,100, 2], [2,player,100, 3], [3,player,100, 4]], [[10, cpu, 75, 1], [9, cpu, 75, 2],[8,cpu,75, 3],[7,cpu,75, 4]], 30,  playing].
escena_lil_4_1(SCENE):- SCENE = [[10,5], [[0,player, 100, 1], [1,player,100, 2], [2,player,100, 3], [3,player,100, 4]], [[10, cpu, 50, 1], [9, cpu, 50, 2],[8,cpu,50, 3],[7,cpu,50, 4]], 30,  playing].

escena_lil_5_3(SCENE):- SCENE = [[10,5], [[0,player, 100, 1], [1,player,100, 2], [2,player,100, 3], [3,player,100, 4], [4,player,100, 5]], [[10, cpu, 100, 1], [9, cpu, 100, 2],[8,cpu,100, 3],[7,cpu,100, 4], [6,cpu,100, 5]], 30,  playing].
escena_lil_5_2(SCENE):- SCENE = [[10,5], [[0,player, 100, 1], [1,player,100, 2], [2,player,100, 3], [3,player,100, 4], [4,player,100, 5]], [[10, cpu, 75, 1], [9, cpu, 75, 2],[8,cpu,75, 3],[7,cpu,75, 4], [6,cpu,75, 5]], 30,  playing].
escena_lil_5_1(SCENE):- SCENE = [[10,5], [[0,player, 100, 1], [1,player,100, 2], [2,player,100, 3], [3,player,100, 4], [4,player,100, 5]], [[10, cpu, 50, 1], [9, cpu, 50, 2],[8,cpu,50, 3],[7,cpu,50, 4], [6,cpu,50, 5]], 30,  playing].

escena_medio_4_3(SCENE):- SCENE = [[10,12], [[0,player, 100, 1], [1,player,100, 2], [2,player,100, 3], [3,player,100, 4]], [[12, cpu, 100, 1], [11, cpu, 100, 2],[10,cpu,100, 3],[9,cpu,100, 4]], 30,  playing].
escena_medio_4_2(SCENE):- SCENE = [[10,12], [[0,player, 100, 1], [1,player,100, 2], [2,player,100, 3], [3,player,100, 4]], [[12, cpu, 75, 1], [11, cpu, 75, 2],[10,cpu,75, 3],[9,cpu,75, 4]], 30,  playing].
escena_medio_4_1(SCENE):- SCENE = [[10,12], [[0,player, 100, 1], [1,player,100, 2], [2,player,100, 3], [3,player,100, 4]], [[12, cpu, 50, 1], [11, cpu, 50, 2],[10,cpu,50, 3],[9,cpu,50, 4]], 30,  playing].

escena_medio_6_3(SCENE):- SCENE = [[10,12], [[0,player, 100, 1], [1,player,100, 2], [2,player,100, 3], [3,player,100, 4], [4,player,100, 5], [5,player,100, 6]], [[12, cpu, 100, 1], [11, cpu, 100, 2],[10,cpu,100, 3], [9,cpu,100, 4], [8,cpu,100, 5], [7,cpu,100, 6]], 30,  playing].
escena_medio_6_2(SCENE):- SCENE = [[10,12], [[0,player, 100, 1], [1,player,100, 2], [2,player,100, 3], [3,player,100, 4], [4,player,100, 5], [5,player,100, 6]], [[12, cpu, 75, 1], [11, cpu, 75, 2],[10,cpu,75, 3], [9,cpu,75, 4], [8,cpu,75, 5], [7,cpu,75, 6]], 30,  playing].
escena_medio_6_1(SCENE):- SCENE = [[10,12], [[0,player, 100, 1], [1,player,100, 2], [2,player,100, 3], [3,player,100, 4], [4,player,100, 5], [5,player,100, 6]], [[12, cpu, 50, 1], [11, cpu, 50, 2],[10,cpu,50, 3], [9,cpu,50, 4], [8,cpu,50, 5], [7,cpu,50, 6]], 30,  playing].

escena_big_8_3(SCENE):- SCENE = [[20,20], [[0,player, 100, 1], [1,player,100, 2], [2,player,100, 3], [3,player,100, 4], [4,player,100, 5], [5,player,100, 6], [6,player,100, 7], [7,player,100, 8]], [[20, cpu, 100, 1], [19, cpu, 100, 2], [18,cpu,100, 3], [17,cpu,100, 4], [16,cpu,100, 5], [15,cpu,100, 6], [14,cpu,100, 7], [13,cpu,100, 8]], 30,  playing].
escena_big_8_2(SCENE):- SCENE = [[20,20], [[0,player, 100, 1], [1,player,100, 2], [2,player,100, 3], [3,player,100, 4], [4,player,100, 5], [5,player,100, 6], [6,player,100, 7], [7,player,100, 8]], [[20, cpu, 75, 1], [19, cpu, 75, 2], [18,cpu,75, 3], [17,cpu,75, 4], [16,cpu,75, 5], [15,cpu,75, 6], [14,cpu,75, 7], [13,cpu,75, 8]], 30,  playing].
escena_big_8_1(SCENE):- SCENE = [[20,20], [[0,player, 100, 1], [1,player,100, 2], [2,player,100, 3], [3,player,100, 4], [4,player,100, 5], [5,player,100, 6], [6,player,100, 7], [7,player,100, 8]], [[20, cpu, 50, 1], [19, cpu, 50, 2], [18,cpu,50, 3], [17,cpu,50, 4], [16,cpu,50, 5], [15,cpu,50, 6], [14,cpu,50, 7], [13,cpu,50, 8]], 30,  playing].

selectScene(N,M,E,D,SCENE):- 	((N=10,M=5,E=2,D=1),(escena_lil_2_1(SCENE)));
								((N=10,M=5,E=2,D=2),(escena_lil_2_2(SCENE)));
								((N=10,M=5,E=2,D=3),(escena_lil_2_3(SCENE)));
								((N=10,M=5,E=4,D=1),(escena_lil_4_1(SCENE)));
								((N=10,M=5,E=4,D=2),(escena_lil_4_2(SCENE)));
								((N=10,M=5,E=4,D=3),(escena_lil_4_3(SCENE)));
								((N=10,M=5,E=5,D=1),(escena_lil_5_1(SCENE)));
								((N=10,M=5,E=5,D=2),(escena_lil_5_2(SCENE)));
								((N=10,M=5,E=5,D=3),(escena_lil_5_3(SCENE)));
								((N=12,M=10,E=4,D=1),(escena_medio_4_1(SCENE)));
								((N=12,M=10,E=4,D=2),(escena_medio_4_2(SCENE)));
								((N=12,M=10,E=4,D=3),(escena_medio_4_3(SCENE)));
								((N=12,M=10,E=6,D=1),(escena_medio_6_1(SCENE)));
								((N=12,M=10,E=6,D=2),(escena_medio_6_2(SCENE)));
								((N=12,M=10,E=6,D=3),(escena_medio_6_3(SCENE)));
								((N=20,M=20,E=8,D=1),(escena_big_8_1(SCENE)));
								((N=20,M=20,E=8,D=2),(escena_big_8_2(SCENE)));
								((N=20,M=20,E=8,D=3),(escena_big_8_3(SCENE))).


createScene(N,M,E,D,SCENE):- largo(N), alto(M), enemigos(E), dificultad(D), selectScene(N,M,E,D,SCENE).


%CheckScene

etiquetaPlayer(player).
etiquetaCPU(cpu).
%esEtiqueta([ETIQUETA]):- etiqueta(ETIQUETA) .

posValida(POS,X):- POS >= 0  ; POS =< X .

esID(ID,X):- (X = 20, ID =< 8); (X=12, (ID =< 4; ID =< 6)); (X = 10, (ID =< 5; ID =< 4; ID =< 2 )) , ID > 0.
salud(VIDA):- VIDA >= 0, VIDA =< 100.
esVida([VIDA| ID],X):- salud(VIDA), esID(ID,X).
%esEtiqueta(ETIQUETA):- et .
esPlayer([ETIQUETA|VIDA],X):- etiquetaPlayer(ETIQUETA), esVida(VIDA,X).
esCPU([ETIQUETA|VIDA],X):- etiquetaCPU(ETIQUETA), esVida(VIDA,X).

es_Character_Cpu([POS|ETIQUETA],[],  X):- posValida(POS,X), esCPU(ETIQUETA,X).
es_Character_Cpu([POS|ETIQUETA],[CPU|CPUs],X):- posValida(POS,X), esCPU(ETIQUETA,X), es_Character_Cpu(CPU,CPUs,X) .
esCharacter([POS|ETIQUETA],[],  X):- posValida(POS,X), esPlayer(ETIQUETA,X).
esCharacter([POS|ETIQUETA],[PLAYER|PLAYERs],X):- posValida(POS,X), esPlayer(ETIQUETA,X), esCharacter(PLAYER,PLAYERs,X) .

checkScene([X,Y], [PLAYER|PLAYERs], [CPU|CPUs], TIEMPO, ESTADO):- 
			largo(X), alto(Y), esCharacter(PLAYER,PLAYERs,X), es_Character_Cpu(CPU,CPUs, X), (TIEMPO >= 0; TIEMPO =< 30), es_estado(ESTADO), !.
%checkScene([X,Y], [PLAYER|PLAYERs], [CPU|CPUs], TIEMPO, ESTADO):- 
%			largo(X), alto(Y), esCharacter(PLAYER,PLAYERs,X), es_Character_Cpu(CPU,CPUs, X), (TIEMPO >= 0; TIEMPO =< 30), es_estado(ESTADO).

%MoverMember

invertirLista([PJ|PJs], INVERT):- reverse([PJ|PJs], INVERT) .


outMap([POS,_,_,_], LARGO_MAPA):- POS < 0; POS > LARGO_MAPA .

esPJ([POS,TIPO,VIDA,ID], MEMBER,MOVE, CHAR):- ID=MEMBER , CHAR = [NEWPOS,TIPO,VIDA,ID], NEWPOS is POS + MOVE.
revisar([], PERSONAJES,PERSONAJES).
revisar([PJ|PJs], MEMBER, MOVE,NL, PERSONAJES, LARGO_MAPA):-
(esPJ(PJ, MEMBER, MOVE, CHAR), ((outMap(CHAR, LARGO_MAPA), (revisar(PJs, NL, PERSONAJES);revisar(PJs, MEMBER,MOVE, NL, PERSONAJES,LARGO_MAPA)));
(revisar(PJs, [CHAR|NL], PERSONAJES); revisar(PJs, MEMBER, MOVE, [CHAR|NL], PERSONAJES,LARGO_MAPA)))) ,!;
(revisar(PJs, [PJ|NL], PERSONAJES); revisar(PJs, MEMBER, MOVE, [PJ|NL], PERSONAJES,LARGO_MAPA))
.

compararPos([_,_,_,_], []).
compararPos([POS1,_,_,_], [POS2,_,_,_]):- not( POS1=POS2 ) .
compararPos([POS,_,_,_], [[POS2, _,_,_]|PJs]):- not(POS = POS2), compararPos([POS, _,_,_],PJs),! .

compararPosIDPj([_,_,_,_], []).
compararPosIDPj([POS,_,_,ID], [[POS2, _,_,ID2]|PJs]):- not(POS = POS2), not(ID = ID2), compararPosIDPj([POS, _,_,ID],PJs),! .

		
mover(PERSONAJES, [PJ|PJs], MEMBER, MOVE,LARGO_MAPA):- revisar([PJ|PJs], MEMBER, MOVE,[], PERSONAJES,LARGO_MAPA) .
moveMember([[X,Y], [PJ|PJs], [CPU|CPUs], TIME,ESTATE], Member, MoveDir, SceneOut):- ESTATE = playing,
checkScene([X,Y], [PJ|PJs], [CPU|CPUs], TIME,ESTATE), mover(PERSONAJES, [PJ|PJs], Member, MoveDir,X), invertirLista(PERSONAJES, NEWPERSONAJES), SceneOut = [[X,Y], NEWPERSONAJES, [CPU|CPUs], NEWTIME,ESTATE] , NEWTIME is TIME - 1 .

%Shoot

convertir(Angle, ALPHA):- ALPHA is (Angle *  pi)/ 180.
toInt(X, NEWX):- round(X, NEWX).

tiempoFinal(ALPHA, T):- T is (8 * sin(ALPHA)/ 5).
xdeT(XI, ALPHA, T, X):- X is XI + (8 * cos(ALPHA) * T).
seno(A,X):- X is sin(A) .
coseno(A,X):- X is cos(A) .

disparar(POSINICIAL, ALPHA, IMPACTO):- tiempoFinal(ALPHA, T), xdeT(POSINICIAL, ALPHA, T, X), round(X,IMPACTO) .

obtenerMember([_,_,_,ID], Member):- ID = Member.
obtenerPos([POS,_,_,_], POS).

damage([POS,TIPO,VIDA,ID], IMPACTO, CHAR):- POS = IMPACTO, CHAR=[POS, TIPO, NEWVIDA, ID], NEWVIDA is (VIDA - 50).
vidaCero([_,_,VIDA,_]):- VIDA =< 0 .

impactoPj([], PERSONAJES, PERSONAJES).
impactoPj([PLAYER|PLAYERs], IMPACTO, LI, PERSONAJES):- 
(damage(PLAYER,IMPACTO,CHARACTER), ((vidaCero(CHARACTER), impactoPj(PLAYERs, LI, PERSONAJES);impactoPj(PLAYERs, IMPACTO, LI, PERSONAJES));
(impactoPj(PLAYERs, [CHARACTER|LI], PERSONAJES); impactoPj(PLAYERs, IMPACTO, [CHARACTER|LI], PERSONAJES)))),!;
(impactoPj(PLAYERs, [PLAYER|LI], PERSONAJES); impactoPj(PLAYERs, IMPACTO, [PLAYER|LI], PERSONAJES))
.

/*impactoCpu([], PERSONAJES, PERSONAJES).
impactoCpu([CPU|CPUs], IMPACTO, LI, PERSONAJES):- 
(damage(CPU,IMPACTO,CHARACTER), (impactoPj(CPUs, [CHARACTER|LI], PERSONAJES); impactoPj(CPUs, IMPACTO, [CHARACTER|LI], PERSONAJES)));
(impactoPj(CPUs, [CPU|LI], PERSONAJES); impactoPj(CPUs, IMPACTO, [CPU|LI], PERSONAJES))
.*/

disparoCPU([[POS, _,_,ID]], Member, Angle, IMPACTO):- (Member = ID, convertir(Angle, ALPHA),disparar(POS, ALPHA, IMPACTO)).
disparoCPU([CPU|CPUs], Member, Angle, IMPACTO ):- 
(obtenerMember(CPU, Member), obtenerPos(CPU, POS), disparar(POS, Angle, IMPACTO)); 
(disparoCPU(CPUs, Member, Angle, IMPACTO)) .
disparoPlayer([[POS, _,_,ID]], Member, Angle, IMPACTO):- (Member = ID, convertir(Angle, ALPHA),disparar(POS, ALPHA, IMPACTO)).
disparoPlayer([PLAYER|PLAYERs], Member, Angle, IMPACTO):- 
(obtenerMember(PLAYER, Member), obtenerPos(PLAYER, POS), disparar(POS, Angle, IMPACTO)); 
(disparoPlayer(PLAYERs, Member, Angle, IMPACTO)) .

shoot([[X,Y], [PJ|PJs], [CPU|CPUs], TIME,ESTATE], [TIPO,Member], ShotType, Angle, _, SceneOut):- ESTATE = playing,
%checkScene([[X,Y], [PJ|PJs], [CPU|CPUs], TIEMPO,ESTATE]), 
(ShotType = parabolico, (TIPO = player, disparoPlayer([PJ|PJs], Member, Angle, IMPACTO)); (TIPO = cpu, disparoCPU([CPU|CPUs], Member, Angle, IMPACTO))),
(impactoPj([PJ|PJs], IMPACTO, [], PERSONAJES), impactoPj([CPU|CPUs], IMPACTO, [], COMP)), (invertirLista(COMP, COMP2), invertirLista(PERSONAJES,PERSONAJES2)),
SceneOut = [[X,Y], PERSONAJES2, COMP2, NEWTIME, ESTATE], NEWTIME is TIME - 1,!
.