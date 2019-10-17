%set_prolog_flag(answer_write_options,[max_depth(0)])
%Hechos
%definir los limites del mapa de alto y largo
largo(10).
largo(12).
largo(20).
alto(5).
alto(10).
alto(20).

%posibles estados que puede tener una escena
es_estado(playing).
es_estado(draw).
es_estado(victory).
es_estado(defeat).

%posible cantidad de enemigos que pueden haber en una escena al crearla
enemigos(2).
enemigos(4).
enemigos(5).
enemigos(6).
enemigos(8).

%es mapa con largo y alto validos
mapa(X,Y):- largo(X), alto(Y).

%tipos de dificultad
easy(1).
medium(2).
hard(3).

%Reglas
%#############################################################################################################################
/*##################################################
################Comienza createScene################
####################################################*/

%es dificultad si es facil, medio o dificil
%Predicado: verifica si es una dificultad valida
%Entrada: integer
%Salida: bool
dificultad(X):- easy(X); medium(X); hard(X).	

%Se definen las escenas posibles a crear, con sus combinaciones de largo mapa vs cantidad de personajes.
%escena, lil = piqueno, medio = tamano mediano, big = tamano grande
%Predicado: se le asigna un valor a una variable SCENE
%Entrada:variable
%Salida: TDAScena
escena_lil_2_3(SCENE):- SCENE = [[10,5], [[0,player, 100, 1], [1,player,100, 2]], [[10, cpu, 100, 1], [9, cpu, 100, 2]], 120,  playing].
escena_lil_2_2(SCENE):- SCENE = [[10,5], [[0,player, 100, 1], [1,player,100, 2]], [[10, cpu, 75, 1], [9, cpu, 75, 2]], 120,  playing].
escena_lil_2_1(SCENE):- SCENE = [[10,5], [[0,player, 100, 1], [1,player,100, 2]], [[10, cpu, 50, 1], [9, cpu, 50, 2]], 120,  playing].

escena_lil_4_3(SCENE):- SCENE = [[10,5], [[0,player, 100, 1], [1,player,100, 2], [2,player,100, 3], [3,player,100, 4]], [[10, cpu, 100, 1], [9, cpu, 100, 2],[8,cpu,100, 3],[7,cpu,100, 4]], 120,  playing].
escena_lil_4_2(SCENE):- SCENE = [[10,5], [[0,player, 100, 1], [1,player,100, 2], [2,player,100, 3], [3,player,100, 4]], [[10, cpu, 75, 1], [9, cpu, 75, 2],[8,cpu,75, 3],[7,cpu,75, 4]], 120,  playing].
escena_lil_4_1(SCENE):- SCENE = [[10,5], [[0,player, 100, 1], [1,player,100, 2], [2,player,100, 3], [3,player,100, 4]], [[10, cpu, 50, 1], [9, cpu, 50, 2],[8,cpu,50, 3],[7,cpu,50, 4]], 120,  playing].

escena_lil_5_3(SCENE):- SCENE = [[10,5], [[0,player, 100, 1], [1,player,100, 2], [2,player,100, 3], [3,player,100, 4], [4,player,100, 5]], [[10, cpu, 100, 1], [9, cpu, 100, 2],[8,cpu,100, 3],[7,cpu,100, 4], [6,cpu,100, 5]], 120,  playing].
escena_lil_5_2(SCENE):- SCENE = [[10,5], [[0,player, 100, 1], [1,player,100, 2], [2,player,100, 3], [3,player,100, 4], [4,player,100, 5]], [[10, cpu, 75, 1], [9, cpu, 75, 2],[8,cpu,75, 3],[7,cpu,75, 4], [6,cpu,75, 5]], 120,  playing].
escena_lil_5_1(SCENE):- SCENE = [[10,5], [[0,player, 100, 1], [1,player,100, 2], [2,player,100, 3], [3,player,100, 4], [4,player,100, 5]], [[10, cpu, 50, 1], [9, cpu, 50, 2],[8,cpu,50, 3],[7,cpu,50, 4], [6,cpu,50, 5]], 120,  playing].

escena_medio_4_3(SCENE):- SCENE = [[10,12], [[0,player, 100, 1], [1,player,100, 2], [2,player,100, 3], [3,player,100, 4]], [[12, cpu, 100, 1], [11, cpu, 100, 2],[10,cpu,100, 3],[9,cpu,100, 4]], 120,  playing].
escena_medio_4_2(SCENE):- SCENE = [[10,12], [[0,player, 100, 1], [1,player,100, 2], [2,player,100, 3], [3,player,100, 4]], [[12, cpu, 75, 1], [11, cpu, 75, 2],[10,cpu,75, 3],[9,cpu,75, 4]], 120,  playing].
escena_medio_4_1(SCENE):- SCENE = [[10,12], [[0,player, 100, 1], [1,player,100, 2], [2,player,100, 3], [3,player,100, 4]], [[12, cpu, 50, 1], [11, cpu, 50, 2],[10,cpu,50, 3],[9,cpu,50, 4]], 120,  playing].

escena_medio_6_3(SCENE):- SCENE = [[10,12], [[0,player, 100, 1], [1,player,100, 2], [2,player,100, 3], [3,player,100, 4], [4,player,100, 5], [5,player,100, 6]], [[12, cpu, 100, 1], [11, cpu, 100, 2],[10,cpu,100, 3], [9,cpu,100, 4], [8,cpu,100, 5], [7,cpu,100, 6]], 120,  playing].
escena_medio_6_2(SCENE):- SCENE = [[10,12], [[0,player, 100, 1], [1,player,100, 2], [2,player,100, 3], [3,player,100, 4], [4,player,100, 5], [5,player,100, 6]], [[12, cpu, 75, 1], [11, cpu, 75, 2],[10,cpu,75, 3], [9,cpu,75, 4], [8,cpu,75, 5], [7,cpu,75, 6]], 120,  playing].
escena_medio_6_1(SCENE):- SCENE = [[10,12], [[0,player, 100, 1], [1,player,100, 2], [2,player,100, 3], [3,player,100, 4], [4,player,100, 5], [5,player,100, 6]], [[12, cpu, 50, 1], [11, cpu, 50, 2],[10,cpu,50, 3], [9,cpu,50, 4], [8,cpu,50, 5], [7,cpu,50, 6]], 120,  playing].

escena_big_8_3(SCENE):- SCENE = [[20,20], [[0,player, 100, 1], [1,player,100, 2], [2,player,100, 3], [3,player,100, 4], [4,player,100, 5], [5,player,100, 6], [6,player,100, 7], [7,player,100, 8]], [[20, cpu, 100, 1], [19, cpu, 100, 2], [18,cpu,100, 3], [17,cpu,100, 4], [16,cpu,100, 5], [15,cpu,100, 6], [14,cpu,100, 7], [13,cpu,100, 8]], 120,  playing].
escena_big_8_2(SCENE):- SCENE = [[20,20], [[0,player, 100, 1], [1,player,100, 2], [2,player,100, 3], [3,player,100, 4], [4,player,100, 5], [5,player,100, 6], [6,player,100, 7], [7,player,100, 8]], [[20, cpu, 75, 1], [19, cpu, 75, 2], [18,cpu,75, 3], [17,cpu,75, 4], [16,cpu,75, 5], [15,cpu,75, 6], [14,cpu,75, 7], [13,cpu,75, 8]], 120,  playing].
escena_big_8_1(SCENE):- SCENE = [[20,20], [[0,player, 100, 1], [1,player,100, 2], [2,player,100, 3], [3,player,100, 4], [4,player,100, 5], [5,player,100, 6], [6,player,100, 7], [7,player,100, 8]], [[20, cpu, 50, 1], [19, cpu, 50, 2], [18,cpu,50, 3], [17,cpu,50, 4], [16,cpu,50, 5], [15,cpu,50, 6], [14,cpu,50, 7], [13,cpu,50, 8]], 120,  playing].

%Predicado: Selecciona una escena a partir del largo y alto del mapa, los enemigos y la dificultad
%Entrada:int x int x int x int x Variable
%Salida: TDA Ecena
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

%Predicado: crea un escenario si los valores para crearlos son verdaderos
%Entrada: int x int x int x int 
%Salida: SCENE = TDA escena
createScene(N,M,E,D,SCENE):- largo(N), alto(M), enemigos(E), dificultad(D), selectScene(N,M,E,D,SCENE),!.
/*#################################################
################Termina createScene################
###################################################*/
%#############################################################################################################################

%#############################################################################################################################
/*##################################################
################Comienza CheckScene#################
####################################################*/

%Predicado: comprueba si 2 personajes están en la misma posicion 
%Entrada: TDA personajes x lista TDA personaje
%Salida: bool
compararPos([_,_,_,_], []).
compararPos([POS1,_,_,_], [POS2,_,_,_]):- not( POS1=POS2 ) .
compararPos([POS,_,_,_], [[POS2, _,_,_]|PJs]):- not(POS = POS2), compararPos([POS, _,_,_],PJs),! .

%Predicado: compara la posicion y la ID de los personajes en una lista de TDAs de personaje
%Entrada: TDA personaje x Lista TDA personaje
%Salida: bool
compararPosIDPj([_,_,_,_], []).
compararPosIDPj([POS,_,_,ID], [[POS2, _,_,ID2]|PJs]):- not(POS = POS2), not(ID = ID2), compararPosIDPj([POS, _,_,ID],PJs),! .

%S partir de diferentes casos bases posible commprueba que las posisiones de los personajes sean diferentes
%Predicado: compara las posisiones de players y cpu a partir de las listas que contienen sus TDAs
%Entrada: Lista TDA personaje x Lista TDA personaje
%Salida: bool
diferentePos([],[]).
diferentePos([PJ|PJs],[]):- compararPosIDPj(PJ, PJs), diferentePos(PJs,[]),! .
diferentePos([],[CPU|CPUs]):-  compararPosIDPj(CPU, CPUs),diferentePos([], CPUs),! .
diferentePos([PJ|PJs], [CPU|CPUs]):- compararPosIDPj(PJ, PJs), compararPosIDPj(CPU, CPUs), compararPos(PJ,CPU),compararPos(CPU,PJs), compararPos(PJ,CPUs), diferentePos(PJs, CPUs), ! .
diferentePos([]).
diferentePos([PJ|PJs]):- compararPosIDPj(PJ, PJs), diferentePos(PJs).

%Hechos para checkScene
%etiquetaPlayer debe ser player
etiquetaPlayer(player).
%etiquetaCPU debe ser cpu
etiquetaCPU(cpu).

%Predicado: compuerba que una posicion esté dentro del mapa
%Entrada: int x int (X es el largo del mapa)
%Salida: bool
posValida(POS,X):- POS >= 0  ; POS =< X .

%Predicado: verifica si la ID del personaje es factible teniendo en cuenta el largo del lugar que es X
%Entrada: int x int
%Salida: bool
esID(ID,X):- (X = 20, ID =< 8); (X=12, (ID =< 4; ID =< 6)); (X = 10, (ID =< 5; ID =< 4; ID =< 2 )) , ID > 0.
%Predicado: verifica si la salud del personaje está entre las posibles
%Entrada: int
%Salida: bool
salud(VIDA):- VIDA >= 0, VIDA =< 100.
%Predicado:se usa como puente para verificar la salud y luego la ID del personaje (los dos ultimos parametros de el TDA personaje) 
%Entrada: lista de enteros, entero X
%Salida: bool
esVida([VIDA| ID],X):- salud(VIDA), esID(ID,X).

%Predicado:comprueba si la etiqueta es player o cpu respectvamente y hace un llamado a la funcion que corrobra con que el personaje es un TDA personaje 
%Entrada: lista [strin| vida, etc]
%Salida:bool
esPlayer([ETIQUETA|VIDA],X):- etiquetaPlayer(ETIQUETA), esVida(VIDA,X).
esCPU([ETIQUETA|VIDA],X):- etiquetaCPU(ETIQUETA), esVida(VIDA,X).

%Predicado: comprueba que cada personaje tengan el TDA como se debe, se revisan por separado las listas de player y cpus
%Entrada: lista de TDA personajes x int largo del mapa
%Salida:bool
es_Character_Cpu([],  _).
es_Character_Cpu([[POS|ETIQUETA]|CPUs],X):- posValida(POS,X), esCPU(ETIQUETA,X), es_Character_Cpu(CPUs,X) .
es_Character_player([],  _). 
es_Character_player([[POS|ETIQUETA]|PLAYERs],X):- posValida(POS,X), esPlayer(ETIQUETA,X), es_Character_player(PLAYERs,X) .

%Predicado: es una escena si los limites del mapa, las listas de personajes, el tiempo y el estado son correctos en un mapa
%ademas comprueba que los personajes no se superpongan
%Entrada: TDA escena
%Salida: bool
checkScene([[X,Y], PLAYERS, CPUS, TIEMPO, ESTADO]):- 
			largo(X), alto(Y), 
			((PLAYERS = [], CPUS = []);
			(PLAYERS= [], es_Character_Cpu(CPUS, X), diferentePos(PLAYERS,CPUS));
			(CPUS = [], es_Character_player(PLAYERS, X), diferentePos(PLAYERS,CPUS));
			(es_Character_player(PLAYERS,X), es_Character_Cpu(CPUS, X), diferentePos(PLAYERS,CPUS))), 
			(TIEMPO >= 0; TIEMPO =< 120), es_estado(ESTADO), !.

/*##################################################
#################Termina CheckScene#################
####################################################*/
%#############################################################################################################################

%#############################################################################################################################
/*###################################################
#################Comienza moveMember#################
#####################################################*/
%Predicado: Cambiar Estado cuando TIME = 0
%Entrada: TDA escena
%Salida: escena con el estado evaluado SceneOut
timeCero([[X,Y], [PJ|PJs], [CPU|CPUs], TIME, ESTATE], SceneOut):- (TIME =< 0, ESTATE = playing ,SceneOut = [[X,Y], [PJ|PJs], [CPU|CPUs], TIME, draw]), ! ; (SceneOut= [[X,Y], [PJ|PJs], [CPU|CPUs], TIME, ESTATE]) .

%Predicado: invierte una lista
%Entrada: Lista TDA personaje
%Salida: Lista invertida INVERT
invertirLista([PJ|PJs], INVERT):- reverse([PJ|PJs], INVERT) .

%Predicado: veerifica si un persona está fuera del mapa
%Entrada: TDA personaje x int
%Salida: bool
outMap([POS,_,_,_], LARGO_MAPA):- POS < 0; POS > LARGO_MAPA .

%Predicado: si es el personaje buscado (identificado por la ID) se le cambia la posicion
%Entrada: TDA personaje x int x int
%Salida: TDA personaje CHAR
esPJ([POS,TIPO,VIDA,ID], MEMBER,MOVE, CHAR):- ID=MEMBER , CHAR = [NEWPOS,TIPO,VIDA,ID], NEWPOS is POS + MOVE.

%Predicado: revisa todos los personajes de una lista y mueve al que corresponde, si cae fuer del mapa lo elimina de la lista de personajes
%Entrada: lista TDA personajes x int x int x lista x int largo mapa
%Salida: lista de personajes con el movimiento hecho PERSONAJES
revisar([], PERSONAJES,PERSONAJES).
revisar([PJ|PJs], MEMBER, MOVE,NL, PERSONAJES, LARGO_MAPA):-
(esPJ(PJ, MEMBER, MOVE, CHAR), ((outMap(CHAR, LARGO_MAPA), (revisar(PJs, NL, PERSONAJES);revisar(PJs, MEMBER,MOVE, NL, PERSONAJES,LARGO_MAPA)));
(revisar(PJs, [CHAR|NL], PERSONAJES); revisar(PJs, MEMBER, MOVE, [CHAR|NL], PERSONAJES,LARGO_MAPA)))) ,!;
(revisar(PJs, [PJ|NL], PERSONAJES); revisar(PJs, MEMBER, MOVE, [PJ|NL], PERSONAJES,LARGO_MAPA))
.

%Predicado: llama a la funcion revisar pasando como una lista vacia como lista inicial o NL (New Lista)
%Entrada: lista TDA personajes x int x int x int largo mapa
%Salida: lista de personajes con el movimiento hecho PERSONAJES
mover(PERSONAJES, [PJ|PJs], MEMBER, MOVE,LARGO_MAPA):- revisar([PJ|PJs], MEMBER, MOVE,[], PERSONAJES,LARGO_MAPA) .


moveMember([[X,Y], [PJ|PJs], [CPU|CPUs], TIME,ESTATE], [TIPO, Member], MoveDir, SceneOut):- ESTATE = playing,
checkScene([[X,Y], [PJ|PJs], [CPU|CPUs], TIME,ESTATE]), 
((TIPO=player, mover(PERSONAJES, [PJ|PJs], Member, MoveDir,X),  ((PERSONAJES = [], NEWPERSONAJES = PERSONAJES, NEWESTATE = defeat);(invertirLista(PERSONAJES, NEWPERSONAJES), NEWESTATE = ESTATE )),
 SceneOutAux = [[X,Y], NEWPERSONAJES, [CPU|CPUs], NEWTIME,NEWESTATE] , NEWTIME is TIME - 1),!;
(TIPO=cpu, mover(COMP, [CPU|CPUs], Member, MoveDir,X),  ( (COMP = [], COMP = NEWCOMP, NEWESTATE=victory);(invertirLista(COMP, NEWCOMP), NEWESTATE = ESTATE)), 
 SceneOutAux = [[X,Y], [PJ|PJs], NEWCOMP, NEWTIME,NEWESTATE] , NEWTIME is TIME - 1)), 
( (NEWESTATE=playing, timeCero(SceneOutAux, SceneOut)),! ; (SceneOut = SceneOutAux)).

/*##################################################
#################Termina moveMember#################
####################################################*/
%#############################################################################################################################

%#############################################################################################################################
/*##################################################
###################Comienza Shoot###################
###################################################*/

%Predicado: convierte un angulo a radian
%Entrada: angulo en grados Angle
%Salida: Angulo en radian ALPHA
convertir(Angle, ALPHA):- ALPHA is (Angle *  pi)/ 180.

%Predicado: redondea el numero en X
%Entrada: float X
%Salida: int NEWX
toInt(X, NEWX):- round(X, NEWX).

%Predicado: obtiene el tiempo final a partir de la ecuacion itinerario de movimiento vertical
%Entrada: float ALPHA
%Salida: float T
tiempoFinal(ALPHA, T):- T is (8 * sin(ALPHA)/ 5).

%Predicado: se calcula donde cae el proyectir en el eje x
%Entrada: int x float x float
%Salida: int lugar de caida X
xdeT(XI, ALPHA, T, X):- X is XI + (8 * cos(ALPHA) * T).

%Predicado: dispara el el ptoyectil con una posicion inicial y un angulo
%Entrada: int x int
%Salida: int IMPACTO
disparar(POSINICIAL, Angle, IMPACTO):- convertir(Angle,ALPHA) ,tiempoFinal(ALPHA, T), xdeT(POSINICIAL, ALPHA, T, X), round(X,IMPACTO) .

%Predicado: verifica si el miembro tiene la ID que se busca
%Entrada: TDA personaje x int
%Salida: bool
obtenerMember([_,_,_,ID], Member):- ID = Member.

%Predicado: se obtiene la posicion inicial a partir de un personaje
%Entrada: TDA personaje
%Salida: int POSINICIAL
obtenerPos([POS,_,_,_], POSINICIAL):- POSINICIAL = POS.

%Predicado: si el impacto llega en la posicion del jugador, este resive daño
%Entrada: TDA personaje x int
%Salida: TDA personaje CHAR
damage([POS,TIPO,VIDA,ID], IMPACTO, CHAR):- POS = IMPACTO, CHAR=[POS, TIPO, NEWVIDA, ID], NEWVIDA is (VIDA - 50).

%Predicado: verifica si la vida de un personaje es 0 o menor
%Entrada: TDA personaje
%Salida:  bool
vidaCero([_,_,VIDA,_]):- VIDA =< 0 .

%Predicado: comprueba si algun personaje resive un impacto del proyectil eliminando a quienes mueran por este
%se cubren todos los casos posibles en los casos bases
%Entrada: lista TDA personaje x  int x lista 
%Salida:  lista TDA personaje en PERSONAJES
impactoPj([], [], PERSONAJES):- PERSONAJES=[] .
impactoPj([], PERSONAJES, PERSONAJES).
impactoPj([PJ], IMPACTO, LI, PERSONAJES):-
(damage(PJ,IMPACTO,CHARACTER), ((vidaCero(CHARACTER), (impactoPj([], LI, PERSONAJES);impactoPj([], IMPACTO, LI, PERSONAJES)));
(impactoPj([], [CHARACTER|LI], PERSONAJES); impactoPj([], IMPACTO, [CHARACTER|LI], PERSONAJES)))),!;
(impactoPj([], [PJ|LI], PERSONAJES); impactoPj([], IMPACTO, [PJ|LI], PERSONAJES)).
impactoPj([PLAYER|PLAYERs], IMPACTO, LI, PERSONAJES):-
(damage(PLAYER,IMPACTO,CHARACTER), ((vidaCero(CHARACTER), (impactoPj(PLAYERs, LI, PERSONAJES);impactoPj(PLAYERs, IMPACTO, LI, PERSONAJES)));
(impactoPj(PLAYERs, [CHARACTER|LI], PERSONAJES); impactoPj(PLAYERs, IMPACTO, [CHARACTER|LI], PERSONAJES)))),!;
(impactoPj(PLAYERs, [PLAYER|LI], PERSONAJES); impactoPj(PLAYERs, IMPACTO, [PLAYER|LI], PERSONAJES))
.

%Predicado: ejecuta un disparo la cpu o el player teniendo en cuentta su posicion inicial y si es el personaje que debe ejecutarlo
%Entrada: TDA personaje x int x int 
%Salida: int IMPACTO
disparoCPU([[POS, _,_,ID]], Member, Angle, IMPACTO):- (Member = ID,disparar(POS, Angle, IMPACTO)).
disparoCPU([CPU|CPUs], Member, Angle, IMPACTO ):- 
(obtenerMember(CPU, Member), obtenerPos(CPU, POS), disparar(POS, Angle, IMPACTO)); 
(disparoCPU(CPUs, Member, Angle, IMPACTO)) .
disparoPlayer([[POS, _,_,ID]], Member, Angle, IMPACTO):- (Member = ID,disparar(POS, Angle, IMPACTO)).
disparoPlayer([PLAYER|PLAYERs], Member, Angle, IMPACTO):- 
(obtenerMember(PLAYER, Member), obtenerPos(PLAYER, POS) ,disparar(POS, Angle, IMPACTO)); 
(disparoPlayer(PLAYERs, Member, Angle, IMPACTO)) .

%Predicado: si el escenario y los parametros son validos, ejecuta un disparo cambiando el tiempo y el estado de juego de ser necesario
%Entrada: TDA escena x lista con tipo de personaje x ID x atomo parabolico x int x Seed (que debe ser una variable anonima)
%Salida: TDA escena SceneOut
shoot([[X,Y], [PJ|PJs], [CPU|CPUs], TIME,ESTATE], [TIPO,Member], ShotType, Angle, _, SceneOut):- ESTATE = playing, checkScene([[X,Y], [PJ|PJs], [CPU|CPUs], TIME,ESTATE]), 
(ShotType = parabolico, (TIPO = player, disparoPlayer([PJ|PJs], Member, Angle, IMPACTO)); (TIPO = cpu, disparoCPU([CPU|CPUs], Member, Angle, IMPACTO))),
(impactoPj([PJ|PJs], IMPACTO, [], PERSONAJES), impactoPj([CPU|CPUs], IMPACTO, [], COMP)), 
((COMP = [], PERSONAJES = [], SceneOutAux = [[X,Y], PERSONAJES, COMP, NEWTIME, NEWESTATE], NEWESTATE = draw); 
(PERSONAJES = [], invertirLista(COMP, COMP2), SceneOutAux = [[X,Y], PERSONAJES, COMP2, NEWTIME, NEWESTATE], NEWESTATE = defeat);
(COMP = [], invertirLista(PERSONAJES,PERSONAJES2), SceneOutAux = [[X,Y], PERSONAJES2, COMP, NEWTIME, NEWESTATE], NEWESTATE = victory);
(invertirLista(PERSONAJES,PERSONAJES2), invertirLista(COMP,COMP2),  SceneOutAux = [[X,Y], PERSONAJES2, COMP2, NEWTIME, NEWESTATE] , NEWESTATE = ESTATE)), NEWTIME is TIME - 1,
( (NEWESTATE=playing, timeCero(SceneOutAux, SceneOut)),! ; (SceneOut = SceneOutAux))
.

/*#################################################
###################Termina Shoot###################
###################################################*/
%#############################################################################################################################

%#############################################################################################################################
/*###################################################
###################Comienza Update###################
####################################################*/

%Predicado: genera un numero aleatorio
%Entrada: int x int
%Salida: int 
random(L, U, R) :-
integer(L), integer(U), !,R is L+random(U-L).

%Predicado: Llama a la funcion moveMember con parametro de movimiento e ID del Miembro aleatorios
%si el personaje no está se llama denuevo
%Entrada: TDA escena
%Salida: TDA escena SceneOutAux
moveAleatorio([[X,Y], [PJ|PJs], [CPU|CPUs], TIME,ESTATE], SceneOutAux):- 
(random(-8,9, MoveDir), random(1,9,Member), moveMember([[X,Y], [PJ|PJs], [CPU|CPUs], TIME,ESTATE], [cpu, Member], MoveDir, SceneOutAux)),!;
moveAleatorio([[X,Y], [PJ|PJs], [CPU|CPUs], TIME,ESTATE], SceneOutAux)
.

%Predicado: verifica si el estado de un escenario es jugable
%Entrada: TDA escenario
%Salida: bool
estadoJugable([_,_,_,_,Estado]):- Estado=playing .

%Predicado: ejecuta un disparo con parametros aleatorios, si no son parametros validos se llama nuevamente para cambiar los valores
%Entrada: TDA escenario
%Salida: TDA escenario SceneOut
shootAleatorio([[X,Y], [PJ|PJs], [CPU|CPUs], TIME,ESTATE], SceneOut):- 
(random(-90,91,Angle),random(1,9,Member) , shoot([[X,Y], [PJ|PJs], [CPU|CPUs], TIME,ESTATE], [cpu,Member], parabolico, Angle, _, SceneOut)),!;
shootAleatorio([[X,Y], [PJ|PJs], [CPU|CPUs], TIME,ESTATE], SceneOut)
.

%Predicado: Ejecuta un movimiento y revisa si se puede ejecutar un disparo a partir del escenario actualizado
%Entrada: TDA escenario x variable anonima (SEED)
%Salida: TDA escenario
updateScene([[X,Y], [PJ|PJs], [CPU|CPUs], TIME,ESTATE], SEED, SceneOut):-
checkScene([[X,Y], [PJ|PJs], [CPU|CPUs], TIME,ESTATE]), moveAleatorio([[X,Y], [PJ|PJs], [CPU|CPUs], TIME,ESTATE], SceneOutAux),
((estadoJugable(SceneOutAux), shootAleatorio(SceneOutAux, SceneOut)),!;
(SceneOut = SceneOutAux)), SEED = _
.

/*##################################################
###################Termina Update###################
###################################################*/
%#############################################################################################################################

%#############################################################################################################################
/*###################################################
###################Comienza Play#####################
####################################################*/

%Predicado: ejecuta un turno entero que consta de movimiento y disparo de un personaje player 
% y un movimiento y tiro de un personaje cpu (siendo un personaje diferente el que se mueve y dispara para el caso de cpu)
% actualizando el escenario constantemente y entregandolo a mitad de la ejecucion de las acciones enel caso de que el estado de juego no sea playing
%Entrada: TDA escenario x int x int x atomo parabolico x int x variable anonima (SEED)
%Salida: TDA escenario SceneOut
play([[X,Y], [PJ|PJs], [CPU|CPUs], TIME,ESTATE], Member, MoveDir, ShotType, Angle, SEED, SceneOut):-
checkScene([[X,Y], [PJ|PJs], [CPU|CPUs], TIME,ESTATE]),
moveMember([[X,Y], [PJ|PJs], [CPU|CPUs], TIME,ESTATE], [player,Member], MoveDir, SceneOutAux1), 
((estadoJugable(SceneOutAux1), shoot(SceneOutAux1, [player,Member], ShotType, Angle, _, SceneOutAux2));
((SceneOut = SceneOutAux1),!)),
((estadoJugable(SceneOutAux2), updateScene(SceneOutAux2, SEED, SceneOut)),!;
(SceneOut=SceneOutAux2))
.

/*##################################################
###################Termina Play#####################
###################################################*/
%#############################################################################################################################