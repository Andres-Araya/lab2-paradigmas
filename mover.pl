

agrega([],[ELEM], ELEM).
agrega([X|Xs],[X|Ys], ELEM):- agrega(Xs,Ys,ELEM).
%mover(BUS,MOV,[POS, _, _, ID], [PLAYER|PLAYERs], PLAYERT):- (ID = BUS, agrega()) .
%cambiar(BUS, MOV, [LI|LI	s], PLAYERT):- mover(BUS,MOV, LI, LIs, PLAYERT).
/*revisar([POS,TIPO,VIDA,ID], [], MEMBER, MOVE, PERSONAJES,LI):- 
(ID = MEMBER, agrega(LI, PERSONAJES, [NEWPOS, TIPO, VIDA, ID]), NEWPOS is POS + MOVE); 
(not(ID=MEMBER), agrega(LI, PERSONAJES, [POS, TIPO, VIDA, ID])) .
revisar([POS,TIPO,VIDA,ID], [PJ|PJs], MEMBER, MOVE, PERSONAJES,LI):- 
(ID = MEMBER, agrega(LI, PERSONAJES, [MOVE, TIPO, VIDA, ID]),revisar(PJ,PJs,MEMBER,MOVE,PERSONAJES, PERSONAJES)); 
(not(ID=MEMBER), agrega(LI, PERSONAJES, [POS, TIPO, VIDA, ID]),revisar(PJ,PJs,MEMBER,MOVE,PERSONAJES,PERSONAJES)) .
%revisar([POS,TIPO,VIDA,ID], [PJ|PJs], MEMBER, MOVE, [PERSONAJES],[LI],NEWPOS):- 
%(ID = MEMBER, agrega(PERSONAJES, PERSONAJES, [(NEWPOS is (POS+MOVE)), TIPO, VIDA, ID]), revisar(PJ,PJs, MEMBER,MOVE,[], PERSONAJES,NEWPOS) );
%(agrega(LI, PERSONAJES, [POS, TIPO, VIDA, ID]), revisar(PJ,PJs, MEMBER,MOVE,LI,[],PERSONAJES, NEWPOS) ).
primero(PERSONAJES, [PJ|PJs], MEMBER, MOVE):- revisar(PJ, PJs, MEMBER, MOVE,PERSONAJES,[]) .
%cambiarpos(POS):- P .*/

%revisar([POS,TIPO,VIDA,ID], [], MEMBER, MOVE, PERSONAJES,LI):- 
%(ID = MEMBER, [[NEWPOS, TIPO, VIDA, ID]|LI], NEWPOS is POS + MOVE, PERSONAJES = [[NEWPOS, TIPO, VIDA, ID]]); 
%(not(ID=MEMBER), [[POS, TIPO, VIDA, ID]|LI]) .
esPJ([POS,TIPO,VIDA,ID], MEMBER,MOVE, CHAR):- ID=MEMBER , CHAR = [NEWPOS,TIPO,VIDA,ID], NEWPOS is POS + MOVE.
revisar([], PERSONAJES,PERSONAJES).
revisar([PJ|PJs], MEMBER, MOVE,NL, PERSONAJES):-
(esPJ(PJ, MEMBER, MOVE, CHAR), (revisar(PJs, [CHAR|NL], PERSONAJES);revisar(PJs, MEMBER,MOVE, [CHAR|NL], PERSONAJES))),!;
(revisar(PJs, [PJ|NL], PERSONAJES); revisar(PJs, MEMBER, MOVE, [PJ|NL], PERSONAJES))
.
%revisar([POS,TIPO,VIDA,ID], [PJ|PJs], MEMBER, MOVE, PERSONAJES,LI):- 
%(ID = MEMBER, [[POS,TIPO,VIDA,ID]|],revisar(PJ,PJs,MEMBER,MOVE,PERSONAJES, PERSONAJES)); 
%(not(ID=MEMBER), agrega(LI, PERSONAJES, [POS, TIPO, VIDA, ID]),revisar(PJ,PJs,MEMBER,MOVE,PERSONAJES,PERSONAJES)) .
primero(PERSONAJES, [PJ|PJs], MEMBER, MOVE):- revisar([PJ|PJs], MEMBER, MOVE,[], PERSONAJES) .