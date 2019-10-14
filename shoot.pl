
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

impactoPlayer([], PERSONAJES, PERSONAJES).
impactoPlayer([PLAYER|PLAYERs], IMPACTO, LI, PERSONAJES):- 
(damage(PLAYER,IMPACTO,CHARACTER), ((vidaCero(CHARACTER), impactoPlayer(PLAYERs, LI, PERSONAJES);impactoPlayer(PLAYERs, IMPACTO, LI, PERSONAJES));(impactoPlayer(PLAYERs, [CHARACTER|LI], PERSONAJES); impactoPlayer(PLAYERs, IMPACTO, [CHARACTER|LI], PERSONAJES))));
(impactoPlayer(PLAYERs, [PLAYER|LI], PERSONAJES); impactoPlayer(PLAYERs, IMPACTO, [PLAYER|LI], PERSONAJES))
.

impactoCpu([], PERSONAJES, PERSONAJES).
impactoCpu([CPU|CPUs], IMPACTO, LI, PERSONAJES):- 
(damage(CPU,IMPACTO,CHARACTER), (impactoPlayer(CPUs, [CHARACTER|LI], PERSONAJES); impactoPlayer(CPUs, IMPACTO, [CHARACTER|LI], PERSONAJES)));
(impactoPlayer(CPUs, [CPU|LI], PERSONAJES); impactoPlayer(CPUs, IMPACTO, [CPU|LI], PERSONAJES))
.

disparoCPU([[POS, _,_,ID]], Member, Angle, IMPACTO):- (Member = ID, convertir(Angle, ALPHA),disparar(POS, ALPHA, IMPACTO)).
disparoCPU([CPU|CPUs], Member, Angle, IMPACTO ):- 
(obtenerMember(CPU, Member), obtenerPos(CPU, POS), disparar(POS, Angle, IMPACTO)); 
(disparoPlayer(CPUs, Member, Angle, IMPACTO)) .
disparoPlayer([[POS, _,_,ID]], Member, Angle, IMPACTO):- (Member = ID, convertir(Angle, ALPHA),disparar(POS, ALPHA, IMPACTO)).
disparoPlayer([PLAYER|PLAYERs], Member, Angle, IMPACTO):- 
(obtenerMember(PLAYER, Member), obtenerPos(PLAYER, POS), disparar(POS, Angle, IMPACTO)); 
(disparoPlayer(PLAYERs, Member, Angle, IMPACTO)) .

shoot([[X,Y], [PJ|PJs], [CPU|CPUs], TIME,ESTATE], [TIPO,Member], ShotType, Angle, _, SceneOut):-
%checkScene([[X,Y], [PJ|PJs], [CPU|CPUs], TIEMPO,ESTATE]), 
(ShotType = parabolico, (TIPO = player, disparoPlayer([PJ|PJs], Member, Angle, IMPACTO)); (TIPO = cpu, disparoCPU([CPU|CPUs], Member, Angle, IMPACTO))),
(impactoPlayer([PJ|PJs], IMPACTO, [], PERSONAJES), impactoCpu([CPU|CPUs], IMPACTO, [], COMP)),
SceneOut = [[X,Y], PERSONAJES, COMP, NEWTIME, ESTATE], NEWTIME is TIME - 1,!
.