:-set_prolog_flag(answer_write_options,[quoted(true),portray(true),spacing(next_argument)]).
% -----------------------------------------------------------------------
% Trabalho pr�tico: factos de cidades com localiza��o baseada em
% latitude e longitude e predicado auxiliar para calcular a dist�ncia
% entre quaisquer duas destas cidades.
% ------------------------------------------------------------------------

:- [tsp_greedy]. 
 
%----------------------------------------------------------------------------------------------------
% rGraph(Origin,UnorderedListOfEdges,OrderedListOfEdges)
%
% Examples:
% ---------
% ?- rGraph(a,[[a,b],[b,c],[c,d],[e,f],[d,f],[e,a]],R).
%
% ?- rGraph(brussels,[[vienna, sarajevo], [sarajevo, tirana],[tirana,sofia], [sofia, minsk], [andorra,brussels],[brussels,minsk],[vienna,andorra]],R).
%
%
rGraph(Orig,[[Orig,Z]|R],R2):-!,
	reorderGraph([[Orig,Z]|R],R2).
rGraph(Orig,R,R3):-
	member([Orig,X],R),!,
	delete(R,[Orig,X],R2),
	reorderGraph([[Orig,X]|R2],R3).
rGraph(Orig,R,R3):-
	member([X,Orig],R),
	delete(R,[X,Orig],R2),
	reorderGraph([[Orig,X]|R2],R3).


reorderGraph([],[]).

reorderGraph([[X,Y],[Y,Z]|R],[[X,Y]|R1]):-
	reorderGraph([[Y,Z]|R],R1).

reorderGraph([[X,Y],[Z,W]|R],[[X,Y]|R2]):-
	Y\=Z,
	reorderGraph2(Y,[[Z,W]|R],R2).

reorderGraph2(_,[],[]).
reorderGraph2(Y,R1,[[Y,Z]|R2]):-
	member([Y,Z],R1),!,
	delete(R1,[Y,Z],R11),
	reorderGraph2(Z,R11,R2).
reorderGraph2(Y,R1,[[Y,Z]|R2]):-
	member([Z,Y],R1),
	delete(R1,[Z,Y],R11),
	reorderGraph2(Z,R11,R2).
	
	
%---------------------------------------------------------------------------------------------------------------------
% As fórmulas que verificam se dois segmentos se intersectam são para o plano, temos por isso de converter a latitude 
% e longitude para o plano.  
%
% Predicado auxiliar que a partir da latitude e longitude dá as coordenadas no plano através de uma conversão aproximada 
% com base numa esfera 
%
%  Conversão elipsoide -> plano: http://www.apsalin.com/convert-geodetic-to-cartesian.aspx 

%  linearCoord(lisbon,X,Y). 
%  X = 4908, 
%  Y = -789. 

linearCoord(City,X,Y):- 
    city(City,Lat,Lon), 
    geo2linear(Lat,Lon,X,Y). 

geo2linear(Lat,Lon,X,Y):- 
    degrees2radians(Lat,LatR), 
    degrees2radians(Lon,LonR), 
    X is round(6370*cos(LatR)*cos(LonR)), 
    Y is round(6370*cos(LatR)*sin(LonR)). 



%Para invocar:

%linearCoord(C1,X1,Y1),linearCoord(C2,X2,Y2),
%linearCoord(C3,X3,Y3),linearCoord(C4,X4,Y4),
%doIntersect((X1,Y1),(X2,Y2),(X3,Y3),(X4,Y4)),



%-------------------------------------------------------
%US5015

%Main method

opt(Orig, FinalEdgeList, TspCost, NewCost):-tsp_greedy(Orig, TspList, TspCost), 
	transform_in_edge_list(Orig, TspList, EdgeList),
	get_list_interception(EdgeList, InterceptionsList),
	optimize(EdgeList, InterceptionsList, List, TspCost, NewCost),
	rGraph(Orig, List, FinalEdgeList), !.


%Makes a list of pair that identify as edges

list_edges(FirstCity, [H], L):- append([[H, FirstCity]], [], L).
list_edges(Orig, [FirstCity, SecondCity | T], EdgeList):- 
	list_edges(Orig, [SecondCity | T], R), 
	append([[FirstCity, SecondCity]], R, EdgeList).


%Deletes from the edgelist an edge that has as parameters the same city.

transform_in_edge_list(Orig, List, EdgeAuxList):-list_edges(Orig, List, EdgeList), 
	delete(EdgeList, [Orig,Orig], EdgeAuxList),!.


get_list_interception([], []).

get_list_interception([[FirstCity, SecondCity] | T], InterceptionsList):-
	list_interceptions([FirstCity, SecondCity], T, Aux),
	get_list_interception(T, List),
	append(List, Aux, InterceptionsList), !.



%Procura os primeiros dois edges que se intercetem

list_interceptions([_,_], [], []).

list_interceptions([C1, C2], [ [C3, C4] | T], InterceptionsList):-
	notAdjacente([C1, C2], [C3, C4]),
	check_interception(C1, C2, C3, C4),
	list_interceptions([C1, C2], T, L),
	append([[C1, C2], [C3, C4]], L, InterceptionsList), !.

list_interceptions([C1, C2],[ [_, _] | T], InterceptionsList):-
	list_interceptions([C1, C2], T, InterceptionsList).


%Verifica se não é adjacente nem igual ao edge atual

notAdjacente([E1, E2], [E3, E4]):- E1 \== E4, E2 \== E3, E1 \== E3, E2 \== E4.

%Verifica se dois edges se intercetam
 
check_interception(C1,C2, C3, C4):- 
	linearCoord(C1,X1,Y1), linearCoord(C2,X2,Y2),
	linearCoord(C3,X3,Y3), linearCoord(C4,X4,Y4), 
	doIntersect((X1,Y1),(X2,Y2),(X3,Y3),(X4,Y4)).
	
	
	
%Método para recursivamente apagar segmentos cruzados,
%fazer append dos novos segmentos (delete de [a,b] [c,d]
%append de [a,c] [b,d]) e calculo do novo custo.

optimize(_, [], [], C, C).

optimize(EdgeList, [[C1, C2], [C3, C4] | T], FinalEdgeList, TspCost, NewCost):-
	dist_cities(C1, C2, Custo1),
	dist_cities(C3, C4, Custo2),
	dist_cities(C1, C3, Custo3),
	dist_cities(C2, C4, Custo4),
	New is TspCost - Custo1 - Custo2 + Custo3 + Custo4,
	optimize(EdgeList, T, _, New, NewCost),
	delete(EdgeList, [C1, C2], Aux1),
	delete(Aux1, [C3, C4], Aux2),
	append(Aux2, [[C1, C3], [C2, C4]], FinalEdgeList).