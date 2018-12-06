:-set_prolog_flag(answer_write_options,[quoted(true),portray(true),spacing(next_argument)]).
:-use_module(library(random)).
% -----------------------------------------------------------------------
% Trabalho pr�tico: factos de cidades com localiza��o baseada em
% latitude e longitude e predicado auxiliar para calcular a dist�ncia
% entre quaisquer duas destas cidades.
% ------------------------------------------------------------------------

%city(name,latitude,longitude)

city(brussels,50.8462807,4.3547273).
city(tirana,41.33165,19.8318).
city(andorra,42.5075025,1.5218033).
city(vienna,48.2092062,16.3727778).
city(minsk,53.905117,27.5611845).
% city(sarajevo,43.85643,18.41342).
% city(sofia,42.6976246,23.3222924).
% city(zagreb,45.8150053,15.9785014).
% city(nicosia,35.167604,33.373621).
% city(prague,50.0878114,14.4204598).
% city(copenhagen,55.6762944,12.5681157).
% city(london,51.5001524,-0.1262362).
% city(tallinn,59.4388619,24.7544715).
% city(helsinki,60.1698791,24.9384078).
% city(paris,48.8566667,2.3509871).
% city(marseille,43.296386,5.369954).
% city(tbilisi,41.709981,44.792998).
% city(berlin,52.5234051,13.4113999).
% city(athens,37.97918,23.716647).
% city(budapest,47.4984056,19.0407578).
% city(reykjavik,64.135338,-21.89521).
% city(dublin,53.344104,-6.2674937).
% city(rome,41.8954656,12.4823243).
% city(pristina,42.672421,21.164539).
% city(riga,56.9465346,24.1048525).
% city(vaduz,47.1410409,9.5214458).
% city(vilnius,54.6893865,25.2800243).
% city(luxembourg,49.815273,6.129583).
% city(skopje,42.003812,21.452246).
% city(valletta,35.904171,14.518907).
% city(chisinau,47.026859,28.841551).
% city(monaco,43.750298,7.412841).
% city(podgorica,42.442575,19.268646).
% city(amsterdam,52.3738007,4.8909347).
% city(belfast,54.5972686,-5.9301088).
% city(oslo,59.9138204,10.7387413).
% city(warsaw,52.2296756,21.0122287).
% city(lisbon,38.7071631,-9.135517).
% city(bucharest,44.430481,26.12298).
% city(moscow,55.755786,37.617633).
% city(san_marino,43.94236,12.457777).
% city(edinburgh,55.9501755,-3.1875359).
% city(belgrade,44.802416,20.465601).
% city(bratislava,48.1483765,17.1073105).
% city(ljubljana,46.0514263,14.5059655).
% city(madrid,40.4166909,-3.7003454).
% city(stockholm,59.3327881,18.0644881).
% city(bern,46.9479986,7.4481481).
% city(kiev,50.440951,30.5271814).
% city(cardiff,51.4813069,-3.1804979).

generations(3).
population(4).
crossing_prob(0.3).
mutation_prob(0.01).

num_cities(N):- 
    findall(City, city(City,_,_), List),
    list_length(List, N).

list_length([], 0).
list_length([_|T], L) :- list_length(T,N) , L is N+1.

%  dist_cities(brussels,prague,D).
%  D = 716837.
dist_cities(C1,C2,Dist):-
    city(C1,Lat1,Lon1),
    city(C2,Lat2,Lon2),
    distance(Lat1,Lon1,Lat2,Lon2,Dist).

degrees2radians(Deg,Rad):-
	Rad is Deg*0.0174532925.

% distance(latitude_first_point,longitude_first_point,latitude_second_point,longitude_second_point,distance
% in meters)
distance(Lat1, Lon1, Lat2, Lon2, Dis2):-
	degrees2radians(Lat1,Psi1),
	degrees2radians(Lat2,Psi2),
	DifLat is Lat2-Lat1,
	DifLon is Lon2-Lon1,
	degrees2radians(DifLat,DeltaPsi),
	degrees2radians(DifLon,DeltaLambda),
	A is sin(DeltaPsi/2)*sin(DeltaPsi/2)+ cos(Psi1)*cos(Psi2)*sin(DeltaLambda/2)*sin(DeltaLambda/2),
	C is 2*atan2(sqrt(A),sqrt(1-A)),
	Dis1 is 6371000*C,
	Dis2 is round(Dis1).

% distance(50.8462807,4.3547273,50.0878114,14.4204598,D).
% Online: http://www.movable-type.co.uk/scripts/latlong.html

% ============== Predicados para detetar interseções ==============
% Given three colinear points p, q, r, the function checks if
% point q lies on line segment 'pr'
%onSegment(P, Q, R)
onSegment((PX,PY), (QX,QY), (RX,RY)):-
    QX =< max(PX,RX),
    QX >= min(PX,RX),
    QY =< max(PY,RY),
    QY >= min(PY,RY).

 
% To find orientation of ordered triplet (p, q, r).
% The function returns following values
% 0 --> p, q and r are colinear
% 1 --> Clockwise
% 2 --> Counterclockwise

orientation((PX,PY), (QX,QY), (RX,RY), Orientation):-
	Val is (QY - PY) * (RX - QX) - (QX - PX) * (RY - QY),
	
	(
		Val == 0, !, Orientation is 0;
		Val >0, !, Orientation is 1;
		Orientation is 2
	).
 
orientation4cases(P1,Q1,P2,Q2,O1,O2,O3,O4):-
    orientation(P1, Q1, P2,O1),
    orientation(P1, Q1, Q2,O2),
    orientation(P2, Q2, P1,O3),
    orientation(P2, Q2, Q1,O4).
 
 
 
% The main function that returns true if line segment 'p1q1'
% and 'p2q2' intersect.
doIntersect(P1,Q1,P2,Q2):-
    % Find the four orientations needed for general and
    % special cases
	orientation4cases(P1,Q1,P2,Q2,O1,O2,O3,O4),
	
	(	
    % General case
    O1 \== O2 , O3 \== O4,!;

    % Special Cases
    % p1, q1 and p2 are colinear and p2 lies on segment p1q1
    O1 == 0, onSegment(P1, P2, Q1),!;
 
    % p1, q1 and p2 are colinear and q2 lies on segment p1q1
    O2 == 0, onSegment(P1, Q2, Q1),!;
 
    % p2, q2 and p1 are colinear and p1 lies on segment p2q2
    O3 == 0, onSegment(P2, P1, Q2),!;
 
     % p2, q2 and q1 are colinear and q1 lies on segment p2q2
    O4 == 0, onSegment(P2, Q1, Q2),!
    ).  
	
tsp_greedy(Origin, _, _):- not(city(Origin, _, _)), fail, !.
tsp_greedy(Origin, FinalList, FinalCost):- findall(X, (city(X, _, _), X \== Origin), Result), list_length(Result, L), (L == 0 -> (FinalList = [Origin], FinalCost is 0) ; tsp_greedy2(Origin, Origin, L, [Origin], FinalList, FinalCost)).

tsp_greedy2(Origin, TempCity, 0, R, FinalList, Cost):- dist_cities(Origin, TempCity, Cost), reverse([Origin|R], FinalList), !.

tsp_greedy2(Origin, TempCity, NumCities, R, FinalList, Cost):- NewNumCities is NumCities - 1, closest_city(TempCity, R, ClosestCity, AssociatedCost), tsp_greedy2(Origin, ClosestCity, NewNumCities, [ClosestCity|R], FinalList, NewCost), Cost is NewCost + AssociatedCost.

closest_city(ReceivedCity, AlreadyVisited, MinCity, MinCost):- findall(
		(C, X),
		(city(X,_,_), not(member(X, AlreadyVisited)), dist_cities(ReceivedCity,X,C)),
		[H|T]
	), sort([H|T], [(MinCost, MinCity)|_]).
	
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



% Genetic Algorithm %

generate:-
    generate_pop(Pop),
    evaluate_pop(Pop,PopEv),
    sort_pop(PopEv,PopOrd),
    generations(NG),
    generate_gen(NG,PopOrd).


sort_pop(PopAv,PopAvOrd):-
	bsort(PopAv,PopAvOrd).

bsort([X],[X]):-!.
bsort([X|Xs],Ys):-
	bsort(Xs,Zs),
	btroca([X|Zs],Ys).


btroca([X],[X]):-!.

btroca([X*VX,Y*VY|L1],[Y*VY|L2]):-
	VX>VY,!,
	btroca([X*VX|L1],L2).

btroca([X|L1],[X|L2]):-btroca(L1,L2).



generate_pop(Pop) :-
    population(SizePop),
    findall(City,city(City,_,_),CityList),
    generate_pop(SizePop,CityList,Pop).

generate_pop(0,_,[ ]) :- !.

generate_pop(SizePop,CityList,[Ind|Others]) :-
    SizePop1 is SizePop-1,
    generate_pop(SizePop1,CityList,Others),
    generate_ind(CityList,Ind), \+ member(Ind,Others).

generate_ind(CityList,Ind) :-
    random_permutation(CityList,Ind).

generate_gen(0,Pop):- !,
    write('Generation '), write(0), write(':'), nl,
    write(Pop), nl.

generate_gen(G,Pop):-
    write('Generation '), write(G), write(':'), nl,
    write(Pop), nl,
    crossover(Pop,NPop1),
    mutation(NPop1,NPop),
    evaluate_pop(NPop,NPopEv),
    msort(NPopEv,NPopEvOrd),
    sort_pop(NPopEv,NPopSort),
    G1 is G-1,
    generate_gen(G1,NPopSort).



generate_cutpoints(P1,P2):-
	generate_cutpoints1(P1,P2).

generate_cutpoints1(P1,P2):-
	num_cities(N),
	NTemp is N+1,
	random(1,NTemp,P11),
	random(1,NTemp,P21),
	P11\==P21,!,
	((P11<P21,!,P1=P11,P2=P21);P1=P21,P2=P11).

generate_cutpoints1(P1,P2):-
	generate_cutpoints1(P1,P2).


eliminate([],_,[]):-!.

eliminate([X|R1],L,[X|R2]):-
	not(member(X,L)),!,
	eliminate(R1,L,R2).

eliminate([_|R1],L,R2):-
	eliminate(R1,L,R2).

eliminateh([],[]).

eliminateh([h|R1],R2):-!,
	eliminateh(R1,R2).

eliminateh([X|R1],[X|R2]):-
	eliminateh(R1,R2).

insert([],L,_,L):-!.
insert([X|R],L,N,L2):-
	num_cities(T),
	((N>T,!,N1 is N mod T);N1 = N),
	insert1(X,N1,L,L1),
	N2 is N + 1,
	insert(R,L1,N2,L2).


insert1(X,1,L,[X|L]):-!.
insert1(X,N,[Y|L],[Y|L1]):-
	N1 is N-1,
	insert1(X,N1,L,L1).


crossover([],[]).
crossover([Ind*_],[Ind]).
crossover([Ind1*_,Ind2*_|Other],[NInd1,NInd2|Other1]):-
    generate_cutpoints(P1,P2),
    crossing_prob(Pcross), Pc is random(1),
    ((Pc =< Pcross, !, cross(Ind1,Ind2,P1,P2,NInd1),
    cross(Ind2,Ind1,P1,P2,NInd2));
    (NInd1=Ind1,NInd2=Ind2)),
    crossover(Other,Other1).

cross(Ind1,Ind2,P1,P2,NInd11):-
	sublist(Ind1,P1,P2,Sub1),
	num_cities(NumT),
	R is NumT-P2,
	rotate_right(Ind2,R,Ind21),
	eliminate(Ind21,Sub1,Sub2),
	P3 is P2 + 1,
	insert(Sub2,Sub1,P3,NInd1),
	eliminateh(NInd1,NInd11).

fillh([],[]).

fillh([_|R1],[h|R2]):-
	fillh(R1,R2).


sublist(L1,I1,I2,L):-
	I1 < I2,!,
	sublist1(L1,I1,I2,L).

sublist(L1,I1,I2,L):-
	sublist1(L1,I2,I1,L).

sublist1([X|R1],1,1,[X|H]):-!,
	fillh(R1,H).

sublist1([X|R1],1,N2,[X|R2]):-!,
	N3 is N2 - 1,
	sublist1(R1,1,N3,R2).

sublist1([_|R1],N1,N2,[h|R2]):-
	N3 is N1 - 1,
	N4 is N2 - 1,
	sublist1(R1,N3,N4,R2).

rotate_right(L,K,L1):-
	num_cities(N),
	T is N - K,
	rotate_right1(T,L,L1).

rotate_right1(0,L,L):-!.
rotate_right1(N,[X|R],R2):-
	N1 is N - 1,
	append(R,[X],R1),
	rotate_right1(N1,R1,R2).


evaluate_pop([ ],[ ]).
evaluate_pop([Ind|Other],[Ind*V|Other1]):-
    eval(Ind,V),
    evaluate_pop(Other,Other1).

eval([H|T], Cost):-
    eval1([H|T], H, Cost), !.

eval1([H], FirstElement, Cost):- dist_cities(H, FirstElement, Cost).
eval1([C1, C2|T], FirstElement, Cost):- 
    eval1([C2 | T], FirstElement, Cost1),
    dist_cities(C1, C2, Aux),
    Cost is Cost1 + Aux.


mutation([ ],[ ]).

mutation([Ind|Rest],[NInd|Rest1]) :-
    mutation_prob(Pmut),
    ((maybe(Pmut),!,mutation1(Ind,NInd)) ; NInd=Ind),
    mutation(Rest,Rest1).

mutation1(Ind,NInd) :-
    generate_cutpoints(P1,P2),
    mutation22(Ind,P1,P2,NInd).

mutation22([G1|Ind],1,P2,[G2|NInd]) :-
    !, P21 is P2-1, mutation23(G1,P21,Ind,G2,NInd).

mutation22([G|Ind],P1,P2,[G|NInd]) :-
    P11 is P1-1, P21 is P2-1, mutation22(Ind,P11,P21,NInd).

mutation23(G1,1,[G2|Ind],G2,[G1|Ind]) :- !.

mutation23(G1,P,[G|Ind],G2,[G|NInd]) :-
    P1 is P-1,
    mutation23(G1,P1,Ind,G2,NInd).