:-set_prolog_flag(answer_write_options,[quoted(true),portray(true),spacing(next_argument)]).
% -----------------------------------------------------------------------
% Trabalho pr�tico: factos de cidades com localiza��o baseada em
% latitude e longitude e predicado auxiliar para calcular a dist�ncia
% entre quaisquer duas destas cidades.
% ------------------------------------------------------------------------

:- [branch_and_bounce].

list_length([], 0).
list_length([_|T], L) :- list_length(T,N) , L is N+1.

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
    
% ============== Predicados da US5010 ==============

/* Old solution
tsp_greedy(Origin, _, _, _) :- not(city(Origin, _, _)), fail, !.
tsp_greedy(_, 0, _, _) :- fail, !.
tsp_greedy(Origin, 1, [Origin], 0):- !.
tsp_greedy(_, NumCities, _, _):- findall(CityName, city(CityName, _, _), Result), list_length(Result, Lenght), NumCities > Lenght, fail, !.
tsp_greedy(Origin, NumCities, FinalList, Cost):- tsp_greedy2(Origin, Origin, NumCities, [Origin], FinalList, Cost). %Verificar se cost iniciado causa problemas

tsp_greedy2(Origin, TempCity, 1, R, FinalList, Cost):- dist_cities(Origin, TempCity, Cost), reverse([Origin|R], FinalList), !.

tsp_greedy2(Origin, TempCity, NumCities, R, FinalList, Cost):- NewNumCities is NumCities - 1, closest_city(TempCity, R, ClosestCity, AssociatedCost), tsp_greedy2(Origin, ClosestCity, NewNumCities, [ClosestCity|R], FinalList, NewCost), Cost is NewCost + AssociatedCost.

closest_city(ReceivedCity, AlreadyVisited, MinCity, MinCost):- findall(
		(C, X),
		(city(X,_,_), not(member(X, AlreadyVisited)), dist_cities(ReceivedCity,X,C)),
		[H|T]
	), sort([H|T], [(MinCost, MinCity)|_]).
*/

tsp_greedy(Origin, _, _):- not(city(Origin, _, _)), fail, !.
tsp_greedy(Origin, FinalList, FinalCost):- findall(X, (city(X, _, _), X \== Origin), Result), list_length(Result, L), (L == 0 -> (FinalList = [Origin], FinalCost is 0) ; tsp_greedy2(Origin, Origin, L, [Origin], FinalList, FinalCost)).

tsp_greedy2(Origin, TempCity, 0, R, FinalList, Cost):- dist_cities(Origin, TempCity, Cost), reverse([Origin|R], FinalList), !.

tsp_greedy2(Origin, TempCity, NumCities, R, FinalList, Cost):- NewNumCities is NumCities - 1, closest_city(TempCity, R, ClosestCity, AssociatedCost), tsp_greedy2(Origin, ClosestCity, NewNumCities, [ClosestCity|R], FinalList, NewCost), Cost is NewCost + AssociatedCost.

closest_city(ReceivedCity, AlreadyVisited, MinCity, MinCost):- findall(
		(C, X),
		(city(X,_,_), not(member(X, AlreadyVisited)), dist_cities(ReceivedCity,X,C)),
		[H|T]
	), sort([H|T], [(MinCost, MinCity)|_]).

