:- use_module(library(term_to_json)).

:-set_prolog_flag(answer_write_options,[quoted(true),portray(true),spacing(next_argument)]).

route([lisboa, porto, madrid, paris, berlim, moscovo]).

order(1, porto).
order(2, madrid).

package(1, size1, 1, 4).
package(2, size2, 1, 5).
package(3, size3, 1, 3).
package(4, size2, 1, 7).
package(5, size1, 2, 2).
package(6, size3, 2, 1).

packageSize(size1, 3, 2, 3). %Height, width, depth
packageSize(size2, 2, 2, 2).
packageSize(size3, 2, 3, 2).

neperNum(2.71828).
numIterations(100).
alpha(0.99).
initialT(1.0).
numIterationsGivenT(T, Result) :- T =< 1, T > 0, Result is round(10 ** (3 - (2 * T))).  %T = 1, f(T) = 10 ; When T tends towards 0, f(T) tends towards 1000.
acceptanceProbability(Cold, Cnew, T, Result):- T > 0, T =< 1, neperNum(NeperNum), Result is NeperNum ** ((Cold - Cnew)/ T).

/**
 * Simulated annealing predicate that receives the dimensions of the truck, a list of order references (whose packages we want to ship).
 * It returns the total amount of volume not occupied in the truck in the variable "LeastUnusedSpace" and the packages that did not fit in the variable
 * "UnfitPackages".
 */
simulatedAnnealing(TruckHeight, TruckWidth, TruckDepth, OrderList, LeastUnusedSpacePackageOrder, LeastUnusedSpace, UnfitPackages):- getPackages(OrderList, PackageList),
	random_permutation(PackageList, PermutationPackageList),
	truckLoading(TruckHeight, TruckWidth, TruckDepth, PermutationPackageList, InitialUnusedSpace, InitialUnfitPackages),
	initialT(InitialT),
	numIterations(NumIterations),
	simulatedAnnealingIterations(InitialT, NumIterations, TruckHeight, TruckWidth, TruckDepth, PermutationPackageList, InitialUnusedSpace, InitialUnfitPackages, LeastUnusedSpacePackageOrder, LeastUnusedSpace, UnfitPackages), !.

/**
 * Predicate relative to the iterations executed 
 */
% There is no rule for when the temperature is 0 because it never gets to 0.
simulatedAnnealingIterations(_, 0, _, _, _, LeastUnusedSpacePackageOrder, LeastUnusedSpace, UnfitPackages, LeastUnusedSpacePackageOrder, LeastUnusedSpace, UnfitPackages).
simulatedAnnealingIterations(CurrentT, NumIterations, TruckHeight, TruckWidth, TruckDepth, S, Cold, OldUnfitPackages, LeastUnusedSpacePackageOrder, LeastUnusedSpace, UnfitPackages):-
	numIterationsGivenT(CurrentT, NumIterationsGivenT),
	sameTIterations(NumIterationsGivenT, CurrentT, TruckHeight, TruckWidth, TruckDepth, S, Cold, OldUnfitPackages, TempS, Ctemp, TempUnfitPackages),

	alpha(AlphaValue),
	NewT is AlphaValue * CurrentT,
	NewNumIterations is NumIterations - 1,
	simulatedAnnealingIterations(NewT, NewNumIterations, TruckHeight, TruckWidth, TruckDepth, TempS, Ctemp, TempUnfitPackages, LeastUnusedSpacePackageOrder, LeastUnusedSpace, UnfitPackages).

sameTIterations(0, _, _, _, _, TempS, Ctemp, TempUnfitPackages, TempS, Ctemp, TempUnfitPackages).
sameTIterations(NumIterations, T, TruckHeight, TruckWidth, TruckDepth, S, Cold, OldUnfitPackages, TempS, Ctemp, TempUnfitPackages):- 
	generateNeighbor(S, SLine),
	truckLoading(TruckHeight, TruckWidth, TruckDepth, SLine, Cnew, NewUnfitPackages),
	NewNumIterations is NumIterations - 1,
	(Cnew < Cold -> 
		 sameTIterations(NewNumIterations, T, TruckHeight, TruckWidth, TruckDepth, SLine, Cnew, NewUnfitPackages, TempS, Ctemp, TempUnfitPackages)
	;
		acceptanceProbability(Cold, Cnew, T, Ap),
		random_between(0,100,Value),
		Random is Value / 100,
		(Ap > Random ->
			sameTIterations(NewNumIterations, T, TruckHeight, TruckWidth, TruckDepth, SLine, Cnew, NewUnfitPackages, TempS, Ctemp, TempUnfitPackages)
		;
			sameTIterations(NewNumIterations, T, TruckHeight, TruckWidth, TruckDepth, S, Cold, OldUnfitPackages, TempS, Ctemp, TempUnfitPackages)
		)
	).

getPackages([], []).
getPackages([H|T], PackageListReturn):-
	findall(Id, (package(Id,_, H, _)), PackageListHead),
	getPackages(T, PackageList),
	append(PackageList, PackageListHead, PackageListReturn).

generateNeighbor(List, NewList):-
	length(List, ListLenght),
	random_between(1, ListLenght, Pos1),
	random_between(1, ListLenght, Pos2),
	(Pos1 == Pos2 -> 
		generateNeighbor(List, NewList)
	; 
		nth1(Pos1, List, Elem1),
		nth1(Pos2, List, Elem2),
		removeElementPos(List, Pos1, TempList1),
		insertElementPos(Elem2, Pos1, TempList1, TempList2),
		removeElementPos(TempList2, Pos2, TempList3),
		insertElementPos(Elem1, Pos2, TempList3, NewList)
	).

removeElementPos([_|T], 1, T):- !.
removeElementPos([H|T], N, [H|L]):- N > 1, TempN is N - 1, !, removeElementPos(T,TempN,L).

insertElementPos(New, 1, T, [New|T]):- !.
insertElementPos(New, Pos, [H|T], [H|L]):- Pos > 1, Pos1 is Pos - 1, insertElementPos(New, Pos1, T, L).

truckLoading(TruckHeight, TruckWidth, TruckDepth, PackageList, TotalUnusedSpace, UnfitPackages) :-
	getVolume(TruckHeight, TruckWidth, TruckDepth, Result),
	append([_{avHeight:TruckHeight, avWidth:TruckWidth, avDepth:TruckDepth, avVolume:Result, avWeight:0, avDestiny:0}], [], AvailableSpaces),
	truckLoading2(PackageList, AvailableSpaces, UnusedSpaces, _, UnfitPackages),
	% Order By Route
	% Order By Weight
	%printUnusedSpaces(UnusedSpaces), %FOR DEBUG PURPOSES
	%printUnfitPackages(UnfitPackages), %FOR DEBUG PURPOSES
	getTotalVolume(UnusedSpaces, TotalUnusedSpace), !.

getTotalVolume(UnusedSpaces, TotalUnusedSpace):- var(UnusedSpaces) -> TotalUnusedSpace is 0 ; getTotalVolume2(UnusedSpaces, TotalUnusedSpace).

getTotalVolume2([H], TotalUnusedSpace):- TotalUnusedSpace is H.get(avVolume).
getTotalVolume2([H|T], TotalUnusedSpace):- getTotalVolume(T, TempTotalUnusedSpace), TotalUnusedSpace is TempTotalUnusedSpace + H.get(avVolume). 

printUnusedSpaces(UnusedSpaces):- var(UnusedSpaces) -> write("") ; write("Dimensions of unused spaces: "), nl, printUnusedSpaces2(UnusedSpaces).

printUnusedSpaces2([]):- nl.
printUnusedSpaces2([H|T]):- write("- "), write("Height: "), write(H.get(avHeight)), write(" | Width: "), write(H.get(avWidth)), write(" | Depth: "), write(H.get(avDepth)), nl, printUnusedSpaces2(T).

printUnfitPackages(UnfitPackages):- var(UnfitPackages) -> write("") ; (write("Packages that did not fit: "), nl, printUnfitPackages2(UnfitPackages)).

printUnfitPackages2([]):- nl.
printUnfitPackages2([H|T]):- write("- "), write(H), nl, printUnfitPackages2(T).

truckLoading2([], UnusedSpaces, UnusedSpaces, UnfitPackages, UnfitPackages).
truckLoading2([H|T], AvailableSpaces, UnusedSpaces, TempUnfitPackages, UnfitPackages) :- 
	package(H, Size,POrder,PWeight),
	packageSize(Size, PHeight, PWidth, PDepth), %Here we obtain the packages dimensions
	order(POrder, PDestiny),
	sort(avVolume, @=<, AvailableSpaces, SortedAvailableSpaces),
	findSpace(SortedAvailableSpaces, PHeight, PWidth, PDepth, PWeight, PDestiny, ChosenSpace),
	(nonvar(ChosenSpace) ->
		updateAvailableSpaces(AvailableSpaces, ChosenSpace, PHeight, PWidth, PDepth, PWeight, PDestiny, NewAvailableSpaces),
		truckLoading2(T, NewAvailableSpaces, UnusedSpaces, TempUnfitPackages, UnfitPackages) 
	;
		append(TempUnfitPackages, [H], NewTempUnfitPackages),
		truckLoading2(T, AvailableSpaces, UnusedSpaces, NewTempUnfitPackages, UnfitPackages)), !.
	
findSpace([], _, _, _, _, _, _).		
			
findSpace([H|_], PHeight, PWidth, PDepth, PWeight, PDestiny, ChosenSpace):- 
	PHeight =< H.get(avHeight),
	PWidth =< H.get(avWidth),
	PDepth =< H.get(avDepth),
	(
		(H.get(avWeight) == 0 -> 
			(H.get(avDestiny) == 0;
			(
				calcDestiny(H.get(avDestiny), NumDestinyH),
				calcDestiny(PDestiny, NumDestinyP),
				NumDestinyP =< NumDestinyH
			)
			);
			false
		);
		(PWeight =< H.get(avWeight) -> 
			(H.get(avDestiny) == 0;
			(
				calcDestiny(H.get(avDestiny), NumDestinyH),
				calcDestiny(PDestiny, NumDestinyP),
				NumDestinyP == NumDestinyH
			)
			);
			false
		)
	),
	(
		H.get(avDestiny) == 0;
		(
			calcDestiny(H.get(avDestiny), NumDestinyH),
			calcDestiny(PDestiny, NumDestinyP),
			NumDestinyP =< NumDestinyH
		)
	),
	ChosenSpace = H,
	!.

findSpace([_|T], PHeight, PWidth, PDepth, PWeight, PDestiny, ChosenSpace):- 
	findSpace(T, PHeight, PWidth, PDepth, PWeight, PDestiny, ChosenSpace).
					

calcDestiny(Destiny, NumDestiny):-
	route(L),
	indexOf(L, Destiny, NumDestiny).

indexOf([Element|_], Element, 1):- !.
indexOf([_|Tail], Element, Index):-
  indexOf(Tail, Element, Index1),
  !,
  Index is Index1+1.

getVolume(Height, Width, Depth, Result):- Result is (Height * Width * Depth).

updateAvailableSpaces(AvailableSpaces, ChosenSpace, PHeight, PWidth, PDepth, PWeight, PDestiny, FinalAvailableSpaces) :-
	OldWeight is ChosenSpace.get(avWeight),
	OldDestiny = ChosenSpace.get(avDestiny),
	delete(AvailableSpaces, ChosenSpace, AvailableSpaces1),
	
	HeightDiff is ChosenSpace.get(avHeight) - PHeight,
	(HeightDiff > 0 -> getVolume(HeightDiff, PWidth, PDepth, ResultH), append([_{avHeight:HeightDiff, avWidth:PWidth, avDepth:PDepth, avVolume:ResultH, avWeight:PWeight, avDestiny:PDestiny}], AvailableSpaces1, AvailableSpaces2)
	; AvailableSpaces2 = AvailableSpaces1),
	
	DepthDiff is ChosenSpace.get(avDepth) - PDepth,
	(DepthDiff > 0 -> getVolume(ChosenSpace.get(avHeight), PWidth, DepthDiff, ResultD), append([_{avHeight : ChosenSpace.get(avHeight), avWidth: PWidth, avDepth: DepthDiff, avVolume: ResultD, avWeight:OldWeight, avDestiny:PDestiny}], AvailableSpaces2, AvailableSpaces3)
	; AvailableSpaces3 = AvailableSpaces2),

	WidthDiff is ChosenSpace.get(avWidth) - PWidth,
	(WidthDiff > 0 -> getVolume(ChosenSpace.get(avHeight), WidthDiff, ChosenSpace.get(avDepth), ResultW), append([_{avHeight:ChosenSpace.get(avHeight), avWidth:WidthDiff, avDepth:ChosenSpace.get(avDepth), avVolume:ResultW, avWeight:OldWeight, avDestiny:OldDestiny}], AvailableSpaces3, FinalAvailableSpaces)
	; FinalAvailableSpaces = AvailableSpaces3).
	
		
