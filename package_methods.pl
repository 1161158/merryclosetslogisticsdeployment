
:- dynamic(package/3).


/**
 * ------------ Packages Methods ------------
 */  

add_packages(_, [],[]).
add_packages(OrderRef, [H|T],Added) :- Tag = H.tag, SizeRef = H.sizeRef,
					 not(package(Tag,_,_)),
					 packageSize(SizeRef,_,_,_),
					 asserta(package(Tag,SizeRef,OrderRef)),
					 add_packages(OrderRef,T, Added1),
					 append([H], Added1, Added).

add_packages(_, [_|T], Added) :- add_packages(_,T, Added).