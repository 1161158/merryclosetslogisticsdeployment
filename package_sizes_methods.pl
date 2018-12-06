
:- dynamic(packageSize/4).


/**
 * ------------ PackageSize Methods ------------
 */ 
add_package_sizes([], []).
add_package_sizes([H|T], Added) :- SizeRef = H.sizeRef, Height = H.height,  Width = H.width, Depth = H.depth,
					 not(packageSize(SizeRef, _, _, _)),
					 asserta(packageSize(SizeRef, Height, Width, Depth)),
					 add_package_sizes(T, Added1),
					 append([H], Added1, Added).

add_package_sizes([_|T], Added) :- add_package_sizes(T, Added).


update_package_sizes(NewSize, SizeRef) :- not(packageSize(NewSize.sizeRef, _, _, _)),
                                          packageSize(SizeRef, _, _, _),
                                          not(packageSize(_, NewSize.height, NewSize.width, NewSize.depth)),
                                          retract(packageSize(SizeRef, _, _, _)),
                                          asserta(packageSize(NewSize.sizeRef, NewSize.height, NewSize.width, NewSize.depth)),
                                          format('Status: 204~n~n').
update_package_sizes(_, _, _) :- format('Status: 400~n~n').

delete_package_sizes(SizeRef) :- packageSize(SizeRef, _, _, _),
                                 retract(packageSize(SizeRef, _, _, _)),
                                format('Status: 204~n~n').
delete_package_sizes(_, _) :- format('Status: 400~n~n').
