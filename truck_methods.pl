
:- dynamic(truck/4).


/**
 * ------------ Truck Methods ------------
 */ 
add_trucks([], []).
add_trucks([H|T], Added) :- TruckRef = H.truckRef, Height = H.height,  Width = H.width, Depth = H.depth,
                     not(truck(TruckRef, _, _, _)),
                     not(truck(_, Height, Width, Depth)),
					 asserta(truck(TruckRef, Height, Width, Depth)),
					 add_trucks(T, Added1),
					 append([H], Added1, Added).

add_trucks([_|T], Added) :- add_trucks(T, Added).
