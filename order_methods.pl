
:- dynamic(order/1).


/**
 * ------------ Orders Methods ------------
 */     
      
add_orders([], []).
add_orders([H|T], Added) :- OrderRef = H.orderRef, Packages= H.packages,
					 not(order(OrderRef)),
					 asserta(order(OrderRef)),
					 add_orders(T, Added1),
                     add_packages(OrderRef,Packages,AddedPackages),
					 append([OrderRef], AddedPackages, Added2),
                     append(Added2, Added1, Added).

add_orders([_|T], Added) :- add_orders(T, Added).
