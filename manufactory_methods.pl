
:- dynamic(manufactory/5).


/**
 * ------------ Manufactory Methods ------------
 */ 
add_manufactories([], []).
add_manufactories([H|T], Added) :- CityName = H.cityName, Name = H.name, FactoryRef = H.factoryRef, 
                                Latitude = H.coordinates.latitude, Longitude = H.coordinates.longitude,
                                city(CityName, _, _),
                                not(manufactory(_,FactoryRef, _, _, _)), not(manufactory(_, _, _, Latitude, Longitude)),
                                asserta(manufactory(Name, FactoryRef, CityName, Latitude, Longitude)),  
                                add_manufactories(T, Added1),
                                append([H], Added1, Added).
					
add_manufactories([_|T], Added) :- add_manufactories(T, Added).
					 					 
					 					 
update_manufactory(NewFactory, FactoryRef) :- %not(manufactory(_, NewFactory.factoryRef, _, _, _)), 
                                            not(manufactory(_, _, _, NewFactory.coordinates.latitude, NewFactory.coordinates.longitude)), 
                                            retract(manufactory(_, FactoryRef, _, _, _)), 
											asserta(manufactory(NewFactory.name, FactoryRef, NewFactory.cityName, NewFactory.coordinates.latitude, NewFactory.coordinates.longitude)),
											format('Status: 204~n~n').
											
update_manufactory(_, _) :-  format('Status: 400~n~n').


delete_manufactory(FactoryRef) :- manufactory(_, FactoryRef, _, _, _), 
                                retract(manufactory(_, FactoryRef, _, _, _)), 
                                format('Status: 204~n~n').
delete_manufactory(_) :- format('Status: 400~n~n').


/**
 * Gets the manufactory that is the closest to the received city
 */
get_best_manufactory(CityName, BestManufactory) :- city(CityName, _, _),
												findall((Custo, FactoryRef), 
                                                ((manufactory(_, FactoryRef, CityFactoryName, _, _)), 
                                                dist_cities(CityName, CityFactoryName, Custo)), List),
                                                sort(List, [(Custo,BestManufactory)|_]), !.

get_best_manufactory(_, _) :- format('Status: 400~n~n').

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
%
