

/**
 * ------------ City Methods ------------
 */ 
add_cities([], []).
add_cities([H|T], Added) :- CityName = H.cityName, Latitude = H.cityCoordinates.latitude, Longitude = H.cityCoordinates.longitude,
                     not(city(CityName, _, _)),
					 asserta(city(CityName, Latitude, Longitude)),
					 add_cities(T, Added1),
					 append([H], Added1, Added).

add_cities([_|T], Added) :- add_cities(T, Added).

update_city(NewCity, Latitude, Longitude) :- not(city(NewCity.cityName, _, _)),
                                          city(_, Latitude, Longitude),
                                          retract(city(_, Latitude, Longitude)),
                                          asserta(city(NewCity.cityName, Latitude, Longitude)),
                                          format('Status: 204~n~n').
update_city(_, _, _) :- format('Status: 400~n~n').

delete_city(Latitude, Longitude) :- city(_, Latitude, Longitude),
                                    retract(city(_, Latitude, Longitude)),
                                    format('Status: 204~n~n').
delete_city(_, _) :- format('Status: 400~n~n').

