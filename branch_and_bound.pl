:-set_prolog_flag(answer_write_options,[quoted(true),portray(true),spacing(next_argument)]).
% -----------------------------------------------------------------------
% Trabalho prático: factos de cidades com localização baseada em
% latitude e longitude e predicado auxiliar para calcular a distância
% entre quaisquer duas destas cidades.
% ------------------------------------------------------------------------

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

obter_cities(Result) :- findall(City, city(City, _, _), Result).

calc_full_path(Origin, Dest, Cam, Custo, TamCities) :- (bnb(Origin, Dest, Cam1, Custo1, TamCities), append(Cam1, [Origin], Cam), dist_cities(Dest, Origin, C), Custo is Custo1 + C);true.
calc_path(_, [], [], _).
calc_path(Origin, [H|T], List, TamCities) :- calc_full_path(Origin, H, Cam, Custo, TamCities), calc_path(Origin, T, List1, TamCities), append([_{cam:Cam, custo:Custo}], List1, List), !.
calc_dist_element(Element, Cam, Custo, Tempo) :- get_time(X), obter_cities(List), length(List, TamCities), delete(List, Element, List1), 
												calc_path(Element, List1, Result1, TamCities), sort(custo, @=<, Result1, [H|_]), 
												Cam = H.cam, Custo is H.custo, get_time(Y), Tempo is (Y-X)*1000.

bnb(Orig,Dest,Cam,Custo, TamCities):- bnb2(Dest,[(0,[Orig])],Cam,Custo,TamCities ).
bnb2(Dest,[(Custo,[Dest|T])|_],Cam,Custo, TamCities):- reverse([Dest|T],Cam), length(Cam, Ll), (Ll is TamCities -> true).
bnb2(Dest,[(Ca,LA)|Outros],Cam,Custo, TamCities):- LA=[Act|_], findall((CaX,[X|LA]),
   										(Dest\==Act,dist_cities(Act,X,CustoX),\+ member(X,LA),CaX is CustoX + Ca),Novos),
  	 									append(Outros,Novos,Todos), sort(Todos,TodosOrd), bnb2(Dest,TodosOrd,Cam,Custo, TamCities).

% 10 cidades -> aproximadamente 1 hora
% 11 cidades -> stack overflow












