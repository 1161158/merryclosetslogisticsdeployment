:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_header)).
:- use_module(library(term_to_json)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- dynamic(city/3).
:- dynamic(manufactory/5).
:- dynamic(packageSize/4).
:- dynamic(package/3).
:- dynamic(truck/4).


encoding(utf8).  

server(Port) :-  
        http_server(http_dispatch, [port(Port)]),
		[json_format, truck_methods, package_sizes_methods, order_methods, city_methods, manufactory_methods, package_methods],
		getCities(Cities),
		add_cities(Cities, CitiesAdded),
		getSizes(Sizes),
		add_package_sizes(Sizes, SizesAdded),
		getOrders(Orders),
        add_orders(Orders, OrdersAdded),
        getManufactories(Manufactories),
		add_manufactories(Manufactories, ManufactoriesAdded),
        writeln("########################### Cities ###########################"),
        writeln(CitiesAdded),
        writeln("########################### Manufactories ###########################"),
        writeln(ManufactoriesAdded),
        writeln("########################### Orders ###########################"),
        writeln(OrdersAdded),
        !.
		
stop_server(Port) :-
    http_stop_server(Port, []),
    retractall(city(_, _, _)),
    retractall(manufactory(_, _, _, _, _)),
    retractall(order(_)),
    retractall(package(_, _, _)),
    retractall(packageSize(_, _, _, _)),
    retractall(truck(_, _, _, _)).
		

:- http_handler('/city', cityReply, []).
:- http_handler('/manufactory', manufactoryReply, []).
:- http_handler('/order', orderReply, []).
:- http_handler('/truck', truckReply, []).
:- http_handler('/packageSize', packageSizeReply, []).
:- http_handler('/best_manufactory', orderManufactoryReply, []).
:- http_handler('/algorithm/bnb', branchAndBoundReply, []).
:- http_handler('/algorithm/tsp_greedy', tspGreedyReply, []).
:- http_handler('/algorithm/opt', optReply, []).
:- http_handler('/algorithm/simulated_annealing', simulatedAnnealingReply, []).


/*
    ------------------------- Algorithms Requests ----------------
*/
simulatedAnnealingReply(Request) :-
    [simulated_annealing],
    member(method(get), Request), !,
    http_parameters(Request,[truckId(TruckId, [string]), order(OrderList, [list(string)])]),
    truck(TermTruckId, TruckHeight, TruckWidth, TruckDepth),
    simulatedAnnealing(TruckHeight, TruckWidth, TruckDepth, OrderList, LeastUnusedSpacePackageOrder, LeastUnusedSpace, UnfitPackages),
    prolog_to_json(simulated_annealing_body_response(LeastUnusedSpacePackageOrder, LeastUnusedSpace, UnfitPackages), Reply),
    reply_json(Reply).


branchAndBoundReply(Request) :-
    [branch_and_bound],
    member(method(get), Request), !,
    http_parameters(Request,[cityName(CityName, [string])]),
    calc_dist_element(Term, Cam, Custo, Tempo),
    prolog_to_json(branch_and_bounce_body_response(Cam,Custo, Tempo), Reply),
    reply_json(Reply).
		
tspGreedyReply(Request) :-
    [tsp_greedy],
    member(method(get), Request), !,
    http_parameters(Request,[cityName(CityName, [string])]),
    tsp_greedy(Term, FinalList, FinalCost),
    prolog_to_json(tsp_greedy_body_response(FinalList, FinalCost), Reply),
    reply_json(Reply).

optReply(Request) :-
    [opt],
    member(method(get), Request), !,
    http_parameters(Request,[cityName(CityName, [string])]),
    opt(Term, FinalEdgeList, TspCost, NewCost),
    prolog_to_json(opt_body_response(FinalEdgeList, TspCost, NewCost), Reply),
    reply_json(Reply).


/*
   ------------ Order Requests ------------
*/ 
orderReply(Request) :-
    member(method(post), Request), !,
    http_read_json(Request, Data, [json_object(dict)]),
    %format('Content-type: application/json~n~n', []),
    add_orders(Data, Added),
    reply_json(Added).


/*
   ------------ PackageSize Requests ------------
*/ 
packageSizeReply(Request) :-
    member(method(post), Request), !,
    http_read_json(Request, Data, [json_object(dict)]),
    add_package_sizes(Data, Added),
    reply_json(Added).

packageSizeReply(Request) :-
    member(method(put), Request), !,
    http_read_json(Request, Data, [json_object(dict)]),
    http_parameters(Request,[sizeRef(SizeRef, [string])),
    update_size(Data, SizeRef).

packageSizeReply(Request) :-
    member(method(delete), Request), !,
    http_parameters(Request,[sizeRef(SizeRef, [string])),
    delete_size(Data, SizeRef).


/*
    ------------ City Requests ------------
*/ 
cityReply(Request) :-
        member(method(post), Request), !,
        http_read_json(Request, Data, [json_object(dict)]),
        %format('Content-type: application/json~n~n', []),
        add_cities(Data, Added),
		reply_json(Added).
		
cityReply(Request) :-
        member(method(put), Request), !,
        http_read_json(Request, Data, [json_object(dict)]),
		http_parameters(Request,[latitude(Latitude, [float]), longitude(Longitude, [float])]),
        update_city(Data, Latitude, Longitude).

cityReply(Request) :-
        member(method(delete), Request), !,
		http_parameters(Request,[latitude(Latitude, [float]), longitude(Longitude, [float])]),
        delete_city(Latitude, Longitude).


/*
   ------------ Manufactory Requests ------------
*/
manufactoryReply(Request) :-
        member(method(post), Request), !,
        http_read_json(Request, Data, [json_object(dict)]),
        add_manufactories(Data, Added),
        reply_json(Added).

manufactoryReply(Request) :-
        member(method(put), Request), !,
        http_read_json(Request, Data, [json_object(dict)]),
		http_parameters(Request,[factoryRef(FactoryRef, [string])]),
        update_manufactory(Data, FactoryRef).

manufactoryReply(Request) :-
        member(method(delete), Request), !,
		http_parameters(Request,[factoryRef(FactoryRef, [string])]),
		delete_manufactory(FactoryRef).
	
orderManufactoryReply(Request) :-
        member(method(get), Request), !,
        http_parameters(Request,[orderCity(OrderCity, [string])]),
		get_best_manufactory(OrderCity, BestManufactory),
		manufactory(Name, BestManufactory, CName, Latitude, Longitude),
		prolog_to_json(manufactories_body_response(Name, BestManufactory, CName, coordinates_body_response(Latitude, Longitude)), Reply),
        reply_json(Reply).


/*
    ------------ Truck Requests ------------
*/ 
truckReply(Request) :-
    member(method(post), Request), !,
    http_read_json(Request, Data, [json_object(dict)]),
    add_trucks(Data, Added),
    reply_json(Added).



/*
    ------------ Requests to the Order ------------
*/ 
getCities(Dict):-
    retractall(city(_, _, _)),
    setup_call_cleanup(
        http_open('http://localhost:1234/cities', In, [request_header('Accept'='application/json')]),
        json_read_dict(In, Dict),
        close(In)
  ).

getManufactories(Dict):-
    retractall(manufactory(_, _, _, _, _)),
    setup_call_cleanup(
        http_open('http://localhost:1234/manufactures', In, [request_header('Accept'='application/json')]),
        json_read_dict(In, Dict),
        close(In)
  ).

getSizes(Dict):-
    retractall(packageSize(_, _, _, _)),
    setup_call_cleanup(
        http_open('http://localhost:1234/sizes', In, [request_header('Accept'='application/json')]),
        json_read_dict(In, Dict),
        close(In)
    ).

getOrders(Dict):-
    retractall(order(_)),
    setup_call_cleanup(
        http_open('http://localhost:1234/orders', In, [request_header('Accept'='application/json')]),
        json_read_dict(In, Dict),
        close(In)
    ).