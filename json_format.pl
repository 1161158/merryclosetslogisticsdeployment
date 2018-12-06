:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- json_object
        coordinates_body_response(latitude:number,longitude:number).
		
:- json_object
        manufactories_body_response(name:string,factoryRef:string,cityName:string,coordinates:coordinates_body_response/2).

:- json_object
	branch_and_bounce_body_response(caminho:list(string),cost:number,time:number).
		
:- json_object
	tsp_greedy_body_response(caminho:list(string), cost:number).
		
:- json_object
	opt_body_response(caminho:list(string), old_cost:number, new_cost:number).

:- json_object
	simulated_annealing_body_response(leastUnusedSpacePackageOrder:list(number), leastUnusedSpace:number, unfitPackages:list(number)).
