:- module(dacura_app,
	  [dacura_schema_update/1, 
	   dacura_instance_update/1, 
	   dacura_validate/1
	  ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)). 
:- use_module(library(http/json_convert)). 
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(semweb/rdf_persistency)).
:- use_module(schemaRules). 

http:location(dacura, '/dacura', []).

:- http_handler(dacura(.), dacura_reply, []). 
:- http_handler(dacura(schema), dacura_schema_update, []). 
:- http_handler(dacura(instance), dacura_instance_update, []). 
:- http_handler(dacura(validate), dacura_validate, []).
:- http_handler(dacura(schema_validate), dacura_schema_validate, []). 

:- use_module(library(http/json_convert)). 
:- use_module(utils). 

dacura_reply(_Request) :- 
    reply_html_page(cliopatria(default), 
		    [ title(['This is a test'])
		    ], 
		    [ h2('This is the root directory for the dacura API.'), 
		      p('Please read the information included in the dacura plugin documentation in order to interact with the RDF store and constraint manager')
		    ]).

dacura_schema_update(Request) :- 
    http_parameters(Request, [], [form_data(Data)]), 
    
    format('Content-type: application/json~n~n'), 

    % Get current stdout 
    current_output(Out), 

    % use pragma from client
    getKey(pragma, Data, Pragma_String, '{"tests": "all", "instance": "instance", "schema":"schema"}'),
    getKey(update, Data, Update_String, '[]'), 

    atom_json_term(Pragma_String, json(Pragma), []),
    atom_json_term(Update_String, json(Update), []),
    
    getKey(inserts, Update, InsertsPreLiteral, []),
    getKey(deletes, Update, DeletesPreLiteral, []),
    convert_triples(InsertsPreLiteral, Inserts),
    convert_triples(DeletesPreLiteral, Deletes),

    Delta=[inserts=Inserts, deletes=Deletes],

    rdf_transaction(runSchemaUpdate(Delta, Pragma, Witnesses)),
    	
    json_write(Out,Witnesses).


dacura_schema_update(Request) :- 
    http_parameters(Request, [], [form_data(Data)]), 
    
    format('Content-type: application/json~n~n'), 

    % Get current stdout 
    current_output(Out), 

    % use pragma from client
    getKey(pragma, Data, Pragma_String, '{"tests": "all", "instance": "instance", "schema":"schema"}'),
    getKey(update, Data, Update_String, '[]'), 

    atom_json_term(Pragma_String, json(Pragma), []),
    atom_json_term(Update_String, json(Update), []),
    
    getKey(inserts, Update, InsertsPreLiteral, []),
    getKey(deletes, Update, DeletesPreLiteral, []),
    convert_triples(InsertsPreLiteral, Inserts),
    convert_triples(DeletesPreLiteral, Deletes),

    Delta=[inserts=Inserts, deletes=Deletes],

    rdf_transaction(runSchemaUpdate(Delta, Pragma, Witnesses)),
    	
    json_write(Out,Witnesses).

    
dacura_instance_update(Request) :- 
    http_parameters(Request, [], [form_data(Data)]), 

    format('Content-type: application/json~n~n'), 

    % Get current stdout 
    current_output(Out), 

    % use pragma from client
    getKey(pragma, Data, Pragma_String, '{"tests": "all", "instance": "instance", "schema":"schema"}'),
    getKey(update, Data, Update_String, '{"update" : {}}'), 
    
    atom_json_term(Pragma_String, json(Pragma), []),
    atom_json_term(Update_String, json(Update), []),
    %% nl,
    %% write("Pragma: "),
    %% write(Pragma), 
    %% nl,
    %% write("Update: "),
    %% write(Update), 
    %% nl,
    getKey(inserts, Update, InsertsPreLiteral, []),
    getKey(deletes, Update, DeletesPreLiteral, []),
    convert_triples(InsertsPreLiteral, Inserts),
    convert_triples(DeletesPreLiteral, Deletes),
    %% nl,
    %% write("Inserts: "),
    %% write(Inserts), 
    %% nl,
    %% write("Deletes: "),
    %% write(Deletes), 
    %% nl,
    Delta=[inserts=Inserts, deletes=Deletes],

    % write(Delta), nl, write(Pragma), nl,  Witnesses=[],
    runInstanceUpdate(Delta, Pragma, Witnesses),
    		  
    json_write(Out,Witnesses).


dacura_validate(Request) :- 
    http_parameters(Request, [], [form_data(Data)]), 

    format('Content-type: application/json~n~n'), 

    % Get current stdout 
    current_output(Out),

    % use pragma from client
    getKey(pragma, Data, Pragma_String, '{"tests": "all", "instance": "instance", "schema":"schema"}'),
    
    atom_json_term(Pragma_String, json(Pragma), []),
    
    rdf_transaction(runFullValidation(Pragma, Witnesses)),

    json_write(Out,Witnesses).


dacura_schema_validate(Request) :- 
    http_parameters(Request, [], [form_data(Data)]), 

    format('Content-type: application/json~n~n'), 

    % Get current stdout 
    current_output(Out), 

    % use pragma from client
    getKey(pragma, Data, Pragma_String, '{"tests": "all", "schema":"schema"}'),
    
    atom_json_term(Pragma_String, json(Pragma), []),
    
    rdf_transaction(runSchemaValidation(Pragma, Witnesses)),

    json_write(Out,Witnesses).


/*
graphs_instance_schema(_Request) :-
	findall(Count-Graph,
		(   rdf_graph(Graph),
		    graph_triples(Graph, Count)
		),
		Pairs),
	keysort(Pairs, Sorted),
	pairs_values(Sorted, UpCount),
	reverse(UpCount, DownCount),
	append(DownCount, [virtual(total)], Rows),
	reply_html_page(cliopatria(default),
			title('RDF Graphs'),
			[ h1('Named graphs in the RDF store'),
			  \graph_table(Rows, [])
			]).
*/
