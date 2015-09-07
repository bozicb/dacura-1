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
:- use_module(library(http/http_client)).
:- use_module(library(http/json)). 
:- use_module(library(http/json_convert)). 
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(semweb/rdf_persistency)).
:- use_module(schemaRules).
:- use_module(test).

% Logging / Turn off for production
:- use_module(library(http/http_log)).

http:location(dacura, '/dacura', []).

:- http_handler(dacura(.), dacura_reply, []). 
:- http_handler(dacura(schema), dacura_schema_update, []). 
:- http_handler(dacura(instance), dacura_instance_update, []). 
:- http_handler(dacura(validate), dacura_validate, []).
:- http_handler(dacura(schema_validate), dacura_schema_validate, []).
:- http_handler(dacura(test), dacura_test, []). 

:- use_module(library(http/json_convert)). 
:- use_module(utils). 

:- debug(http(request)).

dacura_reply(_Request) :- 
    reply_html_page(cliopatria(default), 
		    [ title(['This is a test'])
		    ], 
		    [ h2('This is the root directory for the dacura API.'), 
		      p('Please read the information included in the dacura plugin documentation in order to interact with the RDF store and constraint manager')
		    ]).

dacura_schema_update(Request) :-
    % Get current stdout 
    current_output(Out),
    http_log_stream(Log),

    %http_read_data(Request,Data,[]),
    %nb_setval(http_post_data, read),
    %format(Log,'POST Data: ~p\n', [Data]),
    %member(update=Update_String,Data),
    %format(Log,'Update: ~p\n', [Update_String]),
    
    http_parameters(Request, [], [form_data(Data)]), 
    
    format('Content-type: application/json~n~n'), 

    % use pragma from client
    getKey(pragma, Data, Pragma_String, '{"tests": "all", "instance": "instance", "schema":"schema"}'),
    getKey(update, Data, Update_String, '[]'), 

    %write(Log, "Pragma_String:\n"),
    %write_canonical(Log, Pragma_String), write(Log, "\n"),

    %write(Log, "Update_String:\n"),
    %write_canonical(Log, Update_String), write(Log, "\n"),
    
    atom_json_term(Pragma_String, json(Pragma), []),
    atom_json_term(Update_String, json(Update), []),
    
    getKey(inserts, Update, InsertsPreLiteral, []),
    getKey(deletes, Update, DeletesPreLiteral, []),

    %write(Log,'Inserts:\n'),
    %write_canonical(Log,InsertsPreLiteral),
    %write(Log,'\n'),
    
    convert_quads(InsertsPreLiteral, Inserts),
    %write(Log, 'Made it (A)!!!!'),

    %write(Log, 'Made it (B)!!!!'),
    convert_quads(DeletesPreLiteral, Deletes),
    %write(Log, 'Made it (B)!!!!'),
    %Deletes=[],
    
    Delta=[inserts=Inserts, deletes=Deletes],

    %rdf_transaction(
    runSchemaUpdate(Delta, Pragma, Witnesses),
    %),
    
    write_canonical(Log, Witnesses),
    
    % Witnesses=Delta,
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
    convert_quads(InsertsPreLiteral, Inserts),
    convert_quads(DeletesPreLiteral, Deletes),
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
    
    runFullValidation(Pragma, Witnesses),

    json_write(Out,Witnesses).


dacura_schema_validate(Request) :- 
    http_parameters(Request, [], [form_data(Data)]), 

    format('Content-type: application/json~n~n'), 

    % Get current stdout 
    current_output(Out), 

    % use pragma from client
    getKey(pragma, Data, Pragma_String, '{"tests": "all", "schema":"schema"}'),
    
    atom_json_term(Pragma_String, json(Pragma), []),
    
    %rdf_transaction(
    runSchemaValidation(Pragma, Witnesses),
		       %),

    json_write(Out,Witnesses).


dacura_test(_Request) :- 
    %http_parameters(Request, [], [form_data(Data)]), 

    format('Content-type: application/json~n~n'), 

    % Get current stdout 
    current_output(Out), 

    % Get current stdout
    runTests(Witnesses),
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
