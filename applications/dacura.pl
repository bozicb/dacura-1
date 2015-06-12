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
    getKey(pragma, Data, Pragma_String, '{"tests":"all"}'),
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
    getKey(pragma, Data, Pragma_String, '{"tests": "all"}'),
    getKey(update, Data, Update_String, '{"update" : []}'), 
    
    atom_json_term(Pragma_String, json(Pragma), []),
    atom_json_term(Update_String, json(Update), []),
    
    getKey(inserts, Update, InsertsPreLiteral, []),
    getKey(deletes, Update, DeletesPreLiteral, []),
    convert_triples(InsertsPreLiteral, Inserts),
    convert_triples(DeletesPreLiteral, Deletes),
    
    Delta=[inserts=Inserts, deletes=Deletes],

    rdf_transaction(runInstanceUpdate(Delta, Pragma, Witnesses)),

    json_write(Out,Witnesses).


dacura_validate(Request) :- 
    http_parameters(Request, [], [form_data(Data)]), 

    format('Content-type: application/json~n~n'), 

    % Get current stdout 
    current_output(Out), 

    % use pragma from client
    getKey(pragma, Data, Pragma_String, '{"tests":"all"}'),

    atom_json_term(Pragma_String, json(Pragma), []),
    
    rdf_transaction(runFullValidation(Pragma, Witnesses)),

    json_write(Out,Witnesses).
    
