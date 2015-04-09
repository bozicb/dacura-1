:- module(dacura_app,
	  [
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
%:- use_module(library(yui3_beta)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
% :- use_module(library(skos/util)).
:- use_module(schemaRules). 

http:location(dacura, '/dacura', []).

:- http_handler(dacura(.), dacura_reply, []). 
:- http_handler(dacura(schema), dacura_schema_update, []). 
:- http_handler(dacura(instance), dacura_instance_update, []). 

:- use_module(library(http/json_convert)). 

:- use_module(utils). 

dacura_reply(_Request) :- 
    reply_html_page(cliopatria(default), 
		    [ title(['This is a test'])
		    ], 
		    [  h2('This is the root directory for the dacura API.'), 
		       p('Please read the information included in the dacura plugin documentation in order to interact with the RDF store and constraint manager')
		    ]).


dacura_schema_update(Request) :- 
    http_parameters(Request, [], [form_data(Data)]), 

    format('Content-type: application/json~n~n'), 

    % Get current stdout 
    current_output(Out), 

    %format('Content-type: text/html~n~n'), 
    %write('testing testing, one two three'), 

    % use pragma from client
    getKey(pragma, Data, Pragma_String, []),
    getKey(update, Data, Update_String, ''), 

    atom_json_term(Pragma_String, json(Pragma), []),
    atom_json_term(Update_String, Update, []),
    
    runSchemaUpdate(Update, Pragma, Witnesses),
    	
    json_write(Out,Witnesses).

    
dacura_instance_update(Request) :- 
    http_parameters(Request, [], [form_data(Data)]), 

    % use pragma from client
    getKey(pragma, Data, Pragma_String, []),
    getKey(update, Data, Update_String, ''), 
    
    atom_json_term(Pragma_String, json(_Pragma), []),
    atom_json_term(Update_String, _Update, []),

    % Get current stdout 
    current_output(Out), 
    
    runInstanceUpdate(Update, Pragma, Witnesses), 
    Witnesses = [],

    format('Content-type: application/json~n~n'), 

    json_write(Out,Witnesses).
