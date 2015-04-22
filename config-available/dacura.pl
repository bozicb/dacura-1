:- module(conf_dacura, []).

/** <module> Dacura REST RDF API
*/

:- use_module(applications(dacura)).
:- use_module(cliopatria(hooks)).

cliopatria:menu_popup_order(dacura, 110). 
cliopatria:menu_item(100=dacura/dacura_validate, 'Validate Schema').
