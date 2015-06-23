# [ClioPatria](http://cliopatria.swi-prolog.org) cpack dacura -- Dacura REST RDF API

Version 1.0, Date: 8/6/2015

The API we use for constraint checking inserts/updates/deletes involves a segregation of Schema-Instance Updates and Instance Updates. 

The reasoning behind this is that full constraint checking is currently necessary for changes to the schema (though this is not in fact completely necessary and future implementations might hone this down to only checking necessary constraints), while instance updates only require local checking, are liable to be more frequent and must have better performance.

A reference example for checking is provided in the dacura repository: 

ssh://gavin@tcdfame2.scss.tcd.ie:22/var/git/dacura/

Under services/testInstanceUpdates.py

The format of inserts/deletes

Currently we send updates as a JSON format which is specifically adapted to represent RDF triples.  (Should we use JSON-LD for this purpose?  It seems a bit heavy weight for what we need).

Resources
---------

A resource in our format is simply described as a string, so for instance, the following string represents the “label” property

“http://www.w3.org/1999/02/22-rdf-syntax-ns#label”

Literals
--------

Literals are composite objects which can not be represented directly as a string.  The format for a literal is formated as one of the two: 

`{‘data’ : “2015-06-08T12:30:00”, 
 ‘type’ : “http://www.w3.org/2001/XMLSchema#dateTime”}`

or 

`{‘data’ : “This is a string” 
 ‘lang’ : “en”}`

Graph
-----

Currently we only work with two graphs.  This will be changed in the future but it requires parameterising the instance checking code.  The two graph names are: “instance” and “schema”.

Triples
-------

Triples are described as lists of strings or JSON representations of literals

`[“resource1”, ”resource2”,”resource3”, “graph”]`
`[“resource4”, “resource5”, {“data” : “Hello world”, “lang” : “en-utf8”}, “graph”]`
`[“resource6”, “resource7”, 
 {“data” : “2015-06-08T12:30:00”, “type” : “http://www.w3.org/2001/XMLSchema#dateTime”},     “graph”]`

Instance Updates
----------------

Instance updates can be done by firing a post request to:

http://tcdfame2:3020/dacura/instance

with the “pragma” and “update” POST variables.  The pragma has two name-value pairs, one for “tests” the other for “commit”.   The “tests” pragma gives a JSON list of which tests from ICV and ontology consistency to perform, each one being named with a string.  Currently the complete  list is: 

“checkInstanceClass”, “checkPropertyRange”, “checkPropertyDomain”

Alternatively, one can simply specify “all” to run all constraints.

“commit” status is either “true” which yields an immediate commit if constraints are not violated and “false” which does not commit but returns either an empty list for no constraint errors, or a list of constraint violations and their witnessing information. 

The “update” post variable carries a JSON object describing the inserts and deletes that are required with the following format: 

update: `{‘insert’ : TRIPLES, 
              ‘delete’ : TRIPLES}`

Example:

pragma: `{‘tests’ : ‘all’, 
                ‘commit’ : ‘true’}`
update: `{‘insert’ :  
              [[“resource1”, ”resource2”,”resource3”, “instance”]
               [“resource6”, “resource7”, 
                              {“data” : “2015-06-08T12:30:00”, “type” :
			      “http://www.w3.org/2001/XMLSchema#dateTime”},
			      “instance”]]}`


Schema-Instance Updates
-----------------------

Schema-Instance updates can be done by firing a post request to:

http://tcdfame2:3020/dacura/schema

with the “pragma” and “update” POST variables.  The pragma has two name-value pairs, one for “tests” the other for “commit”.   The “tests” pragma gives a JSON list of which tests from ICV and ontology consistency to perform, each one being named with a string.  Currently the complete  list is: 

"classCycles", "propertyCycles",  "duplicateClasses",  "duplicateProperties",  "orphanSubClasses",  "orphanSubProperties",  "orphanInstance",  "orphanProperties",  "blankNode",  "invalidRange",  "invalidDomain",  "invalidInstanceRange",  "invalidInstanceDomain"

As well as all of the tests above for instance level constraints, or one can simply specify “all” instead of a JSON list to run all constraints. 

“pragma” and “update” variables are as described above for instance updates.

