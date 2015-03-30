# [ClioPatria](http://cliopatria.swi-prolog.org) cpack dacura -- Instance and Schema Constraints

The Dacura cpack for ClioPatria exposes a REST API which is intended to 
allow both instance updates and schema migration exposing a transactional 
interface with pre and post conditions. 

  - We expose an update API for schema and instance data (S&I).
  - We expose an update API for instance data (I).
  - We test constraints over (S&I) and rollback on failure with reporting. 
  - We test constraints over (I) and rollback on failure with reporting. 
