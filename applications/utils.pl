:- module(utils,[getKey/4]).

% convenience function
getKey(Key,Assoc,Val,Default) :- 
    member(Key=Val, Assoc) *-> 
	  true 
    ; Val = Default.
 
