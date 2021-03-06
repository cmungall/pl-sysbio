/* -*- Mode: Prolog -*- */

/**
  */

:- module(sbtext,
          [
           write_model/0,
           write_model/1
           ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module('../biopax_util').
:- use_module('../lego_ns').
:- use_module('../ro').

write_model :-
        % find all parents
        collect_part_parents(Ws),
        debug(render,' ws=~w',[Ws]),
        member(W,Ws),
        debug(render,' testing ~w',[W]),
        % only write roots or singletons
        \+ part_of(W,_),
        write_model(W),
        fail.
write_model.

write_model(P):-
        debug(render,'Writing ~w',[P]),
        phrase(proc(P),Toks),
        concat_atom(Toks,'',A),
        writeln(A).

collect_part_parents(Ws) :-
        setof(W,P^part_of(P,W),Ws),
        !.
collect_part_parents(Ws) :-
        % none found - try this instead
        setof(W,C^occurs_in(W,C),Ws).

        

proc(P) --> parents(P,D), {Dplus1 is D+1},occ(P,Dplus1), children(P,Dplus1),!.
proc(P) --> {throw(proc(P))}.

pproc(P,D) --> parents(P,D), occ(P,D), !.
pproc(P) --> {throw(pproc(P))}.

parents(P,Dplus1) --> {part_of(P,W)},!,pproc(W,D),{Dplus1 is D+1}.
parents(P,1) --> {\+ part_of(P,_)},!,occ(P,1).
parents(P,1) --> {throw(parents(P,1))}.

children(P,D) --> {Dplus1 is D+1},{setof(C,part_of(C,P),Cs)},!,lchildren(Cs,Dplus1).
children(_,_) --> [].

lchildren([],_) --> [].
lchildren([C|Cs],D) --> occ(C,D),children(C,D),lchildren(Cs,D).

occ(P,D) --> {debug(sbtext,' OCC ~w',[P])}, nl, indent(D), [' [ '], e(P),[' ] '],!.
occ(P,D) --> {throw(occ(P,D))}.


e(P) --> label(P),[' '],types(P),props(P),!.
e(P) --> ['?'],label(P),[' '],props(P),!.
e(P) --> {throw(e(P))}.


label(P) --> short(P),[' '],{rdf_has(P,rdfs:label,Lit),lit2atom(Lit,Label)},!,[Label].
label(P) --> short(P).

short(P) --> {rdf_global_id(_:ID,P)},!,[ID].
short(P) --> {concat_atom([_,ID],'#',P)},!,[ID].
short(P) --> [P].


labels([]) --> [].
labels([H|T]) --> label(H),[' '],labels(T).

props(P) --> w_enabled_by(P), w_occurs_in(P), w_io(P), w_next(P).

w_enabled_by(P) --> {enabled_by(P,G)},!,[' enabled by:'],mol(G),[' '].
w_enabled_by(P) --> {svf(P,enabled_by:'', G)},!,[' enabled by:'],label(G),[' '].
w_enabled_by(_) --> [].

w_occurs_in(P) --> {occurs_in(P,G)},!,[' occurs_in:'],types(G),[' '].
w_occurs_in(P) --> {svf(P,occurs_in:'', G)},!,[' occurs in:'],label(G),[' '].
w_occurs_in(_) --> [].

w_next(P) --> {directly_provides_input_for(P,Q)},!,[' --> '],label(Q),[' '].
w_next(_) --> [].

w_io(P) --> ['{'],w_input(P),['->'],w_output(P),['}'].

w_input(P) --> {has_input(P,G)},!,types(G),[' '].
w_input(P) --> {svf(P,has_input:'',G)},!,label(G),[' '].
w_input(_) --> [].
w_output(P) --> {has_output(P,G)},!,types(G),[' '].
w_output(P) --> {svf(P,has_output:'',G)},!,label(G),[' '].
w_output(_) --> [].


mol(M) --> types(M).

types(M) --> {individual_ftypes(M,Ts)},labels(Ts),!.
types(M) --> {throw(types(M))}.


indent(D) --> {D =< 1},!.
indent(D) --> {D>1, Dminus1 is D-1},['   '],indent(Dminus1).
nl --> [' ;\n'].

individual_ftypes(I,Types) :-
        setof(T,
              individual_ftype(I,T),
              Types),
        !.
individual_ftypes(_,[]).

        
individual_ftype(I,T) :-
        rdf(I,rdf:type,T),
        \+ filtered(T).

filtered(T) :- atom_concat('http://www.biopax.org',_,T).
filtered(T) :- atom_concat('http://www.w3.org',_,T).
filtered(T) :- atom_concat('__file',_,T).
filtered(T) :- rdf_is_bnode(T).

lit2atom(literal(type(_,A)),A).
lit2atom(literal(A),A) :- atom(A).






