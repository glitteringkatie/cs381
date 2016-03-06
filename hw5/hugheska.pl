% Here are a bunch of facts describing the Simpson's family tree.
% Don't change them!

female(mona).
female(jackie).
female(marge).
female(patty).
female(selma).
female(lisa).
female(maggie).
female(ling).

male(abe).
male(clancy).
male(herb).
male(homer).
male(bart).

married_(abe,mona).
married_(clancy,jackie).
married_(homer,marge).

married(X,Y) :- married_(X,Y).
married(X,Y) :- married_(Y,X).

parent(abe,herb).
parent(abe,homer).
parent(mona,homer).

parent(clancy,marge).
parent(jackie,marge).
parent(clancy,patty).
parent(jackie,patty).
parent(clancy,selma).
parent(jackie,selma).

parent(homer,bart).
parent(marge,bart).
parent(homer,lisa).
parent(marge,lisa).
parent(homer,maggie).
parent(marge,maggie).

parent(selma,ling).



%%
% Part 1. Family relations
%%

% 1. Define a predicate `child/2` that inverts the parent relationship.

child(X,Y) :- parent(Y,X).

% 2. Define two predicates `isMother/1` and `isFather/1`.

isMother(X) :- female(X), parent(X,_).
isFather(X) :- male(X), parent(X,_).

% 3. Define a predicate `grandparent/2`.

grandparent(X,Y) :- parent(X,P), parent(P,Y).

% 4. Define a predicate `sibling/2`. Siblings share at least one parent.

sibling(X,Y) :- parent(P,X), parent(P,Y), X \= Y.

% 5. Define two predicates `brother/2` and `sister/2`.

sister(X,Y) :- female(X), sibling(X,Y).
brother(Y,X) :- male(Y), sibling(Y,X).
% lol get it because X and Y chromosomes so I flipped the variables
% I'm hilarious


% 6. Define a predicate `siblingInLaw/2`. A sibling-in-law is either married to
%    a sibling or the sibling of a spouse.

siblingInLaw_(X,Y) :- married(X,P), sibling(P,Y).

siblingInLaw(X,Y) :- siblingInLaw_(Y,X).
siblingInLaw(X,Y) :- siblingInLaw_(X,Y).

% 7. Define two predicates `aunt/2` and `uncle/2`. Your definitions of these
%    predicates should include aunts and uncles by marriage.

aunt(X,Y) :- sister(X,P), parent(P,Y).
aunt(X,Y) :- female(X), married(X,P1), sibling(P1,P2), parent(P2,Y).

uncle(X,Y) :- brother(X,P), parent(P,Y).
uncle(X,Y) :- male(X), married(X,P1), sibling(P1,P2), parent(P2,Y).


% gender neutral aunt/uncle for use of cousin
auncle(X,Y) :- aunt(X,Y).
auncle(X,Y) :- uncle(X,Y).

% 8. Define the predicate `cousin/2`.

cousin(X,Y) :- child(X,P), auncle(P,Y).

% 9. Define the predicate `ancestor/2`.

ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(X,P), ancestor(P,Y).

% Extra credit: Define the predicate `related/2`.



%%
% Part 2. Language implementation (see course web page)
%%

