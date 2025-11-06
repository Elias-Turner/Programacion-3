%% Base de conocimiento generada automáticamente

movie('Star Wars', ['sci-fi','adventure']).
movie('The Expanse', ['sci-fi','drama']).
movie('Interstellar', ['sci-fi','drama']).
movie('The Office', ['comedy']).
movie('Parks and Recreation', ['comedy']).
movie('Inception', ['sci-fi','thriller']).
movie('The Witcher', ['fantasy','adventure']).
movie('Lord of the Rings', ['fantasy','adventure']).
movie('Breaking Bad', ['drama','crime']).

watched('alice','Star Wars').
watched('alice','The Office').
watched('alice','Inception').
watched('bob','The Witcher').
watched('bob','Lord of the Rings').
watched('carla','The Expanse').
watched('carla','Breaking Bad').

likes('alice','comedy').
likes('alice','sci-fi').
likes('alice','thriller').
likes('alice','adventure').
likes('bob','fantasy').
likes('bob','adventure').
likes('carla','sci-fi').
likes('carla','drama').
likes('carla','crime').

% Reglas de recomendación:
% 1) Recomendar película que comparta un género que le guste al usuario y que no haya visto.
recommend(U,Title) :- movie(Title, Genres), likes(U,G), member(G, Genres), \+ watched(U,Title).

% 2) Recomendar películas que compartan género con alguna vista por el usuario (fallback).
recommend(U,Title) :- movie(Title, Genres), watched(U,Seen), movie(Seen, SeenGenres), member(G2, SeenGenres), member(G2, Genres), \+ watched(U,Title).

% member/2 está disponible en SWI-Prolog; si no, podríamos definirla aquí.
