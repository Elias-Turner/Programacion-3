:- discontiguous movie/2.
:- discontiguous movie_age/2.
:- discontiguous movie_countries/2.
:- discontiguous serieG/2.
:- discontiguous serieE/2.
:- discontiguous serieP/2.
:- dynamic likes/2, dislikes/2, user_age/2, user_country/2.

% catalogoHas2.pl - Base de datos de ejemplo con edad mínima, países, director y puntuación
% Formatos:
%   movie(Title, GenresList, RatingFloat, Director).
%   movie_age(Title, MinAge).
%   movie_countries(Title, CountriesList).
%
%   serie(Title, GenresList, RatingFloat, Director).
%   serie_age(Title, MinAge).
%   serie_countries(Title, CountriesList).

% Directivas para evitar warnings si las cláusulas quedan separadas
:- discontiguous movie/4.
:- discontiguous movie_age/2.
:- discontiguous movie_countries/2.
:- discontiguous serie/4.
:- discontiguous serie_age/2.
:- discontiguous serie_countries/2.

% Los hechos dinámicos que Python inyectará
:- dynamic likes/2, dislikes/2, user_age/2, user_country/2.

% -------------------------
% Películas (title, genres, rating, director)
% -------------------------
movie('Star Wars', ['sci-fi','adventure'], 8.6, 'George Lucas').
movie_age('Star Wars', 7).
movie_countries('Star Wars', ['argentina','usa','canada','uk']).

movie('The Expanse', ['sci-fi','drama'], 8.5, 'James A. Sullivan').
movie_age('The Expanse', 13).
movie_countries('The Expanse', ['usa','canada','uk']).

movie('Interstellar', ['sci-fi','drama'], 8.6, 'Christopher Nolan').
movie_age('Interstellar', 10).
movie_countries('Interstellar', ['usa','argentina','uk']).

movie('The Office', ['comedia'], 8.9, 'Various').
movie_age('The Office', 13).
movie_countries('The Office', ['usa','uk']).

movie('Parks and Recreation', ['comedia'], 8.6, 'Various').
movie_age('Parks and Recreation', 13).
movie_countries('Parks and Recreation', ['usa','uk']).

movie('Inception', ['sci-fi','thriller'], 8.8, 'Christopher Nolan').
movie_age('Inception', 13).
movie_countries('Inception', ['usa','canada','uk','argentina']).

movie('The Witcher', ['fantasy','adventure'], 8.2, 'Alik Sakharov').
movie_age('The Witcher', 16).
movie_countries('The Witcher', ['uk','usa','poland']).

movie('Lord of the Rings', ['fantasy','adventure'], 8.9, 'Peter Jackson').
movie_age('Lord of the Rings', 10).
movie_countries('Lord of the Rings', ['uk','usa','canada','argentina']).

movie('Breaking Bad', ['drama','crime'], 9.5, 'Vince Gilligan').
movie_age('Breaking Bad', 18).
movie_countries('Breaking Bad', ['usa','uk','canada']).

movie('Blade Runner 2049', ['sci-fi','drama','thriller'], 8.0, 'Denis Villeneuve').
movie_age('Blade Runner 2049', 16).
movie_countries('Blade Runner 2049', ['usa','uk','canada']).

movie('Mad Max: Fury Road', ['sci-fi','adventure','thriller','action'], 8.1, 'George Miller').
movie_age('Mad Max: Fury Road', 15).
movie_countries('Mad Max: Fury Road', ['usa','australia','uk']).

movie('Dune', ['sci-fi','fantasy','adventure','drama'], 8.2, 'Denis Villeneuve').
movie_age('Dune', 13).
movie_countries('Dune', ['usa','canada','uk','argentina']).

movie('Pulp Fiction', ['crime','drama','thriller'], 8.9, 'Quentin Tarantino').
movie_age('Pulp Fiction', 18).
movie_countries('Pulp Fiction', ['usa','uk','france']).

movie('Alien', ['sci-fi','horror','thriller'], 8.4, 'Ridley Scott').
movie_age('Alien', 16).
movie_countries('Alien', ['usa','uk','canada']).

movie('Spirited Away', ['fantasy','adventure','animation'], 8.6, 'Hayao Miyazaki').
movie_age('Spirited Away', 7).
movie_countries('Spirited Away', ['japan','usa','uk','argentina']).

movie('Parasite', ['drama','thriller','crime'], 8.6, 'Bong Joon-ho').
movie_age('Parasite', 16).
movie_countries('Parasite', ['south_korea','usa','france','argentina']).

movie('Eternal Sunshine', ['sci-fi','romance','drama'], 8.3, 'Michel Gondry').
movie_age('Eternal Sunshine', 13).
movie_countries('Eternal Sunshine', ['usa','uk','france']).

movie('The Grand Budapest Hotel', ['comedia','adventure','drama'], 8.1, 'Wes Anderson').
movie_age('The Grand Budapest Hotel', 13).
movie_countries('The Grand Budapest Hotel', ['usa','uk','france']).

movie('Arrival', ['sci-fi','drama','mystery'], 7.9, 'Denis Villeneuve').
movie_age('Arrival', 10).
movie_countries('Arrival', ['usa','uk','canada']).

movie('Knives Out', ['mystery','comedia','crime'], 7.9, 'Rian Johnson').
movie_age('Knives Out', 13).
movie_countries('Knives Out', ['usa','uk','canada','argentina']).

movie('The Matrix', ['sci-fi','action','thriller'], 8.7, 'The Wachowskis').
movie_age('The Matrix', 13).
movie_countries('The Matrix', ['usa','uk','canada']).

movie('The Dark Knight', ['action','crime','drama'], 9.0, 'Christopher Nolan').
movie_age('The Dark Knight', 13).
movie_countries('The Dark Knight', ['usa','uk','canada']).

movie('Crouching Tiger', ['action','fantasy','romance'], 7.8, 'Ang Lee').
movie_age('Crouching Tiger', 12).
movie_countries('Crouching Tiger', ['china','usa','uk']).

movie('Django Unchained', ['western','drama'], 8.4, 'Quentin Tarantino').
movie_age('Django Unchained', 16).
movie_countries('Django Unchained', ['usa','uk']).

movie('Whiplash', ['drama','music'], 8.5, 'Damien Chazelle').
movie_age('Whiplash', 13).
movie_countries('Whiplash', ['usa','uk','canada']).

movie('Shaun of the Dead', ['comedia','horror'], 7.9, 'Edgar Wright').
movie_age('Shaun of the Dead', 13).
movie_countries('Shaun of the Dead', ['uk','usa']).

movie('No Country for Old Men', ['crime','drama','thriller'], 8.1, 'Joel Coen, Ethan Coen').
movie_age('No Country for Old Men', 16).
movie_countries('No Country for Old Men', ['usa','uk']).

movie('Finding Nemo', ['adventure','animation','comedia'], 8.1, 'Andrew Stanton').
movie_age('Finding Nemo', 0).
movie_countries('Finding Nemo', ['usa','uk','canada','argentina']).

movie('Harry Potter 7', ['fantasy','adventure','thriller'], 7.7, 'David Yates').
movie_age('Harry Potter 7', 10).
movie_countries('Harry Potter 7', ['uk','usa','argentina','canada']).

movie('A Quiet Place', ['horror','sci-fi','thriller'], 7.5, 'John Krasinski').
movie_age('A Quiet Place', 13).
movie_countries('A Quiet Place', ['usa','uk','canada']).

movie('Gladiator', ['action','drama','adventure'], 8.5, 'Ridley Scott').
movie_age('Gladiator', 13).
movie_countries('Gladiator', ['usa','uk','italy']).

movie('Sherlock', ['mystery','crime','drama'], 9.1, 'Paul McGuigan').
movie_age('Sherlock', 13).
movie_countries('Sherlock', ['uk','usa']).

movie('Toy Story', ['animation'], 8.3, 'John Lasseter').
movie_age('Toy Story', 0).
movie_countries('Toy Story', ['usa','uk','canada','argentina']).

% -------------------------
% Series (title, genres, rating, director)
% -------------------------
serie('The Big Bang Theory', ['sci-fi','comedia'], 8.1, 'Various').
serie_age('The Big Bang Theory', 10).
serie_countries('The Big Bang Theory', ['argentina','usa','canada','uk']).

serie('Stranger Things', ['sci-fi','horror','drama'], 8.8, 'The Duffer Brothers').
serie_age('Stranger Things', 13).
serie_countries('Stranger Things', ['usa','uk','canada','argentina','france']).

serie('The Expanse (Series)', ['sci-fi','drama'], 8.5, 'Various').
serie_age('The Expanse (Series)', 13).
serie_countries('The Expanse (Series)', ['usa','canada','uk']).

serie('Breaking Bad (Series)', ['drama','crime'], 9.5, 'Vince Gilligan').
serie_age('Breaking Bad (Series)', 18).
serie_countries('Breaking Bad (Series)', ['usa','uk','canada']).

serie('Sherlock (Series)', ['mystery','crime','drama'], 9.1, 'Various').
serie_age('Sherlock (Series)', 13).
serie_countries('Sherlock (Series)', ['uk','usa']).

serie('The Witcher (Series)', ['fantasy','adventure'], 8.2, 'Various').
serie_age('The Witcher (Series)', 16).
serie_countries('The Witcher (Series)', ['uk','usa','poland']).

serie('Black Mirror', ['sci-fi','drama','mystery'], 8.8, 'Charlie Brooker').
serie_age('Black Mirror', 16).
serie_countries('Black Mirror', ['uk','usa','canada']).

serie('Friends', ['comedia','drama'], 8.9, 'Various').
serie_age('Friends', 13).
serie_countries('Friends', ['usa','uk','canada','argentina']).

serie('Parks and Recreation (Series)', ['comedia'], 8.6, 'Various').
serie_age('Parks and Recreation (Series)', 13).
serie_countries('Parks and Recreation (Series)', ['usa','uk']).

% -------------------------
% Reglas de recomendación
% -------------------------
% recommend(User, Title, Director, Rating, Genres)
% La regla selecciona items que:
%  - Tienen al menos un género liked por el usuario
%  - No contienen ningún género disliked por el usuario
%  - La edad mínima de la obra <= edad del usuario
%  - El país del usuario está en la lista de países disponibles

% Películas (usa movie/4, movie_age/2, movie_countries/2)
recommend(U, Title, Director, Rating, Genres) :-
    movie(Title, Genres, Rating, Director),
    likes(U, LikedGenre),
    member(LikedGenre, Genres),
    \+ (dislikes(U, DislikedGenre), member(DislikedGenre, Genres)),
    ( movie_age(Title, MinAge) -> true ; MinAge = 0 ),
    user_age(U, UserAge),
    UserAge >= MinAge,
    user_country(U, UserCountry),
    ( movie_countries(Title, Countries) -> true ; Countries = [] ),
    member(UserCountry, Countries).

% Series (usa serie/4, serie_age/2, serie_countries/2)
recommend_serie(U, Title, Director, Rating, Genres) :-
    serie(Title, Genres, Rating, Director),
    likes(U, LikedGenre),
    member(LikedGenre, Genres),
    \+ (dislikes(U, DislikedGenre), member(DislikedGenre, Genres)),
    ( serie_age(Title, MinAge) -> true ; MinAge = 0 ),
    user_age(U, UserAge),
    UserAge >= MinAge,
    user_country(U, UserCountry),
    ( serie_countries(Title, Countries) -> true ; Countries = [] ),
    member(UserCountry, Countries).

% También se proporciona recommend/2 y recommend_serie/2 de fallback que solo devuelven Title
% (esto facilita llamadas desde Python que esperan solo títulos)
recommend(U, Title) :-
    recommend(U, Title, _Director, _Rating, _Genres).

recommend_serie(U, Title) :-
    recommend_serie(U, Title, _Director, _Rating, _Genres).

% -------------------------
%% --- START USER FACTS INJECTION ---
%% --- GUSTOS DE USUARIOS (Acumulados en la sesión) ---
likes('eliasturner','mystery').
likes('eliasturner','comedia').
likes('eliasturner','action').
likes('eliasturner','thriller').
likes('eliasturner','fantasy').
likes('eliasturner','adventure').
likes('eliasturner','crime').
likes('eliasturner','sci-fi').

%% --- DISGUSTOS DE USUARIOS ---
dislikes('eliasturner','drama').
dislikes('eliasturner','romance').
dislikes('eliasturner','horror').
dislikes('eliasturner','animation').

%% --- METADATOS DE USUARIOS ---
user_age('eliasturner',20).
user_country('eliasturner','argentina').

%% --- END USER FACTS INJECTION ---


