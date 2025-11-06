%% --- BASE DE DATOS DE PELÍCULAS (Mantenido en Prolog) ---

movie('Star Wars', ['sci-fi','adventure']).
movie('The Expanse', ['sci-fi','drama']).
movie('Interstellar', ['sci-fi','drama']).
movie('The Office', ['comedy']).
movie('Parks and Recreation', ['comedy']).
movie('Inception', ['sci-fi','thriller']).
movie('The Witcher', ['fantasy','adventure']).
movie('Lord of the Rings', ['fantasy','adventure']).
movie('Breaking Bad', ['drama','crime']).
movie('Blade Runner 2049', ['sci-fi','drama','thriller']).
movie('Mad Max: Fury Road', ['sci-fi','adventure','thriller','action']).
movie('Dune', ['sci-fi','fantasy','adventure','drama']).
movie('Pulp Fiction', ['crime','drama','thriller']).
movie('Alien', ['sci-fi','horror','thriller']).
movie('Spirited Away', ['fantasy','adventure','animation']).
movie('Parasite', ['drama','thriller','crime']).
movie('Eternal Sunshine', ['sci-fi','romance','drama']).
movie('The Grand Budapest Hotel', ['comedy','adventure','drama']).
movie('Arrival', ['sci-fi','drama','mystery']).
movie('Knives Out', ['mystery','comedy','crime']).
movie('The Matrix', ['sci-fi','action','thriller']).
movie('The Dark Knight', ['action','crime','drama']).
movie('Crouching Tiger', ['action','fantasy','romance']).
movie('Django Unchained', ['western','drama']).
movie('Whiplash', ['drama','music']).
movie('Shaun of the Dead', ['comedy','horror']).
movie('No Country for Old Men', ['crime','drama','thriller']).
movie('Finding Nemo', ['adventure','animation','comedy']).
movie('Harry Potter 7', ['fantasy','adventure','thriller']).
movie('A Quiet Place', ['horror','sci-fi','thriller']).
movie('Gladiator', ['action','drama','adventure']).
movie('Sherlock', ['mystery','crime','drama']).

%% --- REGLAS DE RECOMENDACIÓN ---

% Regla de recomendación: SIN DISLIKES + AL MENOS 1 LIKE
recommend(U,Title) :-
    movie(Title, MovieGenres),
    % FILTRO DE EXCLUSIÓN: \+ (Existe un DislikedGenre que es miembro de MovieGenres)
    \+ (
        dislikes(U, DislikedGenre),
        member(DislikedGenre, MovieGenres)
    ),
    % FILTRO DE INCLUSIÓN: (Existe un LikedGenre que es miembro de MovieGenres)
    (
        likes(U, LikedGenre),
        member(LikedGenre, MovieGenres)
    ).
