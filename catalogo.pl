% catalogo.pl - Base de datos de ejemplo con edad minima, países, director, puntuación y temporadas (para series)

:- discontiguous movie/4.
:- discontiguous movie_age/2.
:- discontiguous movie_countries/2.
:- discontiguous serie/5.
:- discontiguous serie_age/2.
:- discontiguous serie_countries/2.

:- dynamic likes/2, dislikes/2, user_age/2, user_country/2.

% -------------------------
% Películas (title, genres, rating, director) - 100 PELÍCULAS TOTALES
% -------------------------
% --- Películas Originales (con países extendidos) ---
movie('Star Wars', ['sci-fi','adventure'], 8.6, 'George Lucas').
movie_age('Star Wars', 7).
movie_countries('Star Wars', ['argentina','usa','canada','uk','germany','brazil','spain','australia','japan','mexico','italy','china']).

movie('The Expanse', ['sci-fi','drama'], 8.5, 'James A. Sullivan').
movie_age('The Expanse', 13).
movie_countries('The Expanse', ['usa','canada','uk','germany','france','brazil','spain','japan','mexico','india']).

movie('Interstellar', ['sci-fi','drama'], 8.6, 'Christopher Nolan').
movie_age('Interstellar', 10).
movie_countries('Interstellar', ['usa','argentina','uk','japan','australia','south_korea','italy','sweden','norway','finland']).

movie('Parks and Recreation', ['comedia'], 8.6, 'Various').
movie_age('Parks and Recreation', 13).
movie_countries('Parks and Recreation', ['usa','uk','canada','germany','france','brazil','mexico','spain']).

movie('Inception', ['sci-fi','thriller'], 8.8, 'Christopher Nolan').
movie_age('Inception', 13).
movie_countries('Inception', ['usa','canada','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

movie('The Witcher', ['fantasy','adventure'], 8.2, 'Alik Sakharov').
movie_age('The Witcher', 16).
movie_countries('The Witcher', ['uk','usa','poland','germany','france','brazil','spain','japan','mexico','india']).

movie('Lord of the Rings', ['fantasy','adventure'], 8.9, 'Peter Jackson').
movie_age('Lord of the Rings', 10).
movie_countries('Lord of the Rings', ['uk','usa','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

% --- ERROR CORREGIDO: Se añadió la edad a 'Blade Runner 2049' y se corrigió la sintaxis de la línea 45 original.
movie('Blade Runner 2049', ['sci-fi','drama','thriller'], 8.0, 'Denis Villeneuve').
movie_age('Blade Runner 2049', 16).
movie_countries('Blade Runner 2049', ['usa','uk','canada','germany','france','brazil','spain','australia','japan','mexico']).

movie('Mad Max: Fury Road', ['sci-fi','adventure','thriller','action'], 8.1, 'George Miller').
movie_age('Mad Max: Fury Road', 15).
movie_countries('Mad Max: Fury Road', ['usa','australia','uk','germany','france','brazil','spain','japan','mexico','india']).

movie('Dune', ['sci-fi','fantasy','adventure','drama'], 8.2, 'Denis Villeneuve').
movie_age('Dune', 13).
movie_countries('Dune', ['usa','canada','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

movie('Pulp Fiction', ['crime','drama','thriller'], 8.9, 'Quentin Tarantino').
movie_age('Pulp Fiction', 18).
movie_countries('Pulp Fiction', ['usa','uk','france','argentina','germany','brazil','spain','japan']).

movie('Alien', ['sci-fi','horror','thriller'], 8.4, 'Ridley Scott').
movie_age('Alien', 16).
movie_countries('Alien', ['usa','uk','canada','germany','france','brazil','spain','australia']).

movie('Spirited Away', ['fantasy','adventure','animation'], 8.6, 'Hayao Miyazaki').
movie_age('Spirited Away', 7).
movie_countries('Spirited Away', ['japan','usa','uk','argentina','germany','france','brazil','spain','china','south_korea']).

movie('Parasite', ['drama','thriller','crime'], 8.6, 'Bong Joon-ho').
movie_age('Parasite', 16).
movie_countries('Parasite', ['south_korea','usa','france','argentina','germany','brazil','spain','japan']).

movie('Eternal Sunshine', ['sci-fi','romance','drama'], 8.3, 'Michel Gondry').
movie_age('Eternal Sunshine', 13).
movie_countries('Eternal Sunshine', ['usa','uk','france','argentina','germany','brazil','spain','japan']).

movie('The Grand Budapest Hotel', ['comedia','adventure','drama'], 8.1, 'Wes Anderson').
movie_age('The Grand Budapest Hotel', 13).
movie_countries('The Grand Budapest Hotel', ['usa','uk','france','argentina','germany','brazil','spain','japan','mexico']).

movie('Arrival', ['sci-fi','drama','mystery'], 7.9, 'Denis Villeneuve').
movie_age('Arrival', 10).
movie_countries('Arrival', ['usa','uk','canada','argentina','germany','france','brazil','spain']).

movie('Knives Out', ['mystery','comedia','crime'], 7.9, 'Rian Johnson').
movie_age('Knives Out', 13).
movie_countries('Knives Out', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

movie('The Matrix', ['sci-fi','action','thriller'], 8.7, 'The Wachowskis').
movie_age('The Matrix', 13).
movie_countries('The Matrix', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

movie('The Dark Knight', ['action','crime','drama'], 9.0, 'Christopher Nolan').
movie_age('The Dark Knight', 13).
movie_countries('The Dark Knight', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

movie('Crouching Tiger', ['action','fantasy','romance'], 7.8, 'Ang Lee').
movie_age('Crouching Tiger', 12).
movie_countries('Crouching Tiger', ['china','usa','uk','argentina','germany','france','brazil','spain','japan']).

movie('Django Unchained', ['western','drama'], 8.4, 'Quentin Tarantino').
movie_age('Django Unchained', 16).
movie_countries('Django Unchained', ['usa','uk','argentina','germany','france','brazil','spain','japan','mexico']).

movie('Whiplash', ['drama','music'], 8.5, 'Damien Chazelle').
movie_age('Whiplash', 13).
movie_countries('Whiplash', ['usa','uk','canada','argentina','germany','france','brazil','spain','mexico']).

movie('Shaun of the Dead', ['comedia','horror'], 7.9, 'Edgar Wright').
movie_age('Shaun of the Dead', 13).
movie_countries('Shaun of the Dead', ['uk','usa','argentina','germany','france','brazil','spain','japan']).

movie('No Country for Old Men', ['crime','drama','thriller'], 8.1, 'Joel Coen, Ethan Coen').
movie_age('No Country for Old Men', 16).
movie_countries('No Country for Old Men', ['usa','uk','argentina','germany','france','brazil','spain','japan']).

movie('Finding Nemo', ['adventure','animation','comedia'], 8.1, 'Andrew Stanton').
movie_age('Finding Nemo', 0).
movie_countries('Finding Nemo', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

movie('Harry Potter 7', ['fantasy','adventure','thriller'], 7.7, 'David Yates').
movie_age('Harry Potter 7', 10).
movie_countries('Harry Potter 7', ['uk','usa','argentina','canada','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

movie('A Quiet Place', ['horror','sci-fi','thriller'], 7.5, 'John Krasinski').
movie_age('A Quiet Place', 13).
movie_countries('A Quiet Place', ['usa','uk','canada','argentina','germany','france','brazil','spain']).

movie('Gladiator', ['action','drama','adventure'], 8.5, 'Ridley Scott').
movie_age('Gladiator', 13).
movie_countries('Gladiator', ['usa','uk','italy','argentina','germany','france','brazil','spain','australia']).

movie('Sherlock', ['mystery','crime','drama'], 9.1, 'Paul McGuigan').
movie_age('Sherlock', 13).
movie_countries('Sherlock', ['uk','usa','argentina','germany','france','brazil','spain','japan']).

movie('Toy Story', ['animation'], 8.3, 'John Lasseter').
movie_age('Toy Story', 0).
movie_countries('Toy Story', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

% --- 12 Películas con Género Único (Condición 1) ---

movie('2001: A Space Odyssey', ['sci-fi'], 8.3, 'Stanley Kubrick').
movie_age('2001: A Space Odyssey', 13).
movie_countries('2001: A Space Odyssey', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china','india','south_korea','sweden','norway','finland','poland','russia']).

movie('The Revenant', ['adventure'], 8.0, 'Alejandro G. Iñárritu').
movie_age('The Revenant', 16).
movie_countries('The Revenant', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico']).

movie('Monty Python', ['comedia'], 8.2, 'Terry Gilliam, Terry Jones').
movie_age('Monty Python', 13).
movie_countries('Monty Python', ['uk','usa','canada','argentina','germany','france','brazil','spain','australia','japan']).

movie('Misery', ['thriller'], 7.6, 'Rob Reiner').
movie_age('Misery', 16).
movie_countries('Misery', ['usa','uk','canada','argentina','germany','france','brazil','spain','mexico']).

movie('Get Out', ['horror'], 7.7, 'Jordan Peele').
movie_age('Get Out', 16).
movie_countries('Get Out', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

movie('Fight Club', ['drama'], 8.8, 'David Fincher').
movie_age('Fight Club', 18).
movie_countries('Fight Club', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china','india','south_korea']).

movie('Akira', ['animation'], 8.0, 'Katsuhiro Otomo').
movie_age('Akira', 16).
movie_countries('Akira', ['japan','usa','uk','argentina','germany','france','brazil','spain','australia','mexico','italy']).

movie('The Silence of the Lambs', ['crime'], 8.6, 'Jonathan Demme').
movie_age('The Silence of the Lambs', 18).
movie_countries('The Silence of the Lambs', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

movie('Amelie', ['romance'], 8.3, 'Jean-Pierre Jeunet').
movie_age('Amelie', 13).
movie_countries('Amelie', ['france','usa','uk','argentina','germany','brazil','spain','japan','mexico']).

movie('Seven', ['mystery'], 8.6, 'David Fincher').
movie_age('Seven', 18).
movie_countries('Seven', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

movie('300', ['action'], 7.6, 'Zack Snyder').
movie_age('300', 16).
movie_countries('300', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

movie('Pan’s Labyrinth', ['fantasy'], 8.2, 'Guillermo del Toro').
movie_age('Pan’s Labyrinth', 16).
movie_countries('Pan’s Labyrinth', ['spain','usa','uk','argentina','mexico','germany','france','brazil','italy']).

% --- Películas Adicionales (para llegar a 100) ---

movie('The Godfather', ['crime','drama'], 9.2, 'Francis Ford Coppola').
movie_age('The Godfather', 18).
movie_countries('The Godfather', ['usa','italy','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','china','india','south_korea','sweden','norway','finland','poland','russia','portugal','belgium','netherlands']).

movie('Casablanca', ['romance','drama','war'], 8.5, 'Michael Curtiz').
movie_age('Casablanca', 10).
movie_countries('Casablanca', ['usa','uk','france','argentina','germany','brazil','spain','japan','mexico','italy','china','india']).

movie('The Pianist', ['drama','war','biography'], 8.5, 'Roman Polanski').
movie_age('The Pianist', 16).
movie_countries('The Pianist', ['france','poland','uk','argentina','germany','usa','brazil','spain','australia','japan','mexico','italy']).

movie('Indiana Jones: Raiders', ['adventure','action'], 8.4, 'Steven Spielberg').
movie_age('Indiana Jones: Raiders', 7).
movie_countries('Indiana Jones: Raiders', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china','india','south_korea','sweden','norway','finland','poland','russia','portugal','belgium','netherlands','egypt','greece','turkey','thailand','vietnam','indonesia','chile']).

movie('Jurassic Park', ['sci-fi','adventure'], 8.1, 'Steven Spielberg').
movie_age('Jurassic Park', 10).
movie_countries('Jurassic Park', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

movie('Back to the Future', ['sci-fi','adventure','comedia'], 8.5, 'Robert Zemeckis').
movie_age('Back to the Future', 7).
movie_countries('Back to the Future', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china','india','south_korea','sweden','norway','finland','poland']).

movie('Leon: The Professional', ['crime','drama','thriller'], 8.5, 'Luc Besson').
movie_age('Leon: The Professional', 18).
movie_countries('Leon: The Professional', ['france','usa','uk','argentina','germany','brazil','spain','japan','mexico','italy']).

movie('Inglourious Basterds', ['adventure','drama','war'], 8.3, 'Quentin Tarantino').
movie_age('Inglourious Basterds', 18).
movie_countries('Inglourious Basterds', ['usa','germany','france','argentina','uk','brazil','spain','japan','mexico','italy','china']).

movie('Reservoir Dogs', ['crime','thriller'], 8.3, 'Quentin Tarantino').
movie_age('Reservoir Dogs', 18).
movie_countries('Reservoir Dogs', ['usa','uk','argentina','germany','france','brazil','spain','japan','mexico']).

movie('Oldboy', ['action','drama','mystery'], 8.4, 'Park Chan-wook').
movie_age('Oldboy', 18).
movie_countries('Oldboy', ['south_korea','usa','uk','argentina','germany','france','brazil','spain','japan','mexico','italy']).

movie('Trainspotting', ['drama','comedia'], 8.1, 'Danny Boyle').
movie_age('Trainspotting', 18).
movie_countries('Trainspotting', ['uk','usa','argentina','germany','france','brazil','spain','mexico']).

movie('Amadeus', ['biography','drama','music'], 8.4, 'Milos Forman').
movie_age('Amadeus', 13).
movie_countries('Amadeus', ['usa','uk','argentina','germany','france','brazil','spain','japan']).

movie('Ratatouille', ['animation','comedia','family'], 8.0, 'Brad Bird').
movie_age('Ratatouille', 0).
movie_countries('Ratatouille', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china','india']).

movie('Prestige', ['drama','mystery','sci-fi'], 8.5, 'Christopher Nolan').
movie_age('Prestige', 13).
movie_countries('Prestige', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

movie('Eternal Sunshine of the Spotless Mind', ['romance','sci-fi','drama'], 8.3, 'Michel Gondry').
movie_age('Eternal Sunshine of the Spotless Mind', 13).
movie_countries('Eternal Sunshine of the Spotless Mind', ['usa','uk','france','argentina','germany','brazil','spain','japan','mexico','italy']).

movie('V for Vendetta', ['action','drama','sci-fi'], 8.2, 'James McTeigue').
movie_age('V for Vendetta', 16).
movie_countries('V for Vendetta', ['uk','usa','germany','france','argentina','brazil','spain','japan','mexico','italy']).

movie('Goodfellas', ['biography','crime','drama'], 8.7, 'Martin Scorsese').
movie_age('Goodfellas', 18).
movie_countries('Goodfellas', ['usa','uk','argentina','germany','france','brazil','spain','japan','mexico','italy']).

movie('Requiem for a Dream', ['drama'], 8.3, 'Darren Aronofsky').
movie_age('Requiem for a Dream', 18).
movie_countries('Requiem for a Dream', ['usa','uk','argentina','germany','france','brazil','spain','japan','mexico']).

movie('The Green Mile', ['crime','drama','fantasy'], 8.6, 'Frank Darabont').
movie_age('The Green Mile', 16).
movie_countries('The Green Mile', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

movie('Forrest Gump', ['drama','romance'], 8.8, 'Robert Zemeckis').
movie_age('Forrest Gump', 13).
movie_countries('Forrest Gump', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china','india','south_korea','sweden','norway','finland','poland','russia','portugal','belgium','netherlands']).

movie('Spiderman: Into the Spiderverse', ['animation','action','adventure'], 8.4, 'Various').
movie_age('Spiderman: Into the Spiderverse', 7).
movie_countries('Spiderman: Into the Spiderverse', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

movie('Little Miss Sunshine', ['comedia','drama'], 7.8, 'Jonathan Dayton, Valerie Faris').
movie_age('Little Miss Sunshine', 13).
movie_countries('Little Miss Sunshine', ['usa','uk','argentina','germany','france','brazil','spain','mexico']).

movie('Donnie Darko', ['sci-fi','drama','mystery'], 8.0, 'Richard Kelly').
movie_age('Donnie Darko', 16).
movie_countries('Donnie Darko', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico']).

movie('The Martian', ['adventure','drama','sci-fi'], 8.0, 'Ridley Scott').
movie_age('The Martian', 13).
movie_countries('The Martian', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

movie('A Silent Voice', ['animation','drama','romance'], 8.2, 'Naoko Yamada').
movie_age('A Silent Voice', 13).
movie_countries('A Silent Voice', ['japan','usa','uk','argentina','germany','france','brazil','spain','australia','mexico']).

movie('Prisoners', ['crime','drama','mystery'], 8.1, 'Denis Villeneuve').
movie_age('Prisoners', 16).
movie_countries('Prisoners', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

movie('Room', ['drama'], 8.1, 'Lenny Abrahamson').
movie_age('Room', 16).
movie_countries('Room', ['ireland','canada','uk','usa','argentina','germany','france','brazil','spain']).

movie('Drive', ['action','crime','drama'], 7.8, 'Nicolas Winding Refn').
movie_age('Drive', 16).
movie_countries('Drive', ['usa','uk','argentina','germany','france','brazil','spain','japan']).

movie('Arrival (2024)', ['sci-fi','drama','mystery'], 7.9, 'Denis Villeneuve').
movie_age('Arrival (2024)', 10).
movie_countries('Arrival (2024)', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan']).

movie('Get Shorty', ['comedia','crime'], 7.5, 'Barry Sonnenfeld').
movie_age('Get Shorty', 16).
movie_countries('Get Shorty', ['usa','uk','argentina','germany','france','brazil','spain','mexico']).

movie('Rush', ['action','biography','drama'], 8.1, 'Ron Howard').
movie_age('Rush', 13).
movie_countries('Rush', ['uk','germany','usa','argentina','france','brazil','spain','australia','japan']).

% --- ERROR CORREGIDO: Uso de comillas simples dobles ('' ) para el apóstrofe en el título.
movie('Singin'' in the Rain', ['comedia','musical','romance'], 8.3, 'Stanley Donen, Gene Kelly').
movie_age('Singin'' in the Rain', 0).
movie_countries('Singin'' in the Rain', ['usa','uk','argentina','germany','france','brazil','spain','japan']).

movie('Fargo', ['crime','drama','thriller'], 8.1, 'Joel Coen, Ethan Coen').
movie_age('Fargo', 18).
movie_countries('Fargo', ['usa','uk','argentina','germany','france','brazil','spain','japan','mexico']).

movie('Moonlight', ['drama'], 7.4, 'Barry Jenkins').
movie_age('Moonlight', 16).
movie_countries('Moonlight', ['usa','argentina','uk','germany','france','brazil','spain','japan']).

movie('Her', ['drama','romance','sci-fi'], 8.0, 'Spike Jonze').
movie_age('Her', 16).
movie_countries('Her', ['usa','argentina','uk','germany','france','brazil','spain','japan']).

movie('Blade Runner', ['sci-fi','thriller'], 8.1, 'Ridley Scott').
movie_age('Blade Runner', 16).
movie_countries('Blade Runner', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico']).

movie('Apocalypse Now', ['drama','war'], 8.4, 'Francis Ford Coppola').
movie_age('Apocalypse Now', 18).
movie_countries('Apocalypse Now', ['usa','argentina','uk','germany','france','brazil','spain','japan','mexico','italy','china']).

movie('E.T. the Extra-Terrestrial', ['sci-fi','family'], 7.8, 'Steven Spielberg').
movie_age('E.T. the Extra-Terrestrial', 7).
movie_countries('E.T. the Extra-Terrestrial', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

movie('The Social Network', ['biography','drama'], 7.7, 'David Fincher').
movie_age('The Social Network', 13).
movie_countries('The Social Network', ['usa','argentina','uk','germany','france','brazil','spain','japan']).

movie('Almost Famous', ['adventure','comedia','drama'], 7.9, 'Cameron Crowe').
movie_age('Almost Famous', 16).
movie_countries('Almost Famous', ['usa','argentina','uk','germany','france','brazil','spain','japan']).

movie('Boyhood', ['drama'], 7.9, 'Richard Linklater').
movie_age('Boyhood', 13).
movie_countries('Boyhood', ['usa','argentina','uk','germany','france','brazil','spain','mexico']).

movie('Inside Out', ['animation','adventure','comedia'], 8.1, 'Pete Docter, Ronnie Del Carmen').
movie_age('Inside Out', 0).
movie_countries('Inside Out', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china','india']).

movie('The Wolf of Wall Street', ['biography','comedia','crime'], 8.2, 'Martin Scorsese').
movie_age('The Wolf of Wall Street', 18).
movie_countries('The Wolf of Wall Street', ['usa','argentina','uk','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

movie('Mad Max', ['action','sci-fi'], 6.9, 'George Miller').
movie_age('Mad Max', 16).
movie_countries('Mad Max', ['australia','usa','uk','argentina','germany','france','brazil','spain']).

movie('A Star Is Born', ['drama','music','romance'], 7.6, 'Bradley Cooper').
movie_age('A Star Is Born', 16).
movie_countries('A Star Is Born', ['usa','argentina','uk','germany','france','brazil','spain','australia','japan','mexico']).

movie('Coco', ['animation','adventure','family'], 8.4, 'Lee Unkrich, Adrian Molina').
movie_age('Coco', 0).
movie_countries('Coco', ['usa','mexico','uk','canada','argentina','germany','france','brazil','spain','australia','japan','italy','china','india','south_korea','sweden']).

movie('The Shape of Water', ['adventure','drama','fantasy'], 7.3, 'Guillermo del Toro').
movie_age('The Shape of Water', 16).
movie_countries('The Shape of Water', ['usa','argentina','uk','canada','germany','france','brazil','spain','mexico']).

movie('Birdman', ['comedia','drama'], 7.7, 'Alejandro G. Iñárritu').
movie_age('Birdman', 16).
movie_countries('Birdman', ['usa','argentina','uk','germany','france','brazil','spain','japan']).

movie('Whiplash (2025)', ['drama','music'], 8.5, 'Damien Chazelle').
movie_age('Whiplash (2025)', 13).
movie_countries('Whiplash (2025)', ['usa','uk','canada','argentina','germany','france','brazil','spain','mexico','italy']).

movie('The Father', ['drama'], 8.2, 'Florian Zeller').
movie_age('The Father', 13).
movie_countries('The Father', ['uk','france','argentina','usa','germany','brazil','spain','japan']).

movie('Life Is Beautiful', ['comedia','drama','war'], 8.6, 'Roberto Benigni').
movie_age('Life Is Beautiful', 10).
movie_countries('Life Is Beautiful', ['italy','usa','uk','argentina','germany','france','brazil','spain','japan','mexico']).

movie('La La Land', ['comedia','drama','music','romance'], 7.9, 'Damien Chazelle').
movie_age('La La Land', 13).
movie_countries('La La Land', ['usa','argentina','uk','germany','france','brazil','spain','australia','japan','mexico','italy']).

movie('Joker', ['crime','drama','thriller'], 8.4, 'Todd Phillips').
movie_age('Joker', 18).
movie_countries('Joker', ['usa','argentina','uk','canada','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

movie('Once Upon a Time in Hollywood', ['comedia','drama'], 7.6, 'Quentin Tarantino').
movie_age('Once Upon a Time in Hollywood', 18).
movie_countries('Once Upon a Time in Hollywood', ['usa','argentina','uk','germany','france','brazil','spain','australia','japan','mexico','italy']).

movie('The Hateful Eight', ['crime','drama','mystery'], 7.8, 'Quentin Tarantino').
movie_age('The Hateful Eight', 18).
movie_countries('The Hateful Eight', ['usa','argentina','uk','germany','france','brazil','spain','japan','mexico','italy']).

movie('Drive (2025)', ['action','crime','drama'], 7.8, 'Nicolas Winding Refn').
movie_age('Drive (2025)', 16).
movie_countries('Drive (2025)', ['usa','uk','argentina','germany','france','brazil','spain','japan','mexico','italy']).

movie('Rush Hour', ['action','comedia','crime'], 7.0, 'Brett Ratner').
movie_age('Rush Hour', 13).
movie_countries('Rush Hour', ['usa','china','uk','argentina','germany','france','brazil','spain','mexico']).

movie('Minority Report', ['action','mystery','sci-fi'], 7.6, 'Steven Spielberg').
movie_age('Minority Report', 13).
movie_countries('Minority Report', ['usa','uk','canada','argentina','germany','france','brazil','spain','japan']).

movie('Catch Me If You Can', ['biography','crime','drama'], 8.1, 'Steven Spielberg').
movie_age('Catch Me If You Can', 13).
movie_countries('Catch Me If You Can', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).
movie('Donnie Brasco', ['biography','crime','drama'], 7.7, 'Mike Newell').
movie_age('Donnie Brasco', 18).
movie_countries('Donnie Brasco', ['usa','uk','argentina','germany','france','brazil','spain','japan']).
movie('The Big Lebowski', ['comedia','crime','sport'], 8.1, 'Joel Coen, Ethan Coen').
movie_age('The Big Lebowski', 18).
movie_countries('The Big Lebowski', ['usa','uk','argentina','germany','france','brazil','spain','japan','mexico']).
movie('Eternal Sunshine of the Spotless Mind (2024)', ['romance','sci-fi','drama'], 8.3, 'Michel Gondry').
movie_age('Eternal Sunshine of the Spotless Mind (2024)', 13).
movie_countries('Eternal Sunshine of the Spotless Mind (2024)', ['usa','uk','france','argentina','germany','brazil','spain','japan','mexico']).
movie('District 9', ['action','sci-fi','thriller'], 7.9, 'Neill Blomkamp').
movie_age('District 9', 16).
movie_countries('District 9', ['south_africa','usa','uk','argentina','germany','france','brazil','spain','australia','japan']).
movie('Arrival (2024)', ['sci-fi','drama','mystery'], 7.9, 'Denis Villeneuve').
movie_age('Arrival (2024)', 10).
movie_countries('Arrival (2024)', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).
movie('Baby Driver', ['action','crime','music'], 7.6, 'Edgar Wright').
movie_age('Baby Driver', 16).
movie_countries('Baby Driver', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico']).
movie('Gone Girl', ['drama','mystery','thriller'], 8.1, 'David Fincher').
movie_age('Gone Girl', 18).
movie_countries('Gone Girl', ['usa','argentina','uk','germany','france','brazil','spain','australia','japan','mexico','italy','china']).
movie('Spotlight', ['biography','crime','drama'], 8.1, 'Tom McCarthy').
movie_age('Spotlight', 13).
movie_countries('Spotlight', ['usa','argentina','uk','canada','germany','france','brazil','spain']).
movie('La Haine', ['crime','drama'], 8.1, 'Mathieu Kassovitz').
movie_age('La Haine', 18).
movie_countries('La Haine', ['france','argentina','uk','germany','brazil','spain','japan','mexico']).
movie('Tropa de Elite', ['action','crime','drama'], 8.1, 'José Padilha').
movie_age('Tropa de Elite', 18).
movie_countries('Tropa de Elite', ['brazil','argentina','usa','uk','germany','france','spain','mexico']).
movie('City of God', ['crime','drama'], 8.6, 'Fernando Meirelles, Kátia Lund').
movie_age('City of God', 18).
movie_countries('City of God', ['brazil','usa','uk','argentina','germany','france','spain','japan','mexico']).
movie('The Host', ['action','horror','sci-fi'], 7.1, 'Bong Joon-ho').
movie_age('The Host', 16).
movie_countries('The Host', ['south_korea','usa','uk','argentina','germany','france','brazil','spain','japan']).
movie('The Raid', ['action','thriller'], 8.1, 'Gareth Evans').
movie_age('The Raid', 18).
movie_countries('The Raid', ['indonesia','usa','uk','argentina','germany','france','brazil','spain']).
movie('Amelie', ['romance'], 8.3, 'Jean-Pierre Jeunet').
movie_age('Amelie', 13).
movie_countries('Amelie', ['france','usa','uk','argentina','germany','brazil','spain','japan','mexico']).
movie('Run Lola Run', ['action','crime','thriller'], 7.7, 'Tom Tykwer').
movie_age('Run Lola Run', 16).
movie_countries('Run Lola Run', ['germany','usa','uk','argentina','france','brazil','spain','japan']).
movie('El Secreto de Sus Ojos', ['drama','mystery','thriller'], 8.2, 'Juan José Campanella').
movie_age('El Secreto de Sus Ojos', 18).
movie_countries('El Secreto de Sus Ojos', ['argentina','spain','usa','uk','germany','france','brazil','mexico']).
movie('Nine Queens', ['crime','drama','thriller'], 7.9, 'Fabián Bielinsky').
movie_age('Nine Queens', 13).
movie_countries('Nine Queens', ['argentina','spain','usa','uk','germany','france','brazil','mexico']).
movie('Y Tu Mamá También', ['adventure','drama'], 7.6, 'Alfonso Cuarón').
movie_age('Y Tu Mamá También', 18).
movie_countries('Y Tu Mamá También', ['mexico','usa','uk','argentina','germany','france','brazil','spain']).
movie('The Tourist', ['action','thriller','romance'], 6.0, 'Florian Henckel von Donnersmarck').
movie_age('The Tourist', 13).
movie_countries('The Tourist', ['usa','france','uk','argentina','germany','brazil','spain','italy']).
movie('Tears of the Sun', ['action','drama','thriller'], 6.6, 'Antoine Fuqua').
movie_age('Tears of the Sun', 16).
movie_countries('Tears of the Sun', ['usa','uk','argentina','germany','france','brazil','spain','japan','mexico']).
movie('The Mask of Zorro', ['action','adventure','romance'], 6.7, 'Martin Campbell').
movie_age('The Mask of Zorro', 10).
movie_countries('The Mask of Zorro', ['usa','mexico','uk','argentina','germany','france','brazil','spain','australia','japan']).
movie('Sleepy Hollow', ['fantasy','horror','mystery'], 7.3, 'Tim Burton').
movie_age('Sleepy Hollow', 16).
movie_countries('Sleepy Hollow', ['usa','uk','argentina','germany','france','brazil','spain','japan','mexico']).
movie('Seven Pounds', ['drama'], 7.6, 'Gabriele Muccino').
movie_age('Seven Pounds', 13).
movie_countries('Seven Pounds', ['usa','argentina','uk','germany','france','brazil','spain','australia','japan','mexico','italy']).
movie('World War Z', ['action','horror','sci-fi'], 7.0, 'Marc Forster').
movie_age('World War Z', 13).
movie_countries('World War Z', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).
movie('The Da Vinci Code', ['mystery','thriller'], 6.6, 'Ron Howard').
movie_age('The Da Vinci Code', 13).
movie_countries('The Da Vinci Code', ['usa','france','uk','argentina','germany','brazil','spain','japan','mexico','italy']).
movie('Angels & Demons', ['mystery','thriller'], 6.7, 'Ron Howard').
movie_age('Angels & Demons', 13).
movie_countries('Angels & Demons', ['usa','italy','uk','argentina','germany','france','brazil','spain','japan','mexico']).
movie('Inferno', ['action','mystery','thriller'], 6.2, 'Ron Howard').
movie_age('Inferno', 13).
movie_countries('Inferno', ['usa','italy','uk','argentina','germany','france','brazil','spain','japan','mexico','china']).
movie('The Grand Budapest Hotel (2025)', ['comedia','adventure','drama'], 8.1, 'Wes Anderson').
movie_age('The Grand Budapest Hotel (2025)', 13).
movie_countries('The Grand Budapest Hotel (2025)', ['usa','uk','france','argentina','germany','brazil','spain','japan','mexico','italy','china']).
movie('Isle of Dogs', ['animation','adventure','comedia'], 7.9, 'Wes Anderson').
movie_age('Isle of Dogs', 7).
movie_countries('Isle of Dogs', ['usa','japan','uk','argentina','germany','france','brazil','spain','australia','mexico']).
movie('Fantastic Mr. Fox', ['animation','adventure','comedia'], 7.9, 'Wes Anderson').
movie_age('Fantastic Mr. Fox', 7).
movie_countries('Fantastic Mr. Fox', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico']).
movie('Eternal Sunshine (2025)', ['sci-fi','romance','drama'], 8.3, 'Michel Gondry').
movie_age('Eternal Sunshine (2025)', 13).
movie_countries('Eternal Sunshine (2025)', ['usa','uk','france','argentina','germany','brazil','spain','japan','mexico','italy','china']).
movie('The Dark Knight Rises', ['action','crime','drama'], 8.4, 'Christopher Nolan').
movie_age('The Dark Knight Rises', 13).
movie_countries('The Dark Knight Rises', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).
movie('Batman Begins', ['action','adventure'], 8.2, 'Christopher Nolan').
movie_age('Batman Begins', 13).
movie_countries('Batman Begins', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).
movie('Dunkirk', ['action','drama','history'], 7.8, 'Christopher Nolan').
movie_age('Dunkirk', 13).
movie_countries('Dunkirk', ['uk','france','usa','argentina','germany','brazil','spain','australia','japan','mexico','italy','china']).
movie('Shutter Island', ['mystery','thriller'], 8.2, 'Martin Scorsese').
movie_age('Shutter Island', 18).
movie_countries('Shutter Island', ['usa','argentina','uk','germany','france','brazil','spain','australia','japan','mexico','italy']).
movie('The Departed', ['crime','drama','thriller'], 8.5, 'Martin Scorsese').
movie_age('The Departed', 18).
movie_countries('The Departed', ['usa','argentina','uk','germany','france','brazil','spain','australia','japan','mexico','italy','china']).
movie('Gangs of New York', ['crime','drama'], 7.5, 'Martin Scorsese').
movie_age('Gangs of New York', 18).
movie_countries('Gangs of New York', ['usa','italy','uk','argentina','germany','france','brazil','spain','japan','mexico']).
movie('Zodiac', ['crime','drama','mystery'], 7.7, 'David Fincher').
movie_age('Zodiac', 16).
movie_countries('Zodiac', ['usa','argentina','uk','germany','france','brazil','spain','australia','japan','mexico','italy']).
movie('Fight Club (2024)', ['drama'], 8.8, 'David Fincher').
movie_age('Fight Club (2024)', 18).
movie_countries('Fight Club (2024)', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china','india']).
movie('The Girl with the Dragon Tattoo', ['crime','drama','mystery'], 7.8, 'David Fincher').
movie_age('The Girl with the Dragon Tattoo', 18).
movie_countries('The Girl with the Dragon Tattoo', ['usa','sweden','uk','argentina','germany','france','brazil','spain','japan','mexico']).
movie('Catching Fire', ['action','adventure','sci-fi'], 7.5, 'Francis Lawrence').
movie_age('Catching Fire', 13).
movie_countries('Catching Fire', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).
movie('The Hunger Games', ['action','adventure','sci-fi'], 7.2, 'Gary Ross').
movie_age('The Hunger Games', 13).
movie_countries('The Hunger Games', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).
movie('Star Trek', ['action','adventure','sci-fi'], 7.9, 'J.J. Abrams').
movie_age('Star Trek', 13).
movie_countries('Star Trek', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).
movie('Finding Dory', ['animation','adventure','comedia'], 7.3, 'Andrew Stanton').
movie_age('Finding Dory', 0).
movie_countries('Finding Dory', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china','india']).
movie('Monsters, Inc.', ['animation','adventure','comedia'], 8.1, 'Pete Docter, David Silverman').
movie_age('Monsters, Inc.', 0).
movie_countries('Monsters, Inc.', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china','india']).
movie('Up', ['animation','adventure','comedia'], 8.2, 'Pete Docter, Bob Peterson').
movie_age('Up', 0).
movie_countries('Up', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china','india']).
movie('The Incredibles', ['animation','action','adventure'], 8.0, 'Brad Bird').
movie_age('The Incredibles', 7).
movie_countries('The Incredibles', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).
movie('WALL-E', ['animation','adventure','family','sci-fi'], 8.4, 'Andrew Stanton').
movie_age('WALL-E', 0).
movie_countries('WALL-E', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china','india','south_korea','sweden','norway','finland','poland','russia','portugal','belgium','netherlands','egypt','greece','turkey','thailand','vietnam','indonesia','chile','south_africa','new_zealand','ireland']).

% -------------------------
% Series (title, genres, rating, director, seasons) - 100 SERIES TOTALES
% -------------------------
% --- Series Originales (Países ajustados) ---
serie('The Big Bang Theory', ['sci-fi','comedia'], 8.1, 'Various', 12).
serie_age('The Big Bang Theory', 10).
serie_countries('The Big Bang Theory', ['argentina','usa','canada','uk','germany','brazil','spain','japan','mexico','italy']).

serie('Stranger Things', ['sci-fi','horror','drama'], 8.8, 'The Duffer Brothers', 5).
serie_age('Stranger Things', 13).
serie_countries('Stranger Things', ['usa','uk','canada','argentina','france','germany','brazil','spain','australia','japan','mexico','italy','china']).

serie('The Expanse', ['sci-fi','drama'], 8.5, 'Various', 6).
serie_age('The Expanse', 13).
serie_countries('The Expanse', ['usa','canada','uk','germany','france','brazil','spain','japan','mexico','india']).

serie('Breaking Bad', ['drama','crime'], 9.5, 'Vince Gilligan', 5).
serie_age('Breaking Bad', 18).
serie_countries('Breaking Bad', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('Sherlock', ['mystery','crime','drama'], 9.1, 'Various', 4).
serie_age('Sherlock', 13).
serie_countries('Sherlock', ['uk','usa','argentina','germany','france','brazil','spain','japan','mexico']).

serie('The Witcher', ['fantasy','adventure'], 8.2, 'Various', 2).
serie_age('The Witcher', 16).
serie_countries('The Witcher', ['uk','usa','poland','germany','france','brazil','spain','japan','mexico','india']).

serie('Black Mirror', ['sci-fi','drama','mystery'], 8.8, 'Charlie Brooker', 5).
serie_age('Black Mirror', 16).
serie_countries('Black Mirror', ['uk','usa','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Friends', ['comedia','drama'], 8.9, 'Various', 10).
serie_age('Friends', 13).
serie_countries('Friends', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('Parks and Recreation', ['comedia'], 8.6, 'Various', 7).
serie_age('Parks and Recreation', 13).
serie_countries('Parks and Recreation', ['usa','uk','argentina','germany','france','brazil','spain','mexico']).

serie('The Office', ['comedia'], 8.9, 'Various', 9).
serie_age('The Office', 13).
serie_countries('The Office', ['usa','uk','argentina','germany','france','brazil','spain','mexico']).

% --- 12 Series con Género Único (Condición 1) ---

serie('Star Trek: Discovery', ['sci-fi'], 7.2, 'Various', 5).
serie_age('Star Trek: Discovery', 13).
serie_countries('Star Trek: Discovery', ['usa','canada','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china','india','south_korea','sweden']).

serie('Fleabag', ['comedia'], 8.7, 'Harry Bradbeer', 2).
serie_age('Fleabag', 16).
serie_countries('Fleabag', ['uk','usa','argentina','germany','france','brazil','spain','japan','mexico']).

serie('The Crown', ['drama'], 8.7, 'Various', 6).
serie_age('The Crown', 13).
serie_countries('The Crown', ['uk','usa','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Mindhunter', ['crime'], 8.6, 'David Fincher', 2).
serie_age('Mindhunter', 18).
serie_countries('Mindhunter', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('Broadchurch', ['mystery'], 8.4, 'Various', 3).
serie_age('Broadchurch', 13).
serie_countries('Broadchurch', ['uk','usa','argentina','germany','france','brazil','spain','australia','japan']).

serie('The Mandalorian', ['adventure'], 8.7, 'Various', 3).
serie_age('The Mandalorian', 10).
serie_countries('The Mandalorian', ['usa','canada','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china','india']).

serie('Luther', ['thriller'], 8.4, 'Various', 5).
serie_age('Luther', 18).
serie_countries('Luther', ['uk','usa','argentina','germany','france','brazil','spain','japan','mexico']).

serie('The Haunting of Hill House', ['horror'], 8.6, 'Mike Flanagan', 1).
serie_age('The Haunting of Hill House', 16).
serie_countries('The Haunting of Hill House', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Arcane', ['animation'], 9.0, 'Various', 1).
serie_age('Arcane', 13).
serie_countries('Arcane', ['usa','france','uk','canada','argentina','germany','brazil','spain','australia','japan','mexico','italy','china','south_korea']).

serie('Jack Ryan', ['action'], 8.1, 'Various', 4).
serie_age('Jack Ryan', 16).
serie_countries('Jack Ryan', ['usa','canada','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Outlander', ['romance'], 8.4, 'Various', 7).
serie_age('Outlander', 16).
serie_countries('Outlander', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('The Terror', ['fantasy'], 7.9, 'Various', 2).
serie_age('The Terror', 16).
serie_countries('The Terror', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).


% --- Series Adicionales (para llegar a 100) ---
serie('Game of Thrones', ['fantasy','adventure','drama'], 9.2, 'Various', 8).
serie_age('Game of Thrones', 18).
serie_countries('Game of Thrones', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china','india','south_korea','sweden','norway','finland','poland']).

serie('The Sopranos', ['crime','drama'], 9.2, 'Various', 6).
serie_age('The Sopranos', 18).
serie_countries('The Sopranos', ['usa','uk','argentina','germany','france','brazil','spain','japan','mexico','italy']).

serie('The Wire', ['crime','drama','thriller'], 9.3, 'Various', 5).
serie_age('The Wire', 18).
serie_countries('The Wire', ['usa','uk','argentina','germany','france','brazil','spain','japan','mexico']).

serie('Peaky Blinders', ['crime','drama'], 8.8, 'Various', 6).
serie_age('Peaky Blinders', 18).
serie_countries('Peaky Blinders', ['uk','usa','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Chernobyl', ['drama','history'], 9.4, 'Johan Renck', 1).
serie_age('Chernobyl', 16).
serie_countries('Chernobyl', ['usa','uk','argentina','germany','france','brazil','spain','japan','mexico']).

serie('Narcos', ['biography','crime','drama'], 8.8, 'Various', 3).
serie_age('Narcos', 18).
serie_countries('Narcos', ['usa','colombia','argentina','germany','france','brazil','spain','mexico','chile']).

serie('Money Heist', ['action','crime','thriller'], 8.2, 'Various', 5).
serie_age('Money Heist', 16).
serie_countries('Money Heist', ['spain','usa','uk','argentina','germany','france','brazil','mexico','italy','china','india','south_korea']).

serie('Dark', ['sci-fi','thriller','mystery'], 8.7, 'Various', 3).
serie_age('Dark', 16).
serie_countries('Dark', ['germany','usa','uk','argentina','france','brazil','spain','japan','mexico','italy']).

serie('Westworld', ['sci-fi','mystery','drama'], 8.5, 'Various', 4).
serie_age('Westworld', 18).
serie_countries('Westworld', ['usa','canada','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('House of Cards', ['drama','thriller'], 8.7, 'Various', 6).
serie_age('House of Cards', 18).
serie_countries('House of Cards', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('The Queen’s Gambit', ['drama'], 8.6, 'Scott Frank', 1).
serie_age('The Queen’s Gambit', 16).
serie_countries('The Queen’s Gambit', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Downton Abbey', ['drama','romance'], 8.7, 'Various', 6).
serie_age('Downton Abbey', 13).
serie_countries('Downton Abbey', ['uk','usa','canada','argentina','germany','france','brazil','spain','australia','japan']).

serie('Ted Lasso', ['comedia','sport'], 8.8, 'Various', 3).
serie_age('Ted Lasso', 10).
serie_countries('Ted Lasso', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Ozark', ['crime','drama','thriller'], 8.5, 'Various', 4).
serie_age('Ozark', 18).
serie_countries('Ozark', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Squid Game', ['action','drama','thriller'], 8.0, 'Hwang Dong-hyuk', 1).
serie_age('Squid Game', 18).
serie_countries('Squid Game', ['south_korea','usa','uk','argentina','germany','france','brazil','spain','japan','mexico','china','india','sweden','norway','finland']).

serie('Succession', ['drama'], 8.8, 'Various', 4).
serie_age('Succession', 18).
serie_countries('Succession', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Watchmen', ['action','drama','mystery'], 8.2, 'Various', 1).
serie_age('Watchmen', 18).
serie_countries('Watchmen', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Sense8', ['sci-fi','drama','mystery'], 8.2, 'The Wachowskis', 2).
serie_age('Sense8', 18).
serie_countries('Sense8', ['usa','uk','germany','argentina','france','brazil','spain','india','south_korea','mexico','italy','china']).

serie('The Good Place', ['comedia','fantasy'], 8.2, 'Various', 4).
serie_age('The Good Place', 13).
serie_countries('The Good Place', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan']).

serie('Altered Carbon', ['action','sci-fi','thriller'], 8.0, 'Various', 2).
serie_age('Altered Carbon', 18).
serie_countries('Altered Carbon', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Mindhunter (2025)', ['crime','drama'], 8.6, 'David Fincher', 3).
serie_age('Mindhunter (2025)', 18).
serie_countries('Mindhunter (2025)', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('Raised by Wolves', ['sci-fi','drama'], 7.8, 'Various', 2).
serie_age('Raised by Wolves', 16).
serie_countries('Raised by Wolves', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Euphoria', ['drama'], 8.4, 'Various', 3).
serie_age('Euphoria', 18).
serie_countries('Euphoria', ['usa','uk','argentina','germany','france','brazil','spain','japan','mexico','italy']).

serie('The Handmaid\'s Tale', ['drama','sci-fi'], 8.0, 'Various', 5).
serie_age('The Handmaid\'s Tale', 18).
serie_countries('The Handmaid\'s Tale', ['usa','canada','uk','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Yellowstone', ['drama','western'], 8.7, 'Various', 5).
serie_age('Yellowstone', 18).
serie_countries('Yellowstone', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','mexico']).

serie('Severance', ['sci-fi','thriller'], 8.7, 'Various', 2).
serie_age('Severance', 16).
serie_countries('Severance', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('House', ['drama','mystery'], 8.7, 'Various', 8).
serie_age('House', 13).
serie_countries('House', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('Lost', ['adventure','drama','fantasy','mystery'], 8.3, 'Various', 6).
serie_age('Lost', 13).
serie_countries('Lost', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('Battlestar Galactica', ['sci-fi','action','drama'], 8.7, 'Various', 4).
serie_age('Battlestar Galactica', 13).
serie_countries('Battlestar Galactica', ['usa','canada','uk','argentina','germany','france','brazil','spain','australia','japan']).

serie('Mad Men', ['drama'], 8.6, 'Various', 7).
serie_age('Mad Men', 16).
serie_countries('Mad Men', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Fringe', ['sci-fi','mystery','thriller'], 8.4, 'Various', 5).
serie_age('Fringe', 16).
serie_countries('Fringe', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Buffy the Vampire Slayer', ['action','drama','fantasy'], 8.2, 'Various', 7).
serie_age('Buffy the Vampire Slayer', 13).
serie_countries('Buffy the Vampire Slayer', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan']).

serie('Homeland', ['drama','thriller'], 8.3, 'Various', 8).
serie_age('Homeland', 16).
serie_countries('Homeland', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Dexter', ['crime','drama','mystery'], 8.6, 'Various', 8).
serie_age('Dexter', 18).
serie_countries('Dexter', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Westworld (2025)', ['sci-fi','mystery','drama'], 8.5, 'Various', 5).
serie_age('Westworld (2025)', 18).
serie_countries('Westworld (2025)', ['usa','canada','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('The Office (UK)', ['comedia'], 8.5, 'Various', 2).
serie_age('The Office (UK)', 13).
serie_countries('The Office (UK)', ['uk','usa','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('True Detective', ['crime','drama','mystery'], 8.9, 'Various', 3).
serie_age('True Detective', 18).
serie_countries('True Detective', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Arrested Development', ['comedia'], 8.7, 'Various', 5).
serie_age('Arrested Development', 13).
serie_countries('Arrested Development', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan']).

serie('Better Call Saul', ['crime','drama'], 8.8, 'Vince Gilligan', 6).
serie_age('Better Call Saul', 18).
serie_countries('Better Call Saul', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('Oz', ['crime','drama'], 8.7, 'Various', 6).
serie_age('Oz', 18).
serie_countries('Oz', ['usa','uk','argentina','germany','france','brazil','spain','mexico']).

serie('Rome', ['action','drama','history'], 8.7, 'Various', 2).
serie_age('Rome', 18).
serie_countries('Rome', ['usa','uk','italy','argentina','germany','france','brazil','spain','australia','japan']).

serie('The Pacific', ['action','drama','war'], 8.3, 'Various', 1).
serie_age('The Pacific', 18).
serie_countries('The Pacific', ['usa','australia','uk','argentina','germany','france','brazil','spain','japan','mexico']).

serie('Band of Brothers', ['action','drama','war'], 9.4, 'Various', 1).
serie_age('Band of Brothers', 16).
serie_countries('Band of Brothers', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Firefly', ['adventure','drama','sci-fi'], 9.0, 'Joss Whedon', 1).
serie_age('Firefly', 13).
serie_countries('Firefly', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan']).

serie('The Good Wife', ['drama','mystery'], 8.3, 'Various', 7).
serie_age('The Good Wife', 13).
serie_countries('The Good Wife', ['usa','uk','canada','argentina','germany','france','brazil','spain','mexico']).

serie('Hannibal', ['crime','drama','horror'], 8.5, 'Various', 3).
serie_age('Hannibal', 18).
serie_countries('Hannibal', ['usa','uk','canada','argentina','germany','france','brazil','spain','japan','mexico']).

serie('Mr. Robot', ['crime','drama','thriller'], 8.5, 'Various', 4).
serie_age('Mr. Robot', 16).
serie_countries('Mr. Robot', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Jessica Jones', ['action','crime','drama'], 7.9, 'Various', 3).
serie_age('Jessica Jones', 16).
serie_countries('Jessica Jones', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Daredevil', ['action','crime','drama'], 8.6, 'Various', 3).
serie_age('Daredevil', 16).
serie_countries('Daredevil', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Luke Cage', ['action','crime','sci-fi'], 7.3, 'Various', 2).
serie_age('Luke Cage', 16).
serie_countries('Luke Cage', ['usa','uk','canada','argentina','germany','france','brazil','spain','mexico']).

serie('Iron Fist', ['action','adventure','crime'], 6.4, 'Various', 2).
serie_age('Iron Fist', 16).
serie_countries('Iron Fist', ['usa','uk','canada','argentina','germany','france','brazil','spain','mexico']).

serie('The Defenders', ['action','adventure','crime'], 7.3, 'Various', 1).
serie_age('The Defenders', 16).
serie_countries('The Defenders', ['usa','uk','canada','argentina','germany','france','brazil','spain','mexico']).

serie('Lost in Space', ['adventure','sci-fi'], 6.9, 'Various', 3).
serie_age('Lost in Space', 13).
serie_countries('Lost in Space', ['usa','canada','uk','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Lucifer', ['crime','drama','fantasy'], 8.1, 'Various', 6).
serie_age('Lucifer', 16).
serie_countries('Lucifer', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Money Heist (Korea)', ['action','crime','thriller'], 7.0, 'Various', 2).
serie_age('Money Heist (Korea)', 16).
serie_countries('Money Heist (Korea)', ['south_korea','usa','uk','argentina','germany','france','brazil','spain','japan','mexico','china','india','sweden']).

serie('The Last of Us', ['action','adventure','drama'], 8.8, 'Various', 1).
serie_age('The Last of Us', 18).
serie_countries('The Last of Us', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('Arcane (2024)', ['animation','action','fantasy'], 9.0, 'Various', 2).
serie_age('Arcane (2024)', 13).
serie_countries('Arcane (2024)', ['usa','france','uk','canada','argentina','germany','brazil','spain','australia','japan','mexico','italy','china','south_korea']).

serie('Invincible', ['animation','action','adventure'], 8.7, 'Various', 2).
serie_age('Invincible', 18).
serie_countries('Invincible', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Succession (2025)', ['drama','comedia'], 8.8, 'Various', 5).
serie_age('Succession (2025)', 18).
serie_countries('Succession (2025)', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('The Boys', ['action','comedia','crime'], 8.7, 'Various', 4).
serie_age('The Boys', 18).
serie_countries('The Boys', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Gen V', ['action','adventure','sci-fi'], 7.7, 'Various', 1).
serie_age('Gen V', 18).
serie_countries('Gen V', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Andor', ['action','adventure','sci-fi'], 8.4, 'Various', 1).
serie_age('Andor', 13).
serie_countries('Andor', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('Obi-Wan Kenobi', ['action','adventure','sci-fi'], 7.1, 'Various', 1).
serie_age('Obi-Wan Kenobi', 13).
serie_countries('Obi-Wan Kenobi', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('The Book of Boba Fett', ['action','adventure','sci-fi'], 7.2, 'Various', 1).
serie_age('The Book of Boba Fett', 13).
serie_countries('The Book of Boba Fett', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('Ahsoka', ['action','adventure','fantasy'], 8.0, 'Various', 1).
serie_age('Ahsoka', 13).
serie_countries('Ahsoka', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('Foundation', ['sci-fi','drama'], 7.6, 'Various', 2).
serie_age('Foundation', 13).
serie_countries('Foundation', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Halo', ['action','adventure','sci-fi'], 7.3, 'Various', 2).
serie_age('Halo', 16).
serie_countries('Halo', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Fallout', ['action','adventure','sci-fi'], 8.6, 'Various', 1).
serie_age('Fallout', 18).
serie_countries('Fallout', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('The X-Files', ['drama','mystery','sci-fi'], 8.6, 'Various', 11).
serie_age('The X-Files', 13).
serie_countries('The X-Files', ['usa','canada','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('Twin Peaks', ['drama','mystery','thriller'], 8.8, 'David Lynch', 3).
serie_age('Twin Peaks', 16).
serie_countries('Twin Peaks', ['usa','uk','argentina','germany','france','brazil','spain','japan','mexico']).

serie('Lost (2025)', ['adventure','drama','fantasy','mystery'], 8.3, 'Various', 7).
serie_age('Lost (2025)', 13).
serie_countries('Lost (2025)', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('The Witcher (2025)', ['fantasy','adventure','drama'], 8.2, 'Various', 3).
serie_age('The Witcher (2025)', 16).
serie_countries('The Witcher (2025)', ['uk','usa','poland','germany','france','brazil','spain','japan','mexico','india']).

serie('Yellowjackets', ['drama','mystery','thriller'], 8.0, 'Various', 3).
serie_age('Yellowjackets', 18).
serie_countries('Yellowjackets', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Succession (2026)', ['drama'], 8.8, 'Various', 6).
serie_age('Succession (2026)', 18).
serie_countries('Succession (2026)', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('Billions', ['drama'], 8.4, 'Various', 7).
serie_age('Billions', 18).
serie_countries('Billions', ['usa','uk','argentina','germany','france','brazil','spain','japan','mexico','italy']).

serie('The Morning Show', ['drama'], 8.1, 'Various', 3).
serie_age('The Morning Show', 16).
serie_countries('The Morning Show', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Servant', ['drama','horror','mystery'], 7.5, 'Various', 4).
serie_age('Servant', 16).
serie_countries('Servant', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Tehran', ['action','drama','thriller'], 7.6, 'Various', 3).
serie_age('Tehran', 16).
serie_countries('Tehran', ['iran','usa','uk','argentina','germany','france','brazil','spain','japan','mexico']).

serie('Hijack', ['action','thriller'], 7.4, 'Various', 1).
serie_age('Hijack', 13).
serie_countries('Hijack', ['uk','usa','argentina','germany','france','brazil','spain','japan','mexico','italy']).

serie('Silo', ['drama','sci-fi','thriller'], 8.1, 'Various', 2).
serie_age('Silo', 16).
serie_countries('Silo', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Slow Horses', ['drama','thriller'], 8.0, 'Various', 4).
serie_age('Slow Horses', 16).
serie_countries('Slow Horses', ['uk','usa','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Monarch: Legacy of Monsters', ['action','adventure','sci-fi'], 7.1, 'Various', 1).
serie_age('Monarch: Legacy of Monsters', 13).
serie_countries('Monarch: Legacy of Monsters', ['usa','japan','uk','argentina','germany','france','brazil','spain','australia','mexico']).

serie('For All Mankind', ['drama','sci-fi'], 8.0, 'Various', 4).
serie_age('For All Mankind', 13).
serie_countries('For All Mankind', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Shrinking', ['comedia','drama'], 8.0, 'Various', 2).
serie_age('Shrinking', 16).
serie_countries('Shrinking', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Platonic', ['comedia','romance'], 7.2, 'Various', 1).
serie_age('Platonic', 16).
serie_countries('Platonic', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Lessons in Chemistry', ['drama'], 7.7, 'Various', 1).
serie_age('Lessons in Chemistry', 16).
serie_countries('Lessons in Chemistry', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Poker Face', ['comedia','crime','mystery'], 7.9, 'Rian Johnson', 1).
serie_age('Poker Face', 16).
serie_countries('Poker Face', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('The Last Thing He Told Me', ['drama','mystery','thriller'], 7.0, 'Various', 1).
serie_age('The Last Thing He Told Me', 13).
serie_countries('The Last Thing He Told Me', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Tulsa King', ['crime','drama'], 7.7, 'Various', 2).
serie_age('Tulsa King', 18).
serie_countries('Tulsa King', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('1923', ['drama','western'], 8.3, 'Various', 2).
serie_age('1923', 18).
serie_countries('1923', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Rabbit Hole', ['action','thriller'], 7.1, 'Various', 1).
serie_age('Rabbit Hole', 16).
serie_countries('Rabbit Hole', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('The Night Agent', ['action','thriller'], 7.5, 'Various', 2).
serie_age('The Night Agent', 16).
serie_countries('The Night Agent', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('Beef', ['comedia','drama'], 8.0, 'Various', 1).
serie_age('Beef', 18).
serie_countries('Beef', ['usa','south_korea','uk','argentina','germany','france','brazil','spain','japan','mexico']).

serie('One Piece', ['action','adventure','fantasy'], 8.4, 'Various', 2).
serie_age('One Piece', 13).
serie_countries('One Piece', ['japan','usa','uk','canada','argentina','germany','france','brazil','spain','australia','mexico','italy','china','india','south_korea']).

serie('Loki', ['action','adventure','fantasy','sci-fi'], 8.2, 'Various', 2).
serie_age('Loki', 13).
serie_countries('Loki', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('WandaVision', ['drama','fantasy','mystery','sci-fi'], 7.9, 'Various', 1).
serie_age('WandaVision', 13).
serie_countries('WandaVision', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('The Falcon and the Winter Soldier', ['action','adventure','drama'], 7.2, 'Various', 1).
serie_age('The Falcon and the Winter Soldier', 13).
serie_countries('The Falcon and the Winter Soldier', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('Hawkeye', ['action','adventure','crime'], 7.6, 'Various', 1).
serie_age('Hawkeye', 13).
serie_countries('Hawkeye', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('Moon Knight', ['action','adventure','drama'], 7.3, 'Various', 1).
serie_age('Moon Knight', 16).
serie_countries('Moon Knight', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('She-Hulk: Attorney at Law', ['comedia','action','fantasy'], 5.3, 'Various', 1).
serie_age('She-Hulk: Attorney at Law', 16).
serie_countries('She-Hulk: Attorney at Law', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Secret Invasion', ['action','adventure','drama','sci-fi'], 6.0, 'Various', 1).
serie_age('Secret Invasion', 16).
serie_countries('Secret Invasion', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Daredevil: Born Again', ['action','crime','drama'], 8.6, 'Various', 1).
serie_age('Daredevil: Born Again', 16).
serie_countries('Daredevil: Born Again', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('What If...?', ['animation','action','adventure','sci-fi'], 7.4, 'Various', 2).
serie_age('What If...?', 13).
serie_countries('What If...?', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('Tokyo Vice', ['crime','drama','thriller'], 8.0, 'Various', 2).
serie_age('Tokyo Vice', 18).
serie_countries('Tokyo Vice', ['usa','japan','uk','argentina','germany','france','brazil','spain','mexico','italy']).

serie('Shōgun', ['adventure','drama','history'], 9.2, 'Various', 1).
serie_age('Shōgun', 18).
serie_countries('Shōgun', ['usa','japan','uk','argentina','germany','france','brazil','spain','australia','mexico','italy','china']).

serie('The Diplomat', ['drama','thriller'], 8.0, 'Various', 2).
serie_age('The Diplomat', 16).
serie_countries('The Diplomat', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Beef (2025)', ['comedia','drama'], 8.0, 'Various', 2).
serie_age('Beef (2025)', 18).
serie_countries('Beef (2025)', ['usa','south_korea','uk','argentina','germany','france','brazil','spain','japan','mexico']).

serie('One Piece (2025)', ['action','adventure','fantasy'], 8.4, 'Various', 3).
serie_age('One Piece (2025)', 13).
serie_countries('One Piece (2025)', ['japan','usa','uk','canada','argentina','germany','france','brazil','spain','australia','mexico','italy','china','india','south_korea']).

serie('Ted Lasso (2024)', ['comedia','sport'], 8.8, 'Various', 4).
serie_age('Ted Lasso (2024)', 10).
serie_countries('Ted Lasso (2024)', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('The Umbrella Academy', ['action','adventure','comedia','fantasy'], 7.9, 'Various', 4).
serie_age('The Umbrella Academy', 16).
serie_countries('The Umbrella Academy', ['usa','canada','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('Russian Doll', ['comedia','drama','mystery'], 7.9, 'Various', 2).
serie_age('Russian Doll', 16).
serie_countries('Russian Doll', ['usa','uk','argentina','germany','france','brazil','spain','japan','mexico']).

serie('Sweet Tooth', ['adventure','drama','fantasy'], 7.9, 'Various', 3).
serie_age('Sweet Tooth', 13).
serie_countries('Sweet Tooth', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Sex Education', ['comedia','drama'], 8.3, 'Various', 4).
serie_age('Sex Education', 18).
serie_countries('Sex Education', ['uk','usa','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('The Serpent', ['biography','crime','drama'], 7.6, 'Various', 1).
serie_age('The Serpent', 18).
serie_countries('The Serpent', ['uk','france','argentina','germany','brazil','spain','japan','mexico']).

serie('Bridgerton', ['drama','romance'], 7.4, 'Various', 3).
serie_age('Bridgerton', 16).
serie_countries('Bridgerton', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('The Queen’s Gambit (2025)', ['drama'], 8.6, 'Scott Frank', 2).
serie_age('The Queen’s Gambit (2025)', 16).
serie_countries('The Queen’s Gambit (2025)', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Arcane (2025)', ['animation'], 9.0, 'Various', 3).
serie_age('Arcane (2025)', 13).
serie_countries('Arcane (2025)', ['usa','france','uk','canada','argentina','germany','brazil','spain','australia','japan','mexico','italy','china','south_korea']).

serie('The OA', ['drama','fantasy','mystery','sci-fi'], 7.8, 'Various', 2).
serie_age('The OA', 16).
serie_countries('The OA', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('The Night Of', ['crime','drama','mystery'], 8.5, 'Various', 1).
serie_age('The Night Of', 18).
serie_countries('The Night Of', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Mare of Easttown', ['crime','drama','mystery'], 8.4, 'Craig Zobel', 1).
serie_age('Mare of Easttown', 18).
serie_countries('Mare of Easttown', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('True Detective (2025)', ['crime','drama','mystery'], 8.9, 'Various', 4).
serie_age('True Detective (2025)', 18).
serie_countries('True Detective (2025)', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Yellowstone (2025)', ['drama','western'], 8.7, 'Various', 6).
serie_age('Yellowstone (2025)', 18).
serie_countries('Yellowstone (2025)', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','mexico']).

serie('Better Call Saul (2025)', ['crime','drama','thriller'], 8.8, 'Vince Gilligan', 7).
serie_age('Better Call Saul (2025)', 18).
serie_countries('Better Call Saul (2025)', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('The Bear', ['comedia','drama'], 8.6, 'Various', 3).
serie_age('The Bear', 18).
serie_countries('The Bear', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Only Murders in the Building', ['comedia','crime','mystery'], 8.1, 'Various', 4).
serie_age('Only Murders in the Building', 13).
serie_countries('Only Murders in the Building', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Reservation Dogs', ['comedia','crime'], 8.1, 'Various', 3).
serie_age('Reservation Dogs', 16).
serie_countries('Reservation Dogs', ['usa','uk','argentina','germany','france','brazil','spain','australia','mexico']).

serie('What We Do in the Shadows', ['comedia','fantasy'], 8.6, 'Various', 6).
serie_age('What We Do in the Shadows', 16).
serie_countries('What We Do in the Shadows', ['usa','new_zealand','uk','argentina','germany','france','brazil','spain','australia','mexico']).

serie('The Last Kingdom', ['action','drama','history'], 8.4, 'Various', 5).
serie_age('The Last Kingdom', 16).
serie_countries('The Last Kingdom', ['uk','usa','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Vikings', ['action','drama','history'], 8.2, 'Various', 6).
serie_age('Vikings', 16).
serie_countries('Vikings', ['canada','ireland','usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('The Last of Us (2025)', ['action','adventure','drama'], 8.8, 'Various', 2).
serie_age('The Last of Us (2025)', 18).
serie_countries('The Last of Us (2025)', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('Severance (2025)', ['sci-fi','thriller','mystery'], 8.7, 'Various', 3).
serie_age('Severance (2025)', 16).
serie_countries('Severance (2025)', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Euphoria (2025)', ['drama'], 8.4, 'Various', 4).
serie_age('Euphoria (2025)', 18).
serie_countries('Euphoria (2025)', ['usa','uk','argentina','germany','france','brazil','spain','japan','mexico','italy']).

serie('The Handmaid\'s Tale (2025)', ['drama','sci-fi'], 8.0, 'Various', 6).
serie_age('The Handmaid\'s Tale (2025)', 18).
serie_countries('The Handmaid\'s Tale (2025)', ['usa','canada','uk','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Ted Lasso (2025)', ['comedia'], 8.8, 'Various', 5).
serie_age('Ted Lasso (2025)', 10).
serie_countries('Ted Lasso (2025)', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('The White Lotus', ['comedia','drama'], 8.0, 'Mike White', 3).
serie_age('The White Lotus', 18).
serie_countries('The White Lotus', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('House of the Dragon', ['action','adventure','fantasy'], 8.5, 'Various', 2).
serie_age('House of the Dragon', 18).
serie_countries('House of the Dragon', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('The Rings of Power', ['action','adventure','fantasy'], 7.0, 'Various', 2).
serie_age('The Rings of Power', 13).
serie_countries('The Rings of Power', ['usa','new_zealand','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('Peacemaker', ['action','comedia','sci-fi'], 8.3, 'James Gunn', 2).
serie_age('Peacemaker', 18).
serie_countries('Peacemaker', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Doom Patrol', ['action','adventure','comedia','fantasy'], 7.8, 'Various', 4).
serie_age('Doom Patrol', 16).
serie_countries('Doom Patrol', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Titans', ['action','adventure','drama'], 7.5, 'Various', 4).
serie_age('Titans', 16).
serie_countries('Titans', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Hacks', ['comedia','drama'], 8.2, 'Various', 3).
serie_age('Hacks', 18).
serie_countries('Hacks', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Barry', ['action','comedia','crime'], 8.3, 'Bill Hader', 4).
serie_age('Barry', 18).
serie_countries('Barry', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Atlanta', ['comedia','drama'], 8.6, 'Donald Glover', 4).
serie_age('Atlanta', 18).
serie_countries('Atlanta', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('What If...? (2025)', ['animation','action','adventure','sci-fi'], 7.4, 'Various', 3).
serie_age('What If...? (2025)', 13).
serie_countries('What If...? (2025)', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('Invincible (2025)', ['animation','action','adventure'], 8.7, 'Various', 3).
serie_age('Invincible (2025)', 18).
serie_countries('Invincible (2025)', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Masters of the Air', ['action','drama','war'], 8.0, 'Various', 1).
serie_age('Masters of the Air', 16).
serie_countries('Masters of the Air', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Tokyo Vice (2025)', ['crime','drama','thriller'], 8.0, 'Various', 3).
serie_age('Tokyo Vice (2025)', 18).
serie_countries('Tokyo Vice (2025)', ['usa','japan','uk','argentina','germany','france','brazil','spain','mexico','italy']).

serie('The Diplomat (2025)', ['drama','thriller'], 8.0, 'Various', 3).
serie_age('The Diplomat (2025)', 16).
serie_countries('The Diplomat (2025)', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Silo (2025)', ['drama','sci-fi','thriller'], 8.1, 'Various', 3).
serie_age('Silo (2025)', 16).
serie_countries('Silo (2025)', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Slow Horses (2025)', ['drama','thriller'], 8.0, 'Various', 5).
serie_age('Slow Horses (2025)', 16).
serie_countries('Slow Horses (2025)', ['uk','usa','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Foundation (2025)', ['sci-fi','drama'], 7.6, 'Various', 3).
serie_age('Foundation (2025)', 13).
serie_countries('Foundation (2025)', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('The Terminal List', ['action','thriller'], 7.9, 'Various', 2).
serie_age('The Terminal List', 18).
serie_countries('The Terminal List', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Reacher', ['action','crime','thriller'], 8.1, 'Various', 3).
serie_age('Reacher', 18).
serie_countries('Reacher', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Bosch', ['crime','drama'], 8.5, 'Various', 7).
serie_age('Bosch', 16).
serie_countries('Bosch', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('The Marvelous Mrs. Maisel', ['comedia','drama'], 8.7, 'Amy Sherman-Palladino', 5).
serie_age('The Marvelous Mrs. Maisel', 13).
serie_countries('The Marvelous Mrs. Maisel', ['usa','uk','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Transparent', ['comedia','drama'], 7.8, 'Jill Soloway', 5).
serie_age('Transparent', 18).
serie_countries('Transparent', ['usa','uk','argentina','germany','france','brazil','spain','mexico']).

serie('Upload', ['comedia','mystery','sci-fi'], 8.0, 'Greg Daniels', 3).
serie_age('Upload', 16).
serie_countries('Upload', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Good Omens', ['comedia','fantasy'], 8.1, 'Various', 2).
serie_age('Good Omens', 13).
serie_countries('Good Omens', ['uk','usa','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('The Boys (2025)', ['action','comedia','crime'], 8.7, 'Various', 5).
serie_age('The Boys (2025)', 18).
serie_countries('The Boys (2025)', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Gen V (2025)', ['action','adventure','sci-fi'], 7.7, 'Various', 2).
serie_age('Gen V (2025)', 18).
serie_countries('Gen V (2025)', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico']).

serie('Foundation (2026)', ['sci-fi','drama','fantasy'], 7.6, 'Various', 4).
serie_age('Foundation (2026)', 13).
serie_countries('Foundation (2026)', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy']).

serie('Loki (2025)', ['action','adventure','fantasy','sci-fi'], 8.2, 'Various', 3).
serie_age('Loki (2025)', 13).
serie_countries('Loki (2025)', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('WandaVision (2025)', ['drama','fantasy','mystery','sci-fi'], 7.9, 'Various', 2).
serie_age('WandaVision (2025)', 13).
serie_countries('WandaVision (2025)', ['usa','uk','canada','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

serie('The Umbrella Academy (2025)', ['action','adventure','comedia','fantasy'], 7.9, 'Various', 5).
serie_age('The Umbrella Academy (2025)', 16).
serie_countries('The Umbrella Academy (2025)', ['usa','canada','uk','argentina','germany','france','brazil','spain','australia','japan','mexico','italy','china']).

% -------------------------
% Reglas de recomendación
% -------------------------
% recommend(User, Title, Director, Rating, Genres)
% recommend_serie(User, Title, Director, Rating, Genres, Seasons)

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

recommend_serie(U, Title, Director, Rating, Genres, Seasons) :-
    serie(Title, Genres, Rating, Director, Seasons),
    likes(U, LikedGenre),
    member(LikedGenre, Genres),
    \+ (dislikes(U, DislikedGenre), member(DislikedGenre, Genres)),
    ( serie_age(Title, MinAge) -> true ; MinAge = 0 ),
    user_age(U, UserAge),
    UserAge >= MinAge,
    user_country(U, UserCountry),
    ( serie_countries(Title, Countries) -> true ; Countries = [] ),
    member(UserCountry, Countries).

% Fallbacks que solo devuelven Title
recommend(U, Title) :-
    recommend(U, Title, _Director, _Rating, _Genres).

recommend_serie(U, Title) :-
    recommend_serie(U, Title, _Director, _Rating, _Genres, _Seasons).

% -------------------------
%% --- START USER FACTS INJECTION ---
%% --- GUSTOS DE USUARIOS (Acumulados en la sesión) ---
likes('usuario','sci-fi').
likes('usuario','fantasy').
likes('usuario','action').
likes('usuario','mystery').
likes('usuario','thriller').
likes('usuario','adventure').
likes('usuario','crime').
likes('usuario','animation').
likes('usuario','comedia').

%% --- DISGUSTOS DE USUARIOS ---
dislikes('usuario','drama').
dislikes('usuario','horror').
dislikes('usuario','romance').

%% --- METADATOS DE USUARIOS ---
user_age('usuario',20).
user_country('usuario','usa').

%% --- END USER FACTS INJECTION ---

