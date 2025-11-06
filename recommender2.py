#!/usr/bin/env python3
"""
Sistema simple de recomendación usando:
- Python: OO e imperativo (gestión de usuarios persistente)
- Prolog: base de conocimiento y reglas (recomendaciones con FILTRO DE EXCLUSIÓN)
Requiere: SWI-Prolog (comando 'swipl')
"""

import subprocess
import shlex
import ast
from collections import defaultdict
import random 
import os.path
import re # Necesario para parsear el archivo Prolog

# ---------- Datos de ejemplo (AMPLIADOS para más variedad) ----------
MOVIES = [
    # Existentes
    {"title": "Star Wars", "genres": ["sci-fi", "adventure"]},
    {"title": "The Expanse", "genres": ["sci-fi", "drama"]},
    {"title": "Interstellar", "genres": ["sci-fi", "drama"]},
    {"title": "The Office", "genres": ["comedy"]},
    {"title": "Parks and Recreation", "genres": ["comedy"]},
    {"title": "Inception", "genres": ["sci-fi", "thriller"]},
    {"title": "The Witcher", "genres": ["fantasy", "adventure"]},
    {"title": "Lord of the Rings", "genres": ["fantasy", "adventure"]},
    {"title": "Breaking Bad", "genres": ["drama", "crime"]},
    {"title": "Blade Runner 2049", "genres": ["sci-fi", "drama", "thriller"]}, 
    {"title": "Mad Max: Fury Road", "genres": ["sci-fi", "adventure", "thriller", "action"]}, 
    {"title": "Dune", "genres": ["sci-fi", "fantasy", "adventure", "drama"]}, 

    # 20 Películas adicionales
    {"title": "Pulp Fiction", "genres": ["crime", "drama", "thriller"]},
    {"title": "Alien", "genres": ["sci-fi", "horror", "thriller"]},
    {"title": "Spirited Away", "genres": ["fantasy", "adventure", "animation"]},
    {"title": "Parasite", "genres": ["drama", "thriller", "crime"]},
    {"title": "Eternal Sunshine", "genres": ["sci-fi", "romance", "drama"]},
    {"title": "The Grand Budapest Hotel", "genres": ["comedy", "adventure", "drama"]},
    {"title": "Arrival", "genres": ["sci-fi", "drama", "mystery"]},
    {"title": "Knives Out", "genres": ["mystery", "comedy", "crime"]},
    {"title": "The Matrix", "genres": ["sci-fi", "action", "thriller"]},
    {"title": "The Dark Knight", "genres": ["action", "crime", "drama"]},
    {"title": "Crouching Tiger", "genres": ["action", "fantasy", "romance"]},
    {"title": "Django Unchained", "genres": ["western", "drama"]},
    {"title": "Whiplash", "genres": ["drama", "music"]},
    {"title": "Shaun of the Dead", "genres": ["comedy", "horror"]},
    {"title": "No Country for Old Men", "genres": ["crime", "drama", "thriller"]},
    {"title": "Finding Nemo", "genres": ["adventure", "animation", "comedy"]},
    {"title": "Harry Potter 7", "genres": ["fantasy", "adventure", "thriller"]},
    {"title": "A Quiet Place", "genres": ["horror", "sci-fi", "thriller"]},
    {"title": "Gladiator", "genres": ["action", "drama", "adventure"]},
    {"title": "Sherlock", "genres": ["mystery", "crime", "drama"]},
]

# USERS_WATCHED debe estar vacío
USERS_WATCHED = {}

# Géneros para preguntar al usuario (12 en total)
AVAILABLE_GENRES = [
    "sci-fi", "adventure", "comedy", "drama", "fantasy", "thriller", "crime", "horror",
    "action", "mystery", "romance", "animation" 
]

# ---------- Funciones de Persistencia ----------

def load_existing_preferences(filename="peli.pl"):
    """
    Lee el archivo Prolog para inicializar el historial de likes/dislikes 
    de sesiones anteriores.
    """
    all_user_likes = defaultdict(set)
    all_user_dislikes = defaultdict(set)
    
    if not os.path.exists(filename):
        return all_user_likes, all_user_dislikes

    # Regex para capturar hechos likes/dislikes('usuario','genero').
    fact_regex = re.compile(r"(likes|dislikes)\('([^']+)','([^']+)'\)\.")
    
    with open(filename, "r", encoding="utf-8") as f:
        for line in f:
            match = fact_regex.match(line.strip())
            if match:
                fact_type, user, genre = match.groups()
                # Aseguramos que el usuario esté en minúsculas para consistencia
                user = user.lower() 
                if fact_type == 'likes':
                    all_user_likes[user].add(genre)
                elif fact_type == 'dislikes':
                    all_user_dislikes[user].add(genre)
                    
    return all_user_likes, all_user_dislikes

# ---------- Clases Python (gestión simple) ----------
class Movie:
    def __init__(self, title, genres):
        self.title = title
        self.genres = genres

class User:
    def __init__(self, username, watched=None):
        self.username = username
        self.watched = set(watched or [])

class Recommender:
    def __init__(self, movies, users_watched):
        self.movies = [Movie(m["title"], m["genres"]) for m in movies]
        self.users = {u: User(u, wl) for u, wl in users_watched.items()}

    def learn_likes(self, min_seen_for_like=2):
        return defaultdict(set) 

    def build_prolog_peli(self, filename="peli.pl", all_user_likes=None, all_user_dislikes=None):
        """
        Genera/reemplaza el fichero Prolog con TODOS los datos acumulados (persistentes + sesión actual).
        """
        all_user_likes = all_user_likes or {}
        all_user_dislikes = all_user_dislikes or {}

        with open(filename, "w", encoding="utf-8") as f:
            f.write("%% Base de conocimiento generada automáticamente\n\n")
            
            # 1. Hechos 'movie'
            for m in self.movies:
                genres_list = "[" + ",".join(["'{}'".format(g) for g in m.genres]) + "]"
                f.write("movie('{}', {}).\n".format(m.title.replace("'", "\\'"), genres_list))
            f.write("\n")
            
            # 2. Hechos 'likes' de todos los usuarios (PERSISTENTES)
            f.write("%% --- Gustos de Usuarios Persistentes ---\n")
            for uname, genset in all_user_likes.items():
                for g in genset:
                    f.write("likes('{}','{}').\n".format(uname, g))
            f.write("\n")
            
            # 3. Hechos 'dislikes' de todos los usuarios (PERSISTENTES)
            f.write("%% --- Disgustos de Usuarios Persistentes ---\n")
            for uname, genset in all_user_dislikes.items():
                for g in genset:
                    f.write("dislikes('{}','{}').\n".format(uname, g))
            f.write("\n")
            
            # 4. REGLA PROLOG
            f.write("% Regla de recomendación: SIN DISLIKES + AL MENOS 1 LIKE\n")
            f.write("recommend(U,Title) :-\n")
            f.write("    movie(Title, MovieGenres),\n")
            
            # FILTRO DE EXCLUSIÓN: \+ (Existe un DislikedGenre que es miembro de MovieGenres)
            f.write("    \\+ (\n")
            f.write("        dislikes(U, DislikedGenre),\n")
            f.write("        member(DislikedGenre, MovieGenres)\n")
            f.write("    ),\n")
            
            # FILTRO DE INCLUSIÓN: (Existe un LikedGenre que es miembro de MovieGenres)
            f.write("    (\n")
            f.write("        likes(U, LikedGenre),\n")
            f.write("        member(LikedGenre, MovieGenres)\n")
            f.write("    ).\n")
            
            f.write("\n")
            
        print(f"[Python] KB Prolog actualizada en '{filename}' con {len(all_user_likes)} usuarios persistentes.")

    def query_recommendations(self, username, peli_filename="peli.pl"):
        """
        Llama a SWI-Prolog para pedir recomendaciones.
        Devuelve una lista de títulos únicos.
        """
        safe_username = username.lower().replace("'", "")
        # Usamos findall, luego eliminaremos duplicados en Python
        prolog_query = "findall(Title, recommend('{}', Title), L), write(L), nl, halt.".format(safe_username)
        cmd = ["swipl", "-q", "-s", peli_filename, "-g", prolog_query]
        try:
            res = subprocess.run(cmd, capture_output=True, text=True, check=True)
        except FileNotFoundError:
            raise RuntimeError("No se encontró 'swipl'. Asegurate de tener instalado SWI-Prolog y que 'swipl' esté en PATH.")
        except subprocess.CalledProcessError as e:
            error_output = e.stderr.strip()
            if error_output:
                print(f"\n[Error de Prolog]:\n{error_output}")
            raise RuntimeError(f"Error ejecutando Prolog. Comando: {' '.join(cmd)}")
            
        output = res.stdout.strip()
        pylist = []
        try:
            # Intentamos parsear la lista de Prolog
            pylist = ast.literal_eval(output)
            # Aseguramos que son strings
            pylist = [str(x) for x in pylist]
        except Exception:
            # Fallback de parseo si ast.literal_eval falla
            s = output.strip()
            if s.startswith("[") and s.endswith("]"):
                inner = s[1:-1].strip()
                if inner:
                    parts = [p.strip().strip("'\"") for p in inner.split(",")]
                    pylist = parts
        
        # Solución para duplicados: convertir a set y luego a list
        return list(set(pylist))

# ---------- Funciones de entrada de usuario ----------
def get_user_input(existing_users):
    """
    Pide al usuario su nombre y 12 géneros que le gustan/disgustan.
    Devuelve nombre, likes y dislikes.
    """
    print("\n--- INICIO DE NUEVO USUARIO ---")
    
    # 1. Nombre de usuario (advertir si ya existe en el historial persistente)
    while True:
        username = input("Por favor, introduce tu nombre de usuario: ").strip().lower()
        if not username:
            print("El nombre de usuario no puede estar vacío.")
        elif username in existing_users:
            print(f"El usuario '{username}' ya existe en el historial. Sus *nuevas* respuestas se añadirán a las anteriores.")
            # Permitimos continuar para que actualice o refuerce sus preferencias.
            break
        else:
            break

    # 2. Preferencias de género
    user_likes = set()
    user_dislikes = set()
    print(f"\nAhora, vamos a preguntarte sobre {len(AVAILABLE_GENRES)} géneros.")
    print("Responde 'si' o 'no' (o 's'/'n').")

    for genre in AVAILABLE_GENRES:
        while True:
            response = input(f"¿Te gusta el género **{genre.upper()}**? (si/no): ").strip().lower()
            if response in ['si', 's']:
                user_likes.add(genre)
                break
            elif response in ['no', 'n']:
                user_dislikes.add(genre)
                break
            else:
                print("Respuesta no válida. Por favor, escribe 'si' o 'no'.")
    
    return username, user_likes, user_dislikes

# ---------- Demo / ejecución con bucle principal ----------
def demo():
    
    peli_file = "peli.pl"
    
    # 1. CARGAR HISTORIAL PERSISTENTE
    all_user_likes, all_user_dislikes = load_existing_preferences(peli_file)
    print(f"=== Sistema de Recomendación Prolog/Python (Persistente) ===")
    print(f"[Python] Se cargaron datos persistentes de {len(all_user_likes)} usuarios.")

    r = Recommender(MOVIES, USERS_WATCHED.copy())
    
    while True:
        try:
            # Obtener los nombres de los usuarios persistentes para la verificación
            existing_users = all_user_likes.keys()
            
            # 2. Obtener entrada de un nuevo usuario
            new_user, new_likes, new_dislikes = get_user_input(existing_users)
            
            # 3. Actualizar el historial acumulativo (los nuevos datos se fusionan con los persistentes)
            all_user_likes[new_user].update(new_likes)
            all_user_dislikes[new_user].update(new_dislikes)

            # Si no hay likes, saltamos la recomendación
            if not all_user_likes[new_user]:
                print("\n[AVISO] El usuario no tiene ningún género 'si' guardado. No se puede recomendar.")
            else:
                # 4. Generar KB con datos persistentes + datos actuales (GUARDA LA PERSISTENCIA)
                r.build_prolog_peli(filename=peli_file, all_user_likes=all_user_likes, all_user_dislikes=all_user_dislikes)
                
                # 5. Pedir recomendaciones
                recs = r.query_recommendations(new_user, peli_filename=peli_file)
                
                print(f"\n--- RECOMENDACIONES para **{new_user}** (Criterio: SIN DISLIKES + AL MENOS 1 LIKE) ---")
                if recs:
                    for i,m in enumerate(recs, 1):
                        movie_obj = next((m_obj for m_obj in r.movies if m_obj.title == m), None)
                        genres_info = ""
                        if movie_obj:
                            # Calculamos coincidencias basadas en el estado final del usuario
                            user_final_likes = all_user_likes[new_user]
                            user_final_dislikes = all_user_dislikes[new_user]
                            
                            match_likes = user_final_likes.intersection(set(movie_obj.genres))
                            match_dislikes = user_final_dislikes.intersection(set(movie_obj.genres))
                            
                            genres_info = f" ({', '.join(movie_obj.genres)}) - Coincidencias LIKES: {len(match_likes)}"
                            if match_dislikes:
                                # Esto solo debería verse si Prolog falló, pero sirve como verificación
                                genres_info += f" [¡ERROR LÓGICO! Incluye DISLIKE(S): {', '.join(match_dislikes)}]"
                        print(f"  {i}. {m}{genres_info}")
                else:
                    print("  (No se encontró ninguna película que cumpla el criterio de exclusión y al menos un like.)")
        
        except RuntimeError as e:
            print("\nERROR:", e)
            print("El programa se detiene. Asegurate de tener SWI-Prolog instalado y que 'swipl' esté en PATH.")
            break
        
        # 6. Preguntar si desea continuar
        print("-" * 50)
        while True:
            cont = input("¿Desea atender a otro usuario? (si/no): ").strip().lower()
            if cont in ['no', 'n']:
                print("\nGracias por usar el sistema. ¡Adiós!")
                return
            elif cont in ['si', 's']:
                break
            else:
                print("Respuesta no válida.")

if __name__ == "__main__":
    demo()
