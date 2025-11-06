!/usr/bin/env python3
"""
Sistema simple de recomendación usando:
- Python: OO e imperativo (gestión de usuarios, aprendizaje simple)
- Prolog: base de conocimiento y reglas (recomendaciones)
Requiere: SWI-Prolog (comando 'swipl')
"""

import subprocess
import shlex
import ast
from collections import defaultdict

# ---------- Datos de ejemplo (puedes ampliarlos) ----------
MOVIES = [
    {"title": "Star Wars", "genres": ["sci-fi", "adventure"]},
    {"title": "The Expanse", "genres": ["sci-fi", "drama"]},
    {"title": "Interstellar", "genres": ["sci-fi", "drama"]},
    {"title": "The Office", "genres": ["comedy"]},
    {"title": "Parks and Recreation", "genres": ["comedy"]},
    {"title": "Inception", "genres": ["sci-fi", "thriller"]},
    {"title": "The Witcher", "genres": ["fantasy", "adventure"]},
    {"title": "Lord of the Rings", "genres": ["fantasy", "adventure"]},
    {"title": "Breaking Bad", "genres": ["drama", "crime"]},
]

USERS_WATCHED = {
    # usuario: lista de títulos vistos
    "alice": ["Star Wars", "Inception", "The Office"],
    "bob":   ["The Witcher", "Lord of the Rings"],
    "carla": ["Breaking Bad", "The Expanse"],
}

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
        """
        Heurística sencilla: si un usuario vio >= min_seen_for_like películas de un género,
        se considera que "le gusta" ese género.
        """
        likes = defaultdict(set)  # user -> set(genres)
        for username, user in self.users.items():
            genre_counts = defaultdict(int)
            for title in user.watched:
                for m in self.movies:
                    if m.title == title:
                        for g in m.genres:
                            genre_counts[g] += 1
                        break
            for g, cnt in genre_counts.items():
                if cnt >= min_seen_for_like:
                    likes[username].add(g)
        return likes

    def build_prolog_kb(self, filename="kb.pl", likes=None):
        """
        Genera un fichero Prolog con:
        - hechos movie('Title', ['g1','g2']).
        - hechos watched('user','Title').
        - hechos likes('user','genre').   (derivados del aprendizaje)
        - reglas recommend(User,Movie).
        """
        likes = likes or {}
        with open(filename, "w", encoding="utf-8") as f:
            f.write("%% Base de conocimiento generada automáticamente\n\n")
            # movie facts
            for m in self.movies:
                # Prolog list of atoms with quotes
                genres_list = "[" + ",".join(["'{}'".format(g) for g in m.genres]) + "]"
                f.write("movie('{}', {}).\n".format(m.title.replace("'", "\\'"), genres_list))
            f.write("\n")
            # watched facts
            for uname, user in self.users.items():
                for title in user.watched:
                    f.write("watched('{}','{}').\n".format(uname, title.replace("'", "\\'")))
            f.write("\n")
            # likes facts (from learning)
            for uname, genset in likes.items():
                for g in genset:
                    f.write("likes('{}','{}').\n".format(uname, g))
            f.write("\n")
            # rules
            f.write("% Reglas de recomendación:\n")
            f.write("% 1) Recomendar película que comparta un género que le guste al usuario y que no haya visto.\n")
            f.write("recommend(U,Title) :- movie(Title, Genres), likes(U,G), member(G, Genres), \\+ watched(U,Title).\n")
            f.write("\n")
            f.write("% 2) Recomendar películas que compartan género con alguna vista por el usuario (fallback).\n")
            f.write("recommend(U,Title) :- movie(Title, Genres), watched(U,Seen), movie(Seen, SeenGenres), member(G2, SeenGenres), member(G2, Genres), \\+ watched(U,Title).\n")
            f.write("\n")
            f.write("% member/2 está disponible en SWI-Prolog; si no, podríamos definirla aquí.\n")
        print(f"[Python] KB Prolog escrita en '{filename}'")

    def query_recommendations(self, username, kb_filename="kb.pl"):
        """
        Llama a SWI-Prolog para pedir recomendaciones:
        findall(Title, recommend('alice', Title), L), write(L), halt.
        Retorna una lista de strings.
        """
        prolog_query = "findall(Title, recommend('{}', Title), L), write(L), nl, halt.".format(username)
        cmd = ["swipl", "-q", "-s", kb_filename, "-g", prolog_query]
        try:
            res = subprocess.run(cmd, capture_output=True, text=True, check=True)
        except FileNotFoundError:
            raise RuntimeError("No se encontró 'swipl'. Asegurate de tener instalado SWI-Prolog y que 'swipl' esté en PATH.")
        except subprocess.CalledProcessError as e:
            raise RuntimeError(f"Error ejecutando Prolog: {e.stderr}\nComando: {' '.join(cmd)}")
        output = res.stdout.strip()
        # SWI-Prolog imprime la lista como ['The Expanse','Interstellar'] (con comillas)
        # Intentamos convertirla a una lista Python usando ast.literal_eval
        try:
            pylist = ast.literal_eval(output)
            # Normalizamos a strings
            return [str(x) for x in pylist]
        except Exception:
            # Fallback: parse manualmente removiendo [ ] y comillas
            s = output.strip()
            if s.startswith("[") and s.endswith("]"):
                inner = s[1:-1].strip()
                if not inner:
                    return []
                parts = [p.strip().strip("'\"") for p in inner.split(",")]
                return parts
            return []

# ---------- Demo / ejecución ----------
def demo():
    print("=== Recommender demo ===")
    r = Recommender(MOVIES, USERS_WATCHED)
    likes = r.learn_likes(min_seen_for_like=1)  # con 1 para demo; ajustar a 2 si preferís más selectivo
    print("[Python] Gustos aprendidos (likes):")
    for u, gs in likes.items():
        print(f"  - {u}: {sorted(gs)}")
    kb_file = "kb.pl"
    r.build_prolog_kb(filename=kb_file, likes=likes)
    # Pedimos recomendaciones para cada usuario
    for u in r.users.keys():
        try:
            recs = r.query_recommendations(u, kb_filename=kb_file)
            print(f"\nRecomendaciones para {u}:")
            if recs:
                for i,m in enumerate(recs, 1):
                    print(f"  {i}. {m}")
            else:
                print("  (no hay recomendaciones por ahora)")
        except RuntimeError as e:
            print("ERROR:", e)
            print("Asegurate de tener SWI-Prolog instalado y que 'swipl' esté en PATH.")
            break

if __name__ == "__main__":
    demo()
