#!/usr/bin/env python3
"""
Sistema simple de recomendación usando:
- Python: OO e imperativo (gestión de usuarios persistente)
- Prolog: base de conocimiento y reglas (recomendaciones con FILTRO DE EXCLUSIÓN)
- Tkinter: Interfaz gráfica de usuario.
Requiere: SWI-Prolog (comando 'swipl')
"""

import subprocess
import shlex
import ast
from collections import defaultdict
import random
import os.path
import re
import tkinter as tk
from tkinter import messagebox, simpledialog, Listbox, Scrollbar, ttk

# --- [DATOS ORIGINALES Y CONSTANTES] ---
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
    {"title": "Blade Runner 2049", "genres": ["sci-fi", "drama", "thriller"]},
    {"title": "Mad Max: Fury Road", "genres": ["sci-fi", "adventure", "thriller", "action"]},
    {"title": "Dune", "genres": ["sci-fi", "fantasy", "adventure", "drama"]},
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

USERS_WATCHED = {} # No utilizado directamente, se mantiene para compatibilidad de la clase Recommender

AVAILABLE_GENRES = [
    "sci-fi", "adventure", "comedy", "drama", "fantasy", "thriller", "crime", "horror",
    "action", "mystery", "romance", "animation"
]
PELI_FILE = "pelis.pl"

# --- [Funciones de Persistencia] ---
def load_existing_preferences(filename=PELI_FILE):
    """
    Lee el archivo Prolog para inicializar el historial de likes/dislikes
    de sesiones anteriores. (La misma función original)
    """
    all_user_likes = defaultdict(set)
    all_user_dislikes = defaultdict(set)

    if not os.path.exists(filename):
        return all_user_likes, all_user_dislikes

    fact_regex = re.compile(r"(likes|dislikes)\('([^']+)','([^']+)'\)\.")

    with open(filename, "r", encoding="utf-8") as f:
        for line in f:
            match = fact_regex.match(line.strip())
            if match:
                fact_type, user, genre = match.groups()
                user = user.lower()
                if fact_type == 'likes':
                    all_user_likes[user].add(genre)
                elif fact_type == 'dislikes':
                    all_user_dislikes[user].add(genre)

    return all_user_likes, all_user_dislikes

# --- [Clases Python (gestión simple)] ---

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

    def build_prolog_peli(self, filename=PELI_FILE, all_user_likes=None, all_user_dislikes=None):
        """
        Genera/reemplaza el fichero Prolog con TODOS los datos acumulados.
        (La misma función original)
        """
        all_user_likes = all_user_likes or {}
        all_user_dislikes = all_user_dislikes or {}

        with open(filename, "w", encoding="utf-8") as f:
            f.write("%% Base de conocimiento generada automáticamente\n\n")

            # 1. Hechos 'movie'
            for m in self.movies:
                # Escapar comillas simples en el título para Prolog
                safe_title = m.title.replace("'", "''")
                genres_list = "[" + ",".join(["'{}'".format(g) for g in m.genres]) + "]"
                f.write("movie('{}', {}).\n".format(safe_title, genres_list))
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
            f.write("    movie(Title, MovieGenres),\n")

            # FILTRO DE EXCLUSIÓN
            f.write("    \\+ (\n")
            f.write("        dislikes(U, DislikedGenre),\n")
            f.write("        member(DislikedGenre, MovieGenres)\n")
            f.write("    ),\n")

            # FILTRO DE INCLUSIÓN
            f.write("    (\n")
            f.write("        likes(U, LikedGenre),\n")
            f.write("        member(LikedGenre, MovieGenres)\n")
            f.write("    ).\n")

    def query_recommendations(self, username, peli_filename=PELI_FILE):
        """
        Llama a SWI-Prolog para pedir recomendaciones.
        Devuelve una lista de títulos únicos. (La misma función original)
        """
        safe_username = username.lower().replace("'", "")
        prolog_query = "findall(Title, recommend('{}', Title), L), write(L), nl, halt.".format(safe_username)
        cmd = ["swipl", "-q", "-s", peli_filename, "-g", prolog_query]
        
        try:
            # Usar 'timeout' en un entorno de GUI es crucial para evitar bloqueos
            res = subprocess.run(cmd, capture_output=True, text=True, check=True, timeout=10)
        except FileNotFoundError:
            raise RuntimeError("No se encontró 'swipl'. Asegurate de tener instalado SWI-Prolog y que 'swipl' esté en PATH.")
        except subprocess.CalledProcessError as e:
            error_output = e.stderr.strip()
            raise RuntimeError(f"Error ejecutando Prolog. Salida de error:\n{error_output}")
        except subprocess.TimeoutExpired:
            raise RuntimeError("La consulta a Prolog ha tardado demasiado (Timeout).")
            
        output = res.stdout.strip()
        pylist = []
        try:
            # Parseo más robusto
            # Eliminar la salida de 'Warning: ...' si Prolog la incluye
            if 'Warning:' in output:
                output = output[output.rfind('[') :]
            pylist = ast.literal_eval(output)
            pylist = [str(x) for x in pylist]
        except Exception:
            # Fallback (menos confiable, pero ayuda)
            s = output.strip()
            if s.startswith("[") and s.endswith("]"):
                inner = s[1:-1].strip()
                if inner:
                    # Intenta dividir por comas, limpiando las comillas
                    parts = [p.strip().strip("'\"") for p in inner.split(",")]
                    pylist = parts

        return list(set(pylist))

# --- [Lógica de la Interfaz Gráfica (Tkinter)] ---

class RecommenderApp:
    def __init__(self, master):
        self.master = master
        master.title("🎬 Sistema de Recomendación Prolog/Python")
        master.resizable(False, False) # Evita que se cambie el tamaño de la ventana
        
        # Inicializar datos
        self.all_user_likes, self.all_user_dislikes = load_existing_preferences()
        self.recommender = Recommender(MOVIES, USERS_WATCHED.copy())
        
        # Frame Principal
        self.main_frame = ttk.Frame(master, padding="20")
        self.main_frame.pack(fill='both', expand=True)
        
        # Título
        ttk.Label(self.main_frame, text="Sistema de Recomendación", font=("Helvetica", 16, "bold")).pack(pady=10)
        
        # Opciones
        self.recommend_button = ttk.Button(self.main_frame, text="⭐ Recomendar Películas", command=self.start_recommendation_flow)
        self.recommend_button.pack(pady=10, ipadx=20)
        
        self.db_button = ttk.Button(self.main_frame, text="📂 Ver Base de Datos de Usuarios", command=self.show_database)
        self.db_button.pack(pady=10, ipadx=20)

        self.exit_button = ttk.Button(self.main_frame, text="❌ Salir", command=master.quit)
        self.exit_button.pack(pady=20, ipadx=20)

    # --- FLUJO DE RECOMENDACIÓN ---
    
    def start_recommendation_flow(self):
        """Pide el nombre de usuario y luego inicia la encuesta de géneros."""
        username = simpledialog.askstring("Usuario", "Introduce tu nombre de usuario:", parent=self.master)
        if username:
            username = username.strip().lower()
            if not username:
                messagebox.showerror("Error", "El nombre de usuario no puede estar vacío.")
                return

            # Advertir si ya existe
            if username in self.all_user_likes.keys() or username in self.all_user_dislikes.keys():
                messagebox.showinfo("Aviso", f"El usuario '{username}' ya existe. Sus *nuevas* respuestas se añadirán a las anteriores.")

            self.current_user = username
            self.new_likes = set()
            self.new_dislikes = set()
            self.current_genre_index = 0
            
            # Crear ventana para la encuesta de géneros
            self.survey_window = tk.Toplevel(self.master)
            self.survey_window.title(f"Preferencias de {username.capitalize()}")
            self.survey_window.transient(self.master) # Mantenerla encima de la ventana principal
            self.survey_window.grab_set() # Bloquear interacción con la ventana principal
            
            self.survey_frame = ttk.Frame(self.survey_window, padding="15")
            self.survey_frame.pack(fill='both', expand=True)

            self.genre_label = ttk.Label(self.survey_frame, text="", font=("Helvetica", 12))
            self.genre_label.pack(pady=10)

            button_frame = ttk.Frame(self.survey_frame)
            button_frame.pack(pady=10)

            ttk.Button(button_frame, text="✅ Me gusta", command=lambda: self.record_preference("like")).pack(side=tk.LEFT, padx=10)
            ttk.Button(button_frame, text="❌ No me gusta", command=lambda: self.record_preference("dislike")).pack(side=tk.LEFT, padx=10)
            
            self.show_next_genre()

    def show_next_genre(self):
        """Muestra el siguiente género para la encuesta."""
        if self.current_genre_index < len(AVAILABLE_GENRES):
            genre = AVAILABLE_GENRES[self.current_genre_index]
            self.genre_label.config(text=f"¿Te gusta el género **{genre.upper()}**?\n({self.current_genre_index + 1}/{len(AVAILABLE_GENRES)})")
        else:
            self.survey_window.destroy()
            self.process_recommendation()

    def record_preference(self, preference):
        """Registra la preferencia del usuario para el género actual."""
        genre = AVAILABLE_GENRES[self.current_genre_index]
        if preference == "like":
            self.new_likes.add(genre)
        elif preference == "dislike":
            self.new_dislikes.add(genre)
            
        self.current_genre_index += 1
        self.show_next_genre()
        
    def process_recommendation(self):
        """Fusiona los datos, actualiza Prolog y pide recomendaciones."""
        
        # 1. Actualizar el historial acumulativo (fusión)
        self.all_user_likes[self.current_user].update(self.new_likes)
        self.all_user_dislikes[self.current_user].update(self.new_dislikes)
        
        likes_count = len(self.all_user_likes[self.current_user])

        if likes_count == 0:
            messagebox.showinfo("Recomendación", "No registraste ningún género que te guste. No se puede recomendar.")
            return

        try:
            # 2. Generar KB con datos persistentes + datos actuales (GUARDA LA PERSISTENCIA)
            self.recommender.build_prolog_peli(filename=PELI_FILE, 
                                               all_user_likes=self.all_user_likes, 
                                               all_user_dislikes=self.all_user_dislikes)
            
            # 3. Pedir recomendaciones
            recs = self.recommender.query_recommendations(self.current_user, peli_filename=PELI_FILE)
            
            # 4. Mostrar resultados
            self.show_recommendations_result(self.current_user, recs)

        except RuntimeError as e:
            messagebox.showerror("Error de Sistema", str(e))

    def show_recommendations_result(self, username, recs):
        """Muestra las recomendaciones en una nueva ventana."""
        result_window = tk.Toplevel(self.master)
        result_window.title(f"Recomendaciones para {username.capitalize()}")
        result_window.transient(self.master)
        result_window.grab_set()

        frame = ttk.Frame(result_window, padding="15")
        frame.pack(fill='both', expand=True)

        ttk.Label(frame, text=f"Resultados para {username.capitalize()}:", font=("Helvetica", 14, "bold")).pack(pady=10)
        
        list_frame = ttk.Frame(frame)
        list_frame.pack(fill='both', expand=True, padx=10, pady=5)
        
        scrollbar = Scrollbar(list_frame)
        scrollbar.pack(side=tk.RIGHT, fill=tk.Y)
        
        result_listbox = Listbox(list_frame, yscrollcommand=scrollbar.set, width=50, height=15)
        
        if recs:
            for i, m in enumerate(recs, 1):
                # Encontrar el objeto película para mostrar géneros
                movie_obj = next((m_obj for m_obj in self.recommender.movies if m_obj.title == m), None)
                genres_str = f" ({', '.join(movie_obj.genres)})" if movie_obj else ""
                result_listbox.insert(tk.END, f"{i}. {m}{genres_str}")
        else:
            result_listbox.insert(tk.END, "(No se encontró ninguna película que cumpla el criterio.)")
            
        result_listbox.pack(side=tk.LEFT, fill='both', expand=True)
        scrollbar.config(command=result_listbox.yview)

        ttk.Button(frame, text="Cerrar", command=result_window.destroy).pack(pady=10)

    # --- FLUJO DE BASE DE DATOS ---

    def show_database(self):
        """Muestra la lista de usuarios persistentes."""
        # Recargar para asegurar que los datos estén frescos
        self.all_user_likes, self.all_user_dislikes = load_existing_preferences()
        existing_users = sorted(list(set(self.all_user_likes.keys()) | set(self.all_user_dislikes.keys())))

        if not existing_users:
            messagebox.showinfo("Base de Datos", "No hay usuarios registrados en la base de datos persistente.")
            return

        db_window = tk.Toplevel(self.master)
        db_window.title("Base de Datos de Usuarios")
        db_window.transient(self.master)
        db_window.grab_set()

        frame = ttk.Frame(db_window, padding="15")
        frame.pack(fill='both', expand=True)

        ttk.Label(frame, text="Usuarios Registrados:", font=("Helvetica", 14, "bold")).pack(pady=10)

        list_frame = ttk.Frame(frame)
        list_frame.pack(fill='both', expand=True, padx=10, pady=5)

        scrollbar = Scrollbar(list_frame)
        scrollbar.pack(side=tk.RIGHT, fill=tk.Y)

        self.user_listbox = Listbox(list_frame, yscrollcommand=scrollbar.set, width=40, height=15)
        for user in existing_users:
            self.user_listbox.insert(tk.END, user.capitalize())

        self.user_listbox.pack(side=tk.LEFT, fill='both', expand=True)
        scrollbar.config(command=self.user_listbox.yview)

        ttk.Button(frame, text="Ver Preferencias", 
                   command=self.show_user_preferences).pack(pady=10)
        ttk.Button(frame, text="Cerrar", command=db_window.destroy).pack(pady=5)
        
        self.user_listbox.bind('<<ListboxSelect>>', self.show_user_preferences)


    def show_user_preferences(self, event=None):
        """Muestra los géneros preferidos/disgustados del usuario seleccionado."""
        try:
            selected_index = self.user_listbox.curselection()[0]
            selected_user = self.user_listbox.get(selected_index).lower()
        except IndexError:
            messagebox.showwarning("Selección", "Por favor, selecciona un usuario de la lista.")
            return

        likes = self.all_user_likes.get(selected_user, set())
        dislikes = self.all_user_dislikes.get(selected_user, set())

        info = f"--- **{selected_user.capitalize()}** ---\n\n"
        
        info += "✅ Géneros que le **Gustan**:\n"
        if likes:
            info += ", ".join(sorted(list(likes)))
        else:
            info += "(Ninguno registrado)"
            
        info += "\n\n❌ Géneros que le **Disgustan**:\n"
        if dislikes:
            info += ", ".join(sorted(list(dislikes)))
        else:
            info += "(Ninguno registrado)"

        # Mostrar la información en un nuevo cuadro de diálogo
        messagebox.showinfo(f"Preferencias de {selected_user.capitalize()}", info)
        
# --- [Ejecución Principal] ---
if __name__ == "__main__":
    root = tk.Tk()
    app = RecommenderApp(root)
    root.mainloop()
