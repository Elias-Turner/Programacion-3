#!/usr/bin/env python3
"""
Sistema simple de recomendación (FINAL)

- Prolog: Contiene la Base de Datos de MOVIES y la Regla de Recomendación.
- Python: Gestiona la entrada de usuarios, INYECTA Y ACTUALIZA los hechos de
          gustos en Prolog, y consulta la recomendación.

*** ASUME que el archivo 'pelicula.pl' YA EXISTE y contiene los hechos 'movie/2' y la regla 'recommend/2'. ***

Requiere: SWI-Prolog (comando 'swipl')
"""

import subprocess
import ast
from collections import defaultdict
import os.path
import sys

# La lista de géneros sigue siendo necesaria para la encuesta al usuario en Python
AVAILABLE_GENRES = [
    "sci-fi", "adventure", "comedy", "drama", "fantasy", "thriller", "crime", "horror",
    "action", "mystery", "romance", "animation"
]

PELI_FILE = "pelicula.pl"


# --- [CLASES PYTHON SIMPLIFICADAS] ---

class Recommender:
    """Clase para gestionar la interacción con Prolog."""

    def build_prolog_user_facts(self, filename, all_user_likes, all_user_dislikes):
        """
        ACTUALIZA el archivo Prolog, reescribiendo la parte de los hechos de usuario,
        pero manteniendo la Base de Datos de Películas y la Regla de Recomendación.
        """
        
        # Leemos el contenido base de pelicula.pl (debe contener películas y reglas)
        try:
            with open(filename, "r", encoding="utf-8") as f:
                content = f.read()
        except FileNotFoundError:
            raise RuntimeError(f"El archivo base de películas '{filename}' no existe. Por favor, créalo con los hechos 'movie/2' y la regla 'recommend/2'.")
            
        # 1. Definir los separadores que asumimos que existen en el archivo:
        # Buscamos dónde termina la definición de la regla 'recommend' (o dónde empieza la zona de inyección).
        # Usamos un marcador de comentario para delimitar la zona de inyección de Python.
        
        # Marcador de inicio de la zona de inyección (debe estar en pelicula.pl)
        START_MARKER = "%% --- START USER FACTS INJECTION ---"
        
        if START_MARKER not in content:
            # Si el archivo no tiene el marcador, lo añadimos al final.
            base_content = content.strip()
        else:
            # Si el archivo tiene el marcador, truncamos el contenido allí.
            base_content = content[:content.find(START_MARKER)].strip()

        # 2. Generamos los nuevos hechos de gustos
        user_facts = []
        user_facts.append(f"\n{START_MARKER}\n")
        
        # Gustos
        user_facts.append("%% --- GUSTOS DE USUARIOS (Acumulados en la sesión) ---\n")
        for uname, genset in all_user_likes.items():
            for g in genset:
                user_facts.append("likes('{}','{}').\n".format(uname, g))
        
        # Disgustos
        user_facts.append("\n")
        for uname, genset in all_user_dislikes.items():
            for g in genset:
                user_facts.append("dislikes('{}','{}').\n".format(uname, g))
        
        user_facts.append("\n%% --- END USER FACTS INJECTION ---\n")

        # 3. Reescribimos el archivo completo con el contenido base + los nuevos hechos
        with open(filename, "w", encoding="utf-8") as f:
            f.write(base_content)
            f.write("".join(user_facts))

        print(f"[Python] Hechos de likes/dislikes del usuario actual escritos en '{filename}'.")


    def query_recommendations(self, username, peli_filename=PELI_FILE):
        """Llama a SWI-Prolog para pedir recomendaciones."""
        safe_username = username.lower().replace("'", "")
        # Consulta de Prolog para obtener todos los títulos recomendados
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
            raise RuntimeError(f"Error ejecutando Prolog. Asegúrate de que '{peli_filename}' sea un archivo Prolog válido y completo.")
            
        output = res.stdout.strip()
        pylist = []
        try:
            # Parseamos la salida de la lista de Prolog
            pylist = ast.literal_eval(output)
            # Revertir el escape de comillas si fue necesario
            pylist = [str(x).replace("''", "'") for x in pylist]
        except Exception:
            # Fallback de parseo
            s = output.strip()
            if s.startswith("[") and s.endswith("]"):
                inner = s[1:-1].strip()
                if inner:
                    parts = [p.strip().strip("'\"").replace("''", "'") for p in inner.split(",")]
                    pylist = parts
        
        return list(set(pylist))


# --- [FUNCIONES DE ENTRADA Y DEMO] ---

def get_user_input(existing_users):
    """Pide nombre y gustos, asegurando nombre de usuario único en la sesión."""
    print("\n--- INICIO DE NUEVO USUARIO ---")
    while True:
        username = input("Por favor, introduce tu nombre de usuario: ").strip().lower()
        if not username:
            print("El nombre de usuario no puede estar vacío.")
        elif username in existing_users:
            print(f"El usuario '{username}' ya fue atendido en esta sesión. Por favor, usa un nombre diferente.")
        else:
            break

    user_likes = set()
    user_dislikes = set()
    print(f"\nAhora, vamos a preguntarte sobre {len(AVAILABLE_GENRES)} géneros.")
    print("Responde 'si' o 'no' (o 's'/'n'). **'No' excluye películas con ese género.**")

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

def demo():
    print("=== Sistema de Recomendación (Prolog como BD Central) ===")
    
    # Comprobación de existencia del archivo (obligatoria ahora)
    if not os.path.exists(PELI_FILE):
        print("\n[ERROR CRÍTICO]")
        print(f"El archivo '{PELI_FILE}' no existe. Este archivo debe contener los hechos 'movie/2' y la regla 'recommend/2'.")
        print("Por favor, crea el archivo con el contenido necesario antes de ejecutar.")
        sys.exit(1)

    # Historial acumulativo dentro de la sesión
    all_user_likes = defaultdict(set)
    all_user_dislikes = defaultdict(set)
    
    r = Recommender() 
    
    while True:
        try:
            # 1. Obtener entrada de usuario
            new_user, new_likes, new_dislikes = get_user_input(all_user_likes.keys())
            
            # 2. Actualizar el historial acumulativo
            all_user_likes[new_user].update(new_likes)
            all_user_dislikes[new_user].update(new_dislikes)

            if not new_likes:
                print("\n[AVISO] No elegiste ningún género 'si'. Saltando la recomendación.")
            else:
                # 3. Actualizar el archivo Prolog con los hechos del usuario actual y anteriores
                r.build_prolog_user_facts(PELI_FILE, all_user_likes, all_user_dislikes)
                
                # 4. Pedir y obtener recomendaciones de Prolog
                recs = r.query_recommendations(new_user, peli_filename=PELI_FILE)
                
                print(f"\n--- RECOMENDACIONES para **{new_user}** ---")
                
                # 5. Mostrar resultados
                if recs:
                    for i,m in enumerate(recs, 1):
                        print(f"  {i}. **{m}**")
                    print("\n*Recuerda: Estas películas cumplen el criterio de exclusión y al menos un género que te gusta.*")
                else:
                    print("  (No se encontró ninguna película que cumpla el criterio.)")
        
        except RuntimeError as e:
            print("\nERROR:", e)
            print("El programa se detiene.")
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
