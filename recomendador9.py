#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import tkinter as tk
from tkinter import messagebox, simpledialog
import subprocess
import ast
from collections import defaultdict
import os
import sys

# -----------------------
# Config / catálogo Prolog
# -----------------------
generosDisp = ["sci-fi", "adventure", "comedia", "drama", "fantasy", "thriller",
               "crime", "horror", "action", "mystery", "romance", "animation"]

archivoPeliculas = "catalogoHas3.pl"  # asegúrate que exista

# DB en memoria
usuariosDB = {}  # clave = correo_sin_arroba -> {nombre, apellido, password, edad, pais}

# -----------------------
# Clase Recomendador (usa funciones que ya tenías)
# -----------------------
class Recomendador:
    def construirHechos(self, archivo, gustosTodos, disgustosTodos, edadesUsuarios=None, paisesUsuarios=None):
        """Escribe o reemplaza bloque de hechos de usuarios en el archivo Prolog."""
        if edadesUsuarios is None: edadesUsuarios = {}
        if paisesUsuarios is None: paisesUsuarios = {}

        try:
            with open(archivo, "r", encoding="utf-8") as f:
                contenido = f.read()
        except FileNotFoundError:
            raise RuntimeError(f"El archivo Prolog '{archivo}' no existe. Crea uno con movies/series y reglas recommend/..")

        cabecera_necesaria = (
            ":- discontiguous movie/2.\n"
            ":- discontiguous movie_age/2.\n"
            ":- discontiguous movie_countries/2.\n"
            ":- discontiguous serieG/2.\n"
            ":- discontiguous serieE/2.\n"
            ":- discontiguous serieP/2.\n"
            ":- dynamic likes/2, dislikes/2, user_age/2, user_country/2.\n\n"
        )

        contenido_mod = contenido
        if ":- discontiguous movie/2." not in contenido_mod:
            contenido_mod = cabecera_necesaria + contenido_mod

        start = "%% --- START USER FACTS INJECTION ---"
        end = "%% --- END USER FACTS INJECTION ---"

        lines = [f"{start}\n", "%% --- GUSTOS DE USUARIOS (Acumulados en la sesión) ---\n"]
        for u, gens in gustosTodos.items():
            ua = str(u).replace("'", "").lower()
            for g in gens:
                lines.append("likes('{}','{}').\n".format(ua, g))
        lines.append("\n%% --- DISGUSTOS DE USUARIOS ---\n")
        for u, gens in disgustosTodos.items():
            ua = str(u).replace("'", "").lower()
            for g in gens:
                lines.append("dislikes('{}','{}').\n".format(ua, g))
        lines.append("\n%% --- METADATOS DE USUARIOS ---\n")
        for u, edad in (edadesUsuarios or {}).items():
            ua = str(u).replace("'", "").lower()
            lines.append("user_age('{}',{}).\n".format(ua, int(edad)))
        for u, pais in (paisesUsuarios or {}).items():
            ua = str(u).replace("'", "").lower()
            pa = str(pais).strip().lower().replace(" ", "_")
            lines.append("user_country('{}','{}').\n".format(ua, pa))
        lines.append("\n" + end + "\n")
        bloque = "".join(lines)

        if start in contenido_mod and end in contenido_mod:
            pre = contenido_mod.split(start)[0]
            post = contenido_mod.split(end, 1)[1] if end in contenido_mod else ""
            nuevo = pre + bloque + post
        else:
            if not contenido_mod.endswith("\n"): contenido_mod += "\n"
            nuevo = contenido_mod + "\n" + bloque

        with open(archivo, "w", encoding="utf-8") as f:
            f.write(nuevo)

    def consultarRec(self, usuario, tipo="pelicula", archivo=archivoPeliculas, timeout=6):
        """
        Intenta obtener (Title,Director,Rating,Genres) con recommend/5 o recommend_serie/5.
        Si no hay tuplas, hace fallback a pedir sólo Title.
        Devuelve lista sin duplicados (preserva orden).
        """
        usuarioSeguro = str(usuario).lower().replace("'", "")
        if tipo == "serie":
            consulta_tupla = "findall((Title,Director,Rating,Genres), recommend_serie('{}', Title, Director, Rating, Genres), L), write(L), nl, halt.".format(usuarioSeguro)
            consulta_simple = "findall(Title, recommend_serie('{}', Title), L), write(L), nl, halt.".format(usuarioSeguro)
        else:
            consulta_tupla = "findall((Title,Director,Rating,Genres), recommend('{}', Title, Director, Rating, Genres), L), write(L), nl, halt.".format(usuarioSeguro)
            consulta_simple = "findall(Title, recommend('{}', Title), L), write(L), nl, halt.".format(usuarioSeguro)

        # Primero intento la consulta que devuelve tuplas completas
        for consulta in (consulta_tupla, consulta_simple):
            cmd = ["swipl", "-q", "-s", archivo, "-g", consulta]
            try:
                resultado = subprocess.run(cmd, capture_output=True, text=True, timeout=timeout)
            except FileNotFoundError:
                raise RuntimeError("swipl no encontrado. Instala SWI-Prolog y pon 'swipl' en PATH.")
            except subprocess.TimeoutExpired:
                raise RuntimeError("La consulta a Prolog tardó demasiado (timeout).")

            if resultado.stderr and resultado.stderr.strip():
                # mostramos advertencias pero seguimos intentando
                print("[Prolog stderr]:", resultado.stderr.strip())

            salida = resultado.stdout.strip()
            if not salida:
                continue

            # parseo
            try:
                lista = ast.literal_eval(salida)
                raw = lista if isinstance(lista, list) else [lista]
            except Exception:
                s = salida.strip()
                if s.startswith("[") and s.endswith("]"):
                    interno = s[1:-1].strip()
                    if not interno:
                        raw = []
                    else:
                        # separar por '), (' si tuplas, o por comas simples si strings
                        # simplificado: separo por '),(' primero
                        if ")," in interno and "(" in interno:
                            parts = interno.split("),")
                            raw = []
                            for p in parts:
                                p = p.strip().lstrip("(").rstrip(")").strip()
                                campos = [c.strip().strip("'\"") for c in p.split(",")]
                                raw.append(tuple(campos))
                        else:
                            raw = [p.strip().strip("'\"") for p in interno.split(",") if p.strip()]
                else:
                    raw = []

            # normalizar raw: si vienen strings o tuplas de strings, devolver
            # eliminar duplicados por título (case-insensitive)
            seen = set()
            final = []
            for it in raw:
                if isinstance(it, (list, tuple)) and len(it) >= 1:
                    title = str(it[0])
                else:
                    title = str(it)
                key = title.strip().lower()
                if key in seen: continue
                seen.add(key)
                final.append(it)
            if final:
                return final

        return []

# -----------------------
# Interfaz gráfica (Netflix-like)
# -----------------------
class HasklixGUI:
    def __init__(self, root):
        self.root = root
        self.root.title("Hasklix - Redflix")
        self.root.geometry("900x700")
        self.root.configure(bg="black")
        self.recomendador = Recomendador()

        # datos de sesión
        self.gustos = defaultdict(set)
        self.disgustos = defaultdict(set)
        self.edades = {}
        self.paises = {}
        self.usuarioActual = None

        self.login_screen()

    # ----------------- pantallas -----------------
    def clear(self):
        for w in self.root.winfo_children():
            w.destroy()

    def login_screen(self):
        self.clear()
        # Header
        tk.Label(self.root, text="Hasklix", font=("Helvetica", 42, "bold"), fg="red", bg="black").pack(pady=20)
        frame = tk.Frame(self.root, bg="black")
        frame.pack(pady=10)

        tk.Label(frame, text="Correo (sin @):", fg="white", bg="black").grid(row=0, column=0, sticky="e", padx=8, pady=6)
        self.email_entry = tk.Entry(frame, width=30); self.email_entry.grid(row=0, column=1, pady=6)
        tk.Label(frame, text="Contraseña:", fg="white", bg="black").grid(row=1, column=0, sticky="e", padx=8, pady=6)
        self.pass_entry = tk.Entry(frame, show="*", width=30); self.pass_entry.grid(row=1, column=1, pady=6)

        tk.Button(self.root, text="Ingresar", bg="red", fg="white", width=20, command=self.login).pack(pady=12)
        tk.Button(self.root, text="Crear cuenta", bg="#333333", fg="white", width=20, command=self.register_screen).pack()

    def register_screen(self):
        self.clear()
        tk.Label(self.root, text="Crear cuenta", font=("Helvetica", 28, "bold"), fg="red", bg="black").pack(pady=10)
        form = tk.Frame(self.root, bg="black"); form.pack(pady=6)

        labels = ["Nombre","Apellido","Correo (sin @)","Contraseña","Edad","País"]
        self.reg_vars = {}
        for i, lab in enumerate(labels):
            tk.Label(form, text=lab+":", fg="white", bg="black").grid(row=i, column=0, sticky="e", padx=6, pady=6)
            ent = tk.Entry(form, width=30)
            if lab == "Contraseña": ent.config(show="*")
            ent.grid(row=i, column=1, pady=6)
            self.reg_vars[lab] = ent

        tk.Button(self.root, text="Registrar", bg="red", fg="white", width=20, command=self.create_account).pack(pady=12)
        tk.Button(self.root, text="Volver", bg="#333333", fg="white", width=20, command=self.login_screen).pack()

    def create_account(self):
        data = {k: self.reg_vars[k].get().strip() for k in self.reg_vars}
        if not data["Nombre"] or not data["Apellido"] or not data["Correo (sin @)"]:
            messagebox.showerror("Error", "Completa nombre, apellido y correo.")
            return
        user = data["Correo (sin @)"].lower()
        if "@" in user:
            messagebox.showerror("Error", "No incluyas @ en el correo de registro.")
            return
        if user in usuariosDB:
            messagebox.showerror("Error","Usuario ya registrado.")
            return
        if len(data["Contraseña"]) < 6:
            messagebox.showerror("Error","Contraseña mínimo 6 caracteres.")
            return
        try:
            edad = int(data["Edad"])
        except:
            messagebox.showerror("Error","Edad inválida.")
            return
        usuariosDB[user] = {"nombre": data["Nombre"], "apellido": data["Apellido"],
                            "password": data["Contraseña"], "edad": edad, "pais": data["País"]}
        messagebox.showinfo("Listo","Cuenta creada. Inicia sesión.")
        self.login_screen()

    def login(self):
        email = self.email_entry.get().strip().lower()
        pwd = self.pass_entry.get().strip()
        user = usuariosDB.get(email)
        if not user or user["password"] != pwd:
            messagebox.showerror("Error","Correo o contraseña incorrectos")
            return
        self.usuarioActual = email
        # cargar edad/pais en caches
        self.edades[email] = user["edad"]
        self.paises[email] = user["pais"].strip().lower().replace(" ", "_")
        self.select_type_screen()

    def select_type_screen(self):
        self.clear()
        nombre = usuariosDB[self.usuarioActual]["nombre"]
        tk.Label(self.root, text=f"Hola, {nombre}", font=("Helvetica", 22, "bold"), fg="white", bg="black").pack(pady=8)
        tk.Label(self.root, text="¿Qué quieres ver?", font=("Helvetica", 18), fg="red", bg="black").pack(pady=10)
        btn_frame = tk.Frame(self.root, bg="black"); btn_frame.pack(pady=8)
        tk.Button(btn_frame, text="Películas", bg="red", fg="white", width=20, command=lambda:self.genre_screen("pelicula")).grid(row=0, column=0, padx=10, pady=6)
        tk.Button(btn_frame, text="Series", bg="red", fg="white", width=20, command=lambda:self.genre_screen("serie")).grid(row=0, column=1, padx=10, pady=6)
        tk.Button(self.root, text="Cerrar sesión", bg="#333333", fg="white", width=20, command=self.logout).pack(pady=18)

    def logout(self):
        self.usuarioActual = None
        self.gustos.clear()
        self.disgustos.clear()
        self.login_screen()

    def genre_screen(self, tipo):
        self.clear()
        tk.Label(self.root, text=f"Selecciona géneros ({tipo})", font=("Helvetica", 18, "bold"), fg="red", bg="black").pack(pady=8)
        container = tk.Frame(self.root, bg="black"); container.pack(fill="both", expand=True, padx=20, pady=8)
        canvas = tk.Canvas(container, bg="black", highlightthickness=0)
        scrollbar = tk.Scrollbar(container, orient="vertical", command=canvas.yview)
        scroll_frame = tk.Frame(canvas, bg="black")
        scroll_frame.bind("<Configure>", lambda e: canvas.configure(scrollregion=canvas.bbox("all")))
        canvas.create_window((0,0), window=scroll_frame, anchor="nw")
        canvas.configure(yscrollcommand=scrollbar.set)
        canvas.pack(side="left", fill="both", expand=True)
        scrollbar.pack(side="right", fill="y")

        self.genre_vars = {}
        for g in generosDisp:
            v = tk.IntVar()
            chk = tk.Checkbutton(scroll_frame, text=g.title(), variable=v, fg="white", bg="black", selectcolor="red", activebackground="black", activeforeground="white", font=("Arial", 12))
            chk.pack(anchor="w", padx=8, pady=4)
            self.genre_vars[g] = v

        btn_frame = tk.Frame(self.root, bg="black"); btn_frame.pack(pady=10)
        tk.Button(btn_frame, text="Ver recomendaciones", bg="red", fg="white", width=20, command=lambda:self.show_recommendations(tipo)).grid(row=0, column=0, padx=8)
        tk.Button(btn_frame, text="Volver", bg="#333333", fg="white", width=20, command=self.select_type_screen).grid(row=0, column=1, padx=8)

    def show_recommendations(self, tipo):
        seleccion = {g for g,v in self.genre_vars.items() if v.get()==1}
        self.gustos[self.usuarioActual] = seleccion
        self.disgustos[self.usuarioActual] = {g for g in generosDisp if g not in seleccion}
        self.edades[self.usuarioActual] = usuariosDB[self.usuarioActual]["edad"]
        self.paises[self.usuarioActual] = usuariosDB[self.usuarioActual]["pais"].strip().lower().replace(" ", "_")

        if not seleccion:
            messagebox.showinfo("Aviso","Selecciona al menos un género.")
            return

        # Inyectar hechos (solo la sesión actual)
        gustosTodos = {self.usuarioActual: self.gustos[self.usuarioActual]}
        disgustosTodos = {self.usuarioActual: self.disgustos[self.usuarioActual]}
        edades = {self.usuarioActual: self.edades[self.usuarioActual]}
        paises = {self.usuarioActual: self.paises[self.usuarioActual]}

        try:
            self.recomendador.construirHechos(archivoPeliculas, gustosTodos, disgustosTodos, edades, paises)
        except RuntimeError as e:
            messagebox.showerror("Error", str(e))
            return

        try:
            recs = self.recomendador.consultarRec(self.usuarioActual, tipo=tipo, archivo=archivoPeliculas)
        except RuntimeError as e:
            messagebox.showerror("Error al consultar Prolog", str(e))
            return

        # Mostrar pantalla de resultados
        self.clear()
        tk.Label(self.root, text=f"Recomendaciones ({tipo})", font=("Helvetica", 20, "bold"), fg="red", bg="black").pack(pady=10)
        if not recs:
            tk.Label(self.root, text="No se encontraron resultados.", fg="white", bg="black").pack(pady=10)
        else:
            # Scrollable list
            frame = tk.Frame(self.root, bg="black"); frame.pack(fill="both", expand=True, padx=20, pady=6)
            canvas = tk.Canvas(frame, bg="black", highlightthickness=0)
            scrollbar = tk.Scrollbar(frame, orient="vertical", command=canvas.yview)
            list_frame = tk.Frame(canvas, bg="#111111")
            list_frame.bind("<Configure>", lambda e: canvas.configure(scrollregion=canvas.bbox("all")))
            canvas.create_window((0,0), window=list_frame, anchor="nw", width=820)
            canvas.configure(yscrollcommand=scrollbar.set)
            canvas.pack(side="left", fill="both", expand=True)
            scrollbar.pack(side="right", fill="y")

            for item in recs:
                # item puede ser tuple (Title,Director,Rating,Genres) o string Title
                if isinstance(item, (list, tuple)):
                    title = item[0]
                    director = item[1] if len(item) > 1 else ""
                    rating = item[2] if len(item) > 2 else ""
                    genres = item[3] if len(item) > 3 else ""
                else:
                    title = str(item)
                    director = ""
                    rating = ""
                    genres = ""

                btn_text = f"{title}  {'| ' + str(director) if director else ''} {'| ⭐ '+str(rating) if rating else ''}"
                b = tk.Button(list_frame, text=btn_text, anchor="w", width=110, bg="#222222", fg="white",
                              activebackground="#333333", command=lambda it=item: self.show_details(it))
                b.pack(fill="x", padx=6, pady=6)

        tk.Button(self.root, text="Volver", bg="#333333", fg="white", width=18, command=lambda:self.genre_screen(tipo)).pack(pady=10)
        tk.Button(self.root, text="Cerrar sesión", bg="#333333", fg="white", width=18, command=self.logout).pack()

    def show_details(self, item):
        # item puede venir como tuple (Title,Director,Rating,Genres) o como str Title
        if isinstance(item, (list, tuple)) and len(item) >= 1:
            title = str(item[0])
            director = str(item[1]) if len(item) > 1 else "Desconocido"
            rating = str(item[2]) if len(item) > 2 else "N/A"
            genres = item[3] if len(item) > 3 else []
        else:
            title = str(item)
            director = "Desconocido"
            rating = "N/A"
            genres = []

        genres_str = ", ".join(genres) if hasattr(genres, "__iter__") and not isinstance(genres, str) else str(genres)

        messagebox.showinfo(title, f"Título: {title}\nGéneros: {genres_str}\nDirector: {director}\nPuntuación: {rating}")

# -----------------------
# Ejecutar app
# -----------------------
if __name__ == "__main__":
    # comprobaciones iniciales
    if not os.path.exists(archivoPeliculas):
        print(f"AVISO: No se encontró '{archivoPeliculas}'. Crea el archivo Prolog con películas y series y las reglas recommend/.. y recommend_serie/..")
    root = tk.Tk()
    app = HasklixGUI(root)
    root.mainloop()
