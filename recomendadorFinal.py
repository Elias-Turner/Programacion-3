#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import tkinter as tk
from tkinter import messagebox, simpledialog
import subprocess
import ast
from collections import defaultdict
import os
import sys

# Config / catálogo Prolog
generosDisp = ["sci-fi", "adventure", "comedia", "drama", "fantasy", "thriller",
               "crime", "horror", "action", "mystery", "romance", "animation"]

archivoPeliculas = "catalogo.pl"   # <-- nombre en camelCase

# DB en memoria
usuariosDB = {}  # clave = correo_sin_arroba -> {nombre, apellido, password, edad, pais}

# Clase Recomendador (nombres en método actualizados a camelCase)
class Recomendador:
    def construirHechos(self, archivo, gustosTodos, disgustosTodos, edadesUsuarios=None, paisesUsuarios=None):
        """Escribe o reemplaza bloque de hechos de usuarios en el archivo Prolog."""
        if edadesUsuarios is None:
            edadesUsuarios = {}
        if paisesUsuarios is None:
            paisesUsuarios = {}

        try:
            with open(archivo, "r", encoding="utf-8") as f:
                contenido = f.read()
        except FileNotFoundError:
            raise RuntimeError(f"El archivo Prolog '{archivo}' no existe. Crea uno con movies/series y reglas recommend/..")

        cabeceraNecesaria = (
            ":- discontiguous movie/4.\n"
            ":- discontiguous movie_age/2.\n"
            ":- discontiguous movie_countries/2.\n"
            ":- discontiguous serie/5.\n"
            ":- discontiguous serie_age/2.\n"
            ":- discontiguous serie_countries/2.\n"
            ":- dynamic likes/2, dislikes/2, user_age/2, user_country/2.\n\n"
        )

        contenidoMod = contenido
        if ":- discontiguous movie/4." not in contenidoMod:
            contenidoMod = cabeceraNecesaria + contenidoMod

        inicio = "%% --- START USER FACTS INJECTION ---"
        fin = "%% --- END USER FACTS INJECTION ---"

        lines = [f"{inicio}\n", "%% --- GUSTOS DE USUARIOS (Acumulados en la sesión) ---\n"]
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
        lines.append("\n" + fin + "\n")
        bloque = "".join(lines)

        if inicio in contenidoMod and fin in contenidoMod:
            pre = contenidoMod.split(inicio)[0]
            post = contenidoMod.split(fin, 1)[1] if fin in contenidoMod else ""
            nuevo = pre + bloque + post
        else:
            if not contenidoMod.endswith("\n"):
                contenidoMod += "\n"
            nuevo = contenidoMod + "\n" + bloque

        with open(archivo, "w", encoding="utf-8") as f:
            f.write(nuevo)

    def consultarRec(self, usuario, tipo="pelicula", archivo=archivoPeliculas, timeout=6):
        """
        Intenta obtener:
          - para peliculas: (Title,Director,Rating,Genres)
          - para series:   (Title,Director,Rating,Genres,Seasons)
        Si no hay tuplas, hace fallback a pedir sólo Title.
        Devuelve lista sin duplicados (preserva orden).
        """
        usuarioSeguro = str(usuario).lower().replace("'", "")
        if tipo == "serie":
            consultaTupla = "findall((Title,Director,Rating,Genres,Seasons), recommend_serie('{}', Title, Director, Rating, Genres, Seasons), L), write(L), nl, halt.".format(usuarioSeguro)
            consultaSimple = "findall(Title, recommend_serie('{}', Title), L), write(L), nl, halt.".format(usuarioSeguro)
        else:
            consultaTupla = "findall((Title,Director,Rating,Genres), recommend('{}', Title, Director, Rating, Genres), L), write(L), nl, halt.".format(usuarioSeguro)
            consultaSimple = "findall(Title, recommend('{}', Title), L), write(L), nl, halt.".format(usuarioSeguro)

        def splitTopLevelItems(s: str):
            items = []
            cur = []
            depthParen = 0
            depthBrack = 0
            i = 0
            while i < len(s):
                ch = s[i]
                if ch == "(":
                    depthParen += 1
                    cur.append(ch)
                elif ch == ")":
                    depthParen = max(depthParen - 1, 0)
                    cur.append(ch)
                elif ch == "[":
                    depthBrack += 1
                    cur.append(ch)
                elif ch == "]":
                    depthBrack = max(depthBrack - 1, 0)
                    cur.append(ch)
                elif ch == "," and depthParen == 0 and depthBrack == 0:
                    token = "".join(cur).strip()
                    if token:
                        items.append(token)
                    cur = []
                else:
                    cur.append(ch)
                i += 1
            last = "".join(cur).strip()
            if last:
                items.append(last)
            return items

        for consulta in (consultaTupla, consultaSimple):
            cmd = ["swipl", "-q", "-s", archivo, "-g", consulta]
            try:
                resultado = subprocess.run(cmd, capture_output=True, text=True, timeout=timeout)
            except FileNotFoundError:
                raise RuntimeError("swipl no encontrado. Instala SWI-Prolog y pon 'swipl' en PATH.")
            except subprocess.TimeoutExpired:
                raise RuntimeError("La consulta a Prolog tardó demasiado (timeout).")

            if resultado.stderr and resultado.stderr.strip():
                print("[Prolog stderr]:", resultado.stderr.strip())

            salida = resultado.stdout.strip()
            if not salida:
                continue

            try:
                lista = ast.literal_eval(salida)
                raw = lista if isinstance(lista, list) else [lista]
            except Exception:
                s = salida.strip()
                raw = []
                if s.startswith("[") and s.endswith("]"):
                    interno = s[1:-1].strip()
                    if interno:
                        elements = splitTopLevelItems(interno)
                        for el in elements:
                            el = el.strip()
                            try:
                                parsed = ast.literal_eval(el)
                                raw.append(parsed)
                                continue
                            except Exception:
                                pass
                            if el.startswith("(") and el.endswith(")"):
                                inner = el[1:-1].strip()
                                parts = splitTopLevelItems(inner)
                                parsedParts = []
                                for p in parts:
                                    p = p.strip()
                                    try:
                                        parsedP = ast.literal_eval(p)
                                    except Exception:
                                        parsedP = p.strip().strip("'\"")
                                    parsedParts.append(parsedP)
                                raw.append(tuple(parsedParts))
                            else:
                                token = el.strip().strip("'\"")
                                raw.append(token)
                else:
                    parts = splitTopLevelItems(s)
                    for p in parts:
                        p = p.strip().strip("'\"")
                        if p:
                            raw.append(p)

            seen = set()
            final = []
            for it in raw:
                if isinstance(it, (list, tuple)) and len(it) >= 1:
                    title = str(it[0])
                else:
                    title = str(it)
                key = title.strip().lower()
                if key in seen:
                    continue
                seen.add(key)
                final.append(it)
            if final:
                return final

        return []

# Interfaz gráfica
class HaskflixGUI:
    def __init__(self, root):
        self.root = root
        self.root.title("Haskflix")
        self.root.geometry("900x700")
        self.root.configure(bg="black")
        self.recomendador = Recomendador()

        # datos de sesión
        self.gustos = defaultdict(set)
        self.disgustos = defaultdict(set)
        self.edades = {}
        self.paises = {}
        self.usuarioActual = None

        self.pantallaLogin()

    def limpiar(self):
        for w in self.root.winfo_children():
            w.destroy()

    # Pantallas (nombres en camelCase)
    def pantallaLogin(self):
        self.limpiar()
        tk.Label(self.root, text="Haskflix", font=("Helvetica", 42, "bold"), fg="red", bg="black").pack(pady=20)
        frame = tk.Frame(self.root, bg="black")
        frame.pack(pady=10)

        tk.Label(frame, text="Correo (sin @):", fg="white", bg="black").grid(row=0, column=0, sticky="e", padx=8, pady=6)
        self.entradaCorreo = tk.Entry(frame, width=30); self.entradaCorreo.grid(row=0, column=1, pady=6)
        tk.Label(frame, text="Contraseña:", fg="white", bg="black").grid(row=1, column=0, sticky="e", padx=8, pady=6)
        self.entradaClave = tk.Entry(frame, show="*", width=30); self.entradaClave.grid(row=1, column=1, pady=6)

        tk.Button(self.root, text="Ingresar", bg="red", fg="white", width=20, command=self.ingresar).pack(pady=12)
        tk.Button(self.root, text="Crear cuenta", bg="#333333", fg="white", width=20, command=self.pantallaRegistro).pack()

    def pantallaRegistro(self):
        self.limpiar()
        tk.Label(self.root, text="Crear cuenta", font=("Helvetica", 28, "bold"), fg="red", bg="black").pack(pady=10)
        form = tk.Frame(self.root, bg="black"); form.pack(pady=6)

        labels = ["Nombre","Apellido","Correo (sin @)","Contraseña","Edad","País"]
        self.varsRegistro = {}
        for i, lab in enumerate(labels):
            tk.Label(form, text=lab+":", fg="white", bg="black").grid(row=i, column=0, sticky="e", padx=6, pady=6)
            ent = tk.Entry(form, width=30)
            if lab == "Contraseña":
                ent.config(show="*")
            ent.grid(row=i, column=1, pady=6)
            self.varsRegistro[lab] = ent

        tk.Button(self.root, text="Registrar", bg="red", fg="white", width=20, command=self.crearCuenta).pack(pady=12)
        tk.Button(self.root, text="Volver", bg="#333333", fg="white", width=20, command=self.pantallaLogin).pack()

    def crearCuenta(self):
        data = {k: self.varsRegistro[k].get().strip() for k in self.varsRegistro}
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
        self.pantallaLogin()

    def ingresar(self):
        email = self.entradaCorreo.get().strip().lower()
        pwd = self.entradaClave.get().strip()
        user = usuariosDB.get(email)
        if not user or user["password"] != pwd:
            messagebox.showerror("Error","Correo o contraseña incorrectos")
            return
        self.usuarioActual = email
        self.edades[email] = user["edad"]
        self.paises[email] = user["pais"].strip().lower().replace(" ", "_")
        self.pantallaSeleccionarTipo()

    def pantallaSeleccionarTipo(self):
        self.limpiar()
        nombre = usuariosDB[self.usuarioActual]["nombre"]
        tk.Label(self.root, text=f"Hola, {nombre}", font=("Helvetica", 22, "bold"), fg="white", bg="black").pack(pady=8)
        tk.Label(self.root, text="¿Qué quieres ver?", font=("Helvetica", 18), fg="red", bg="black").pack(pady=10)
        btn_frame = tk.Frame(self.root, bg="black"); btn_frame.pack(pady=8)
        tk.Button(btn_frame, text="Películas", bg="red", fg="white", width=20, command=lambda:self.pantallaGeneros("pelicula")).grid(row=0, column=0, padx=10, pady=6)
        tk.Button(btn_frame, text="Series", bg="red", fg="white", width=20, command=lambda:self.pantallaGeneros("serie")).grid(row=0, column=1, padx=10, pady=6)
        tk.Button(self.root, text="Cerrar sesión", bg="#333333", fg="white", width=20, command=self.cerrarSesion).pack(pady=18)

    def cerrarSesion(self):
        self.usuarioActual = None
        self.gustos.clear()
        self.disgustos.clear()
        self.pantallaLogin()

    def pantallaGeneros(self, tipo):
        self.limpiar()
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

        self.varsGeneros = {}
        for g in generosDisp:
            v = tk.IntVar()
            chk = tk.Checkbutton(scroll_frame, text=g.title(), variable=v, fg="white", bg="black", selectcolor="red", activebackground="black", activeforeground="white", font=("Arial", 12))
            chk.pack(anchor="w", padx=8, pady=4)
            self.varsGeneros[g] = v

        btn_frame = tk.Frame(self.root, bg="black"); btn_frame.pack(pady=10)
        tk.Button(btn_frame, text="Ver recomendaciones", bg="red", fg="white", width=20, command=lambda:self.mostrarRecom(tipo)).grid(row=0, column=0, padx=8)
        tk.Button(btn_frame, text="Volver", bg="#333333", fg="white", width=20, command=self.pantallaSeleccionarTipo).grid(row=0, column=1, padx=8)

    def mostrarRecom(self, tipo):
        seleccion = {g for g,v in self.varsGeneros.items() if v.get()==1}
        self.gustos[self.usuarioActual] = seleccion
        self.disgustos[self.usuarioActual] = {g for g in generosDisp if g not in seleccion}
        self.edades[self.usuarioActual] = usuariosDB[self.usuarioActual]["edad"]
        self.paises[self.usuarioActual] = usuariosDB[self.usuarioActual]["pais"].strip().lower().replace(" ", "_")

        if not seleccion:
            messagebox.showinfo("Aviso","Selecciona al menos un género.")
            return

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

        self.limpiar()
        tk.Label(self.root, text=f"Recomendaciones ({tipo})", font=("Helvetica", 20, "bold"), fg="red", bg="black").pack(pady=10)
        if not recs:
            tk.Label(self.root, text="No se encontraron resultados.", fg="white", bg="black").pack(pady=10)
        else:
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
                # item puede ser tuple (Title,Director,Rating,Genres) [pelicula]
                # o (Title,Director,Rating,Genres,Seasons) [serie]
                if isinstance(item, (list, tuple)):
                    title = item[0]
                    director = item[1] if len(item) > 1 else ""
                    rating = item[2] if len(item) > 2 else ""
                    genres = item[3] if len(item) > 3 else ""
                    seasons = item[4] if len(item) > 4 else None
                else:
                    title = str(item)
                    director = ""
                    rating = ""
                    genres = ""
                    seasons = None

                genresStr = self.formatearGeneros(genres)
                seasonsStr = f" | Temporadas: {seasons}" if seasons is not None else ""
                btn_text = f"{title}  {'| ' + str(director) if director else ''} {'| ⭐ '+str(rating) if rating else ''} {'| ' + genresStr if genresStr else ''}{seasonsStr}"
                b = tk.Button(list_frame, text=btn_text, anchor="w", width=110, bg="#222222", fg="white",
                              activebackground="#333333", command=lambda it=item: self.mostrarDetalles(it))
                b.pack(fill="x", padx=6, pady=6)

        tk.Button(self.root, text="Volver", bg="#333333", fg="white", width=18, command=lambda:self.pantallaGeneros(tipo)).pack(pady=10)
        tk.Button(self.root, text="Cerrar sesión", bg="#333333", fg="white", width=18, command=self.cerrarSesion).pack()

    # helpers (nombres en camelCase)
    def formatearGeneros(self, genres):
        if isinstance(genres, (list, tuple)):
            return ", ".join(str(g).strip("'\" ") for g in genres)
        if not genres:
            return ""
        if isinstance(genres, str):
            s = genres.strip()
            if (s.startswith("[") and s.endswith("]")) or (s.startswith("(") and s.endswith(")")):
                try:
                    parsed = ast.literal_eval(s)
                    if isinstance(parsed, (list, tuple)):
                        return ", ".join(str(g).strip("'\" ") for g in parsed)
                except Exception:
                    pass
            if "," in s:
                parts = [p.strip().strip("'\" ") for p in s.split(",") if p.strip()]
                return ", ".join(parts) if parts else s
            return s.strip("[]()'\" ")
        return str(genres)

    def mostrarDetalles(self, item):
        if isinstance(item, (list, tuple)) and len(item) >= 1:
            title = str(item[0])
            director = str(item[1]) if len(item) > 1 and item[1] is not None else "Desconocido"
            rating = str(item[2]) if len(item) > 2 and item[2] is not None else "N/A"
            genresRaw = item[3] if len(item) > 3 else []
            seasons = item[4] if len(item) > 4 else None
        else:
            title = str(item)
            director = "Desconocido"
            rating = "N/A"
            genresRaw = []
            seasons = None

        genresStr = self.formatearGeneros(genresRaw)
        seasonsDisplay = f"{seasons}" if seasons is not None else "N/A"

        msg = f"Título: {title}\nGéneros: {genresStr if genresStr else 'N/A'}\nDirector: {director}\nPuntuación: {rating}"
        if seasons is not None:
            msg += f"\nTemporadas: {seasonsDisplay}"
        messagebox.showinfo(title, msg)


# Ejecutar app
if __name__ == "__main__":
    if not os.path.exists(archivoPeliculas):
        print(f"AVISO: No se encontró '{archivoPeliculas}'. Crea el archivo Prolog con películas y series y las reglas recommend/.. y recommend_serie/..")
    root = tk.Tk()
    app = HaskflixGUI(root)
    root.mainloop()
