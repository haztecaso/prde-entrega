Meta Tic Tac Toe
================
- Se asume que siempre empieza el jugador X

TO DO
-----

### Desarrollo
Es importante ir documentando las funciones sobre la marcha!

- Tipos de datos
- Funciones show
- Reglas del juego
  - Determinar a quien le toca
  - Donde se puede jugar
  - Determinar si ha acabado la partida
    - ¿Quien ha ganado?

- Operaciones de E/S
    - Leer tablero desde archivo
    - CLI
        - Preguntar donde jugar
        - Enseñar tablero
    - GUI

- IA
    - Jugador aleatorio
    - Definición de funciones heurísticas
    - MiniMax
    - Poda Alpha-Beta
    - ¿Reducciones del árbol, definiciones de jugadas equivalentes?

- Tests
    - Comparar funciones heurísticas

### Estilo y detalles
- Cuidado con exportar las representaciones internas de los tipos de datos. Mejor definir funciones que usen bien las representaciones.
- Revisar todos los comentarios de TODO
- Revisar formato codigo
- Revisar warnings
- Ver si hay funciones sin documentar: `$ stack haddock`

### Organización del código

- Decidir bien los nombres de las cosas
    - Tipso (Bloque, Tablero, ...)
    - Módulos

- Separar app/Main.hs en cuatro programas, uno para cada juego (cli / gui)

Dependencias
------------
Se pueden consultar en `package.yaml`.
- [gloss](https://hackage.haskell.org/package/gloss/)
- [parseargs](https://hackage.haskell.org/package/parseargs/)

Referencias
-----------
https://www.cs.huji.ac.il/~ai/projects/2013/UlitmateTic-Tac-Toe/
