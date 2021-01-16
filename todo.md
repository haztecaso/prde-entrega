TO DO
-----

- Definir un tablero de prueba para
    - Comprobar que se está dibujando bien el tablero


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

- Revisar todos los comentarios de TODO
- Revisar formato codigo
- Revisar warnings
- Ver si hay funciones sin documentar: `$ stack haddock`
- Atención con la pereza
  - Tipos de datos estrictos?

### Organización del código

- Decidir bien los nombres de las cosas
  - Tipso (Bloque, Tablero, ...)
  - Módulos

- Separar app/Main.hs en cuatro programas, uno para cada juego (cli / gui)
