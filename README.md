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

- Cuidado con exportar las representaciones internas de los tipos de datos.
Mejor definir funciones que usen bien las representaciones.
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

Paquetes
------------

### Stack

Stack gestiona las dependencias de la aplicación y proporciona una interfaz muy
cómoda para ghc y ghci. Además está muy bien integrado con otras utilidades como
cabal, ghcid y el lenguaje nix.

Stack es compatible con cabal. Para generar el archivo .cabal a partir de
`packacge.yaml` (configuración del proyecto de stack) se puede usar *hpack* o
`stack build`.

Se puede compilar el codigo mediante el comando `stack build`.

### Nix

[Nix](https://nixos.wiki/wiki/Nix) es un gestor de paquetes que deriva
instrucciones de compilación especificadas en el lenguaje de programación
[nix](https://nixos.wiki/wiki/Nix_Expression_Language), un lenguaje de
programación funcional puro y perezoso.

Este proyecto usa nix solo para gestionar el entorno de desarollo de haskell.
La gestión de dependencias y compilación queda delegada totalmente a stack. Lo
bueno de esto es que no es necesario escribir una derivación de nix y como stack
está integrado con nix, no perdemos ciertos beneficios de nix (como tener un
entorno aislado).

Con **nix-shell** se puede utilizar el entorno de desarollo definido en `shell.nix`.
Aquí se incluyen algunas utilidades cómodas para programar en haskell:

- **ghcid**: Versión de ghci que recompila el código cada vez que se modifica un
  fichero. Se puede lanzar con el comando `nix-shell --run ghcid`.

- **haskell-language-server**:
  [LSP](https://microsoft.github.io/language-server-protocol/) para haskell.
  Útil para integrar con un editor (por ejemplo con nvim, mediante coc-nvim).

Dependencias
------------

Se pueden consultar en `package.yaml`.

- [gloss](https://hackage.haskell.org/package/gloss/)
<!-- # - [parseargs](https://hackage.haskell.org/package/parseargs/) -->

Referencias
-----------

https://www.cs.huji.ac.il/~ai/projects/2013/UlitmateTic-Tac-Toe/
