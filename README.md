Meta Tic Tac Toe
================

- Se asume que siempre empieza el jugador X

Haddock: documentación a partir del código y los comentarios
------------------------------------------------------------

[haddock](https://www.haskell.org/haddock/) es una utilidad para
generar documentación automáticamente a partir de los comentarios del código
fuente.

En [este enlace](https://haztecaso.com/mttt/) se puede consultar la
documentación generada por haddock. Es posible el enlace esté caido o
desactualizado.

Stack: paquete, gestión de dependecias
--------------------------------------

Stack gestiona las dependencias de la aplicación y proporciona una interfaz muy
cómoda para ghc y ghci. Además está muy bien integrado con otras utilidades como
cabal, ghcid y el lenguaje nix.

Stack es compatible con cabal. Para generar el archivo .cabal a partir de
`packacge.yaml` (configuración del proyecto de stack) se puede usar *hpack* o
`stack build`.

Algunos comandos útiles de stack:

- `stack build` para compilar el programa.
- `stack ghci` para obtener un prompt de ghci con el programa cargado como
  librería.
- `stack exec mttt` para ejecutar el programa.
- `stack haddock` para generar la documentación.

También se puede usar nix para evitar tener que instalar stack en el entorno
global de nuestro sistema: `nix-shell --run "stack ..."`.

Nix
---

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
- [parseargs](https://hackage.haskell.org/package/parseargs/)

Referencias
-----------

https://www.cs.huji.ac.il/~ai/projects/2013/UlitmateTic-Tac-Toe/
