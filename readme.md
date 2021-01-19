Práctica de Haskell: Meta tres en raya
======================================

Hemos implementado dos juegos: el *tres en raya* y una generalización de este,
el *meta tres en raya*. En ambos casos se asume que siempre empieza el jugador
X. Además de programar las reglas de los juegos hemos desarrollado interfaces
gráficas y de texto y un sistema muy simple de agentes inteligentes.

Salvo en algún caso donde no hemos sido capaces hemos intentado evitar repetir
código, definiendo clases que acomunan las interfaces y permiten separar las
partes comunes.

Documentación
-------------

[*Haddock*](https://www.haskell.org/haddock/) es una utilidad para
generar documentación automáticamente a partir de comentarios del código fuente.
Hay que tener en cuenta que *haddock* incluye las descripciones y datos de las
funciones que son exportadas por los módulos, por lo que hay comentarios que no
se ven en la documentación. Esto no es malo, ya que somos nosotros quienes
decidimos que funciones queremos que sean visibles fuera de los módulos y cuales
son internas.

Otra función muy chula de *haddock* es que para cada fichero de código genera un
[html](./docs/src) donde podemos navegar el código de forma interactiva, viendo
de que tipo son las cosas y donde están las definiciones. Se puede acceder a
estas páginas a través de los enlaces *#source* al lado de cada entrada de la
documentación o desde el enlace *source* a de la barra superior.

Hemos adjuntado una carpeta [/docs](./docs) con la documentación generada por
haddock en el momento de entregar la práctica. También se puede consultar en
[este enlace](https://haztecaso.com/mttt/docs). Es posible el enlace esté caido o
que el contenido no coincida exactamente con la versión entregada.

### Estructura del código

El proyecto está organizado con la estructura predefinida de *stack*:

- En [/app](./app) se encuentra el código compilable a un binario. Al compilar
  el programa se genera un binario `mttt`, al que podemos pasarle opciones para
  lanzar las distintas interfaces de los juegos. Se pueden consultar las
  opciones mediante el comando `mttt -h`. Por completitud las pegamos aquí
  también:

  ```
  usage: mttt [options]
    [-h,--help]     Lista de opciones para la interfaz cli
    [-t,--tui]      Interfaz de texto
    [-s,--simple]   Jugar al tres en raya
    [-m,--multi]    Jugar en modo multijugador
    [-p,--primero]  Dejar que empiece el agente
  ```

- En [/src](./src) están los módulos que forman la librería *Mttt*.

- En [/test](./test) estárían los tests. Hemos dejado un ejemplo de un test que
  hemos usado para debuguear la traducción de posiciones del puntero a
  posiciones del tablero. Se puede ejecutar mediante el comando `stack test`

### Organización de los módulos

Esta es una descripción superficial de que hay en cada módulo. En el propio
código hay comentarios que explican con más detalle que hacen las funciones.

- *Mttt*: Función que usa el binario `mttt` para lanzar las distintas
  interfaces.
- Tipos de datos
  - *Mttt.Common*: Aquí está definida la parte común para los tipos de datos
    de los juegos. Para ello hemos definido la clase *Juego*. Esta clase está
    parametrizada mediante tres tipos (para poder hacer esto hemos tenido que
    incluir la extensión [`MultiParamTypeClasses`](https://downloads.haskell.org/~ghc/8.8.4/docs/html/users_guide/glasgow_exts.html#multi-parameter-type-classes)).
    El tipo *juego* es el que almacena los datos del juego correspondiente,
    *pos* el tipo para las posiciones del juego y *casilla* el tipo de las
    casillas del juego. *pos* y *casilla* son tipos que dependen de *juego*
    (para esto hemos incluido la extensión [`FunctionalDependencies`](https://downloads.haskell.org/~ghc/8.8.4/docs/html/users_guide/glasgow_exts.html#functional-dependencies)).
    También hemos definido un tipo para los agentes inteligentes.
  - *Mttt.Bloque*: Tipo de datos para el *tres en raya*. Para que el tipo
    *Bloque* pueda ser instancia de la clase *Juego* es necesario incluir las
    extensiones [`MultiParamTypeClasses`](https://downloads.haskell.org/~ghc/8.8.4/docs/html/users_guide/glasgow_exts.html#multi-parameter-type-classes)
    y [`FlexibleInstances`](https://downloads.haskell.org/~ghc/8.8.4/docs/html/users_guide/glasgow_exts.html#extension-FlexibleInstances).
  - *Mttt.Tablero*: Tipo de datos para el *meta tres en raya*. Aquí también
    hemos tenido que incluir las mismas extensiones que en *Mttt.Bloque*. El
    tipo 'Tablero' usa la [sintaxis
    *record*](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-490003.15),
    mediante la cual se le asignan nombres a los contenidos del tipo. Esto es
    muy cómodo porque define automáticamente unas funciones (llamadas *getters*)
    para extraer los valores.  
    La función heurística *heur0* la hemos robado de un trabajo que hemos
    encontrado en la red (más info abajo).
  - *Mttt.Inteligencia*: Aquí está definido el algoritmo *minimax*.
  - *Mttt.Gui.Common*: Parte común de las interfaces gráficas. Hemos acomunado
    todas las funcionalidades que hemos podido en la clase *Estado*.
  - *Mttt.Gui.Bloque* y *Mttt.Gui.Tablero*: Interfaces para los juegos.
  - *Mttt.Tui*: Interfaz de texto para los juegos.

Problemas y posibles mejoras
----------------------------

- Definir mejores funciones heurísticas.
- Implementar testeos de eficiencia para comparar los distintos agentes.

Compilando el proyecto
----------------------

Puesto que el proyecto está organizado con *stack* es poco práctico compilarlo
directamente con *ghc* (no sabemos como habría que proceder para enlazar
correctamente los módulos). Tampoco estamos seguros de si las funciones de
*Prelude* que hemos usado y las extensiones del lenguaje son compatibles con
*Hugs* (sospechamos que no es el caso...).

Por tanto lo mejor es usar una de las herramientas compatibles con el proyecto:
*stack* o *cabal*. Nosotros hemos aprendido a usar *stack* y no tenemos
experiencia con *cabal*, pero en teoría *stack* es totalmente compatible con
*cabal*, ya que genera un fichero [mttt.cabal](./mttt.cabal).

Stack: paquete, gestión de dependecias
--------------------------------------

*Stack* gestiona las dependencias de la aplicación y proporciona una interfaz
muy cómoda para *ghc* y *ghci*. Además está muy bien integrado con otras
utilidades como *cabal*, *ghcid*, *haddock* y *nix*.

*Stack* es compatible con *cabal*. Para generar el archivo .cabal a partir de
`packacge.yaml` (configuración del proyecto de *stack*) se puede usar *hpack* o
`stack build`.

Algunos comandos útiles de *stack*:

- `stack build` para compilar el programa.
- `stack ghci` para obtener un prompt de *ghci* con el programa cargado como
  librería.
- `stack exec -- mttt -h` para ejecutar el programa.
- `stack haddock --file-watch` para generar la documentación de forma automática
  mientras editamos los archivos.

Nix
---

[Nix](https://nixos.wiki/wiki/Nix) es un gestor de paquetes que deriva
instrucciones de compilación especificadas en el lenguaje de programación
[*nix*](https://nixos.wiki/wiki/Nix_Expression_Language), un lenguaje de
programación funcional puro y perezoso.

Este proyecto usa *nix* solo para gestionar el entorno de desarollo de
*haskell*. La gestión de dependencias y compilación queda delegada totalmente a
*stack*. Lo bueno de esto es que no es necesario escribir una derivación de
*nix* y como *stack* está integrado con *nix*, no perdemos ciertos beneficios de
*nix* (como tener un entorno aislado).

Con *nix-shell* se puede utilizar el entorno de desarollo definido en
[shell.nix](./shell.nix). Aquí se incluyen algunas utilidades cómodas para
programar en *haskell*:

- *ghcid*: Versión de *ghci* que recompila el código cada vez que se modifica
  un fichero. Se puede lanzar con el comando `nix-shell --run ghcid`.

- *haskell-language-server*:
  *[LSP](https://microsoft.github.io/language-server-protocol/)* para *haskell*.
  Útil para integrar con un editor (por ejemplo con [*nvim*](https://neovim.io),
  mediante [*coc.nvim*](https://github.com/neoclide/coc.nvim/)).

Dependencias
------------

Hemos usado dos librerías:

- [gloss](https://hackage.haskell.org/package/gloss/) para la parte gráfica.
  Es una dependencia de toda la librería.
- [parseargs](https://hackage.haskell.org/package/parseargs/) para gestionar las
  opciones que se le pasan al binario. Esta dependencia es necesaria solo para
  compilar el ejecutable, no para la librería.

Las dependencias del proyecto están especificadas en
[package.yaml](./package.yaml).

Releases
--------

[Aquí](https://haztecaso.com/mttt/releases/) están subidas las últimas versiones
del proyecto.

Referencias
-----------

- [AI agent for Ultimate Tic Tac Toe Game](https://www.cs.huji.ac.il/~ai/projects/2013/U2T3P/files/AI_Report.pdf).
  De aquí hemos sacado la idea para la función heurística *Mttt.Tablero.heur0'
- [At Most 43 Moves, At Least 29: Optimal Strategies and Bounds for Ultimate
  Tic-Tac-Toe](https://arxiv.org/abs/2006.02353). Aquí se demuestra formalmente
  que existe una estrategia óptima para jugar al *meta tres en raya* (para unas
  reglas ligeramente distintas).
- [AI Approaches to Ultimate Tic-Tac-Toe](https://www.cs.huji.ac.il/~ai/projects/2013/UlitmateTic-Tac-Toe/files/report.pdf)
- [How I used the AlphaZero algorithm to play Ultimate
  tic-tac-toe](https://youtu.be/CcwC8tTe_QE)
