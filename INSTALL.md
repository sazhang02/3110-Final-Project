# CS-3110-Final-Project

# How to install the system:

## For Mac:

### First, we need to install the [Graphics Library](https://github.com/ocaml/graphics).

1. Install XQuartz
2. `brew install pkg-config`
3. `opam install graphics`

### Now, we need to install an image processing library. We use [camlimages](https://www.google.com/url?q=https://gitlab.com/camlspotter/camlimages&sa=D&source=editors&ust=1617134969939000&usg=AOvVaw3al_S_FsgYZIQaoyCdh0Bz) to render images in OCaml. In order to install this, you must install libpng and libjpg first.

1. `brew install libpng`
2. `brew install libjpg`
3. `opam install camlimages`


## For Windows:

### First, we need to install the [Graphics Library](https://github.com/ocaml/graphics).

1. Install Xming
2. `sudo apt install pkg-config`
3. `opam install graphics`

### Now, we need to install an image processing library. We use [camlimages](https://www.google.com/url?q=https://gitlab.com/camlspotter/camlimages&sa=D&source=editors&ust=1617134969939000&usg=AOvVaw3al_S_FsgYZIQaoyCdh0Bz) to render images in OCaml. In order to install this, you must install libpng and libjpg first.
 
1. `sudo apt install libpng libjpeg`
2. `opam install camlimages`


# How to build the system:

1. Download the source code or clone our git repository
2. Open your terminal and change directory (aka `cd`) to the repository
3. If you are on Windows, you must open Xming (Mac should automatically open XQuartz)
4. Run `make play` to open the game
