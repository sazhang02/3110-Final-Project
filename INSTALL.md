# CS-3110-Final-Project

# How to install the system:

## For Mac:

### 0. Install [Homebrew](https://brew.sh/) if it is not already installed using installiation instructions found on its website.

### 1. Install the [Graphics Library](https://github.com/ocaml/graphics).

1. Install [XQuartz](https://www.xquartz.org/)
2. `brew install pkg-config`
3. `opam install graphics`

### 2. Install an image processing library. We use [camlimages](https://www.google.com/url?q=https://gitlab.com/camlspotter/camlimages&sa=D&source=editors&ust=1617134969939000&usg=AOvVaw3al_S_FsgYZIQaoyCdh0Bz) to render images in OCaml. In order to install this, install libpng and libjpg first.

1. `brew install libpng`
2. `brew install libjpg`
3. `opam install camlimages`


## For Windows:

### 1. Install the [Graphics Library](https://github.com/ocaml/graphics).

1. Install [Xming](https://sourceforge.net/projects/xming/)
2. `sudo apt install pkg-config`
3. `opam install graphics`

### 2. Install an image processing library. We use [camlimages](https://www.google.com/url?q=https://gitlab.com/camlspotter/camlimages&sa=D&source=editors&ust=1617134969939000&usg=AOvVaw3al_S_FsgYZIQaoyCdh0Bz) to render images in OCaml. In order to install this, install libpng and libjpg first.
 
1. `sudo apt install libpng-dev libjpeg-dev`
2. `opam install camlimages`


# How to build the system:

1. Download the source code `desert.zip`
2. Open your terminal and move the `desert.zip` to a folder of your choice
3. Change directory (aka `cd`) to the folder and `unzip desert.zip`
4. If you are on Windows, you must open Xming (Mac should automatically open XQuartz) and run `export DISPLAY=:0`
5. Run `make play` to open the game

    