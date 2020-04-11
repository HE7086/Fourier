# Simple Fun Fourier Drawing Programm

### Note

This is oringinally a program for the __Functional Programming Competition__ and is under refactor  
see details about the competition here: [Link](http://www21.in.tum.de/teaching/fpv/WS1920/wettbewerb.html)  
see original source here: [Link](2c31d26154b3e22afb435f266e6cce97c1ab28ab)

### Inspiration

* 3Blue1Brwon's amazing [**video**](https://www.youtube.com/watch?v=r6sGWTCMz2k) told us that Fourier Series can draw anything
* In the form of Integral, the series will approach the oringinal image as the number of terms rise

<img src="https://render.githubusercontent.com/render/math?math=f(t) = \int_{0}^{1}c_{n}e^{2i\pi n * t} dt">

* The Mathematical Pinciple behind the image is so amazing that I decide to present this simple fun programm that draws anything

### Data Format

* prepare your data in the following json format, where each object of series represents a term of fourier series
```json
{
    "series": [
        {
            "number" : 0,
            "real" : 1,
            "imag" : 2 
        }
    ]
}
```
* `number` represents exactly __n__ in the form, which should be a Integer, and controls how fast the vector spins
* `real` and `imag` represent the complex constant __Cn__ in the form, control the starting position and length of the vector
* Then put them in one directory without other files. Each file will be transformed to a closed curve. In order to draw a image with multiple curves, you'll need the same amount of json files.

### Example
![haskell](./example/haskell.gif)

### Usage

##### Build the Binary
* clone the repository and run cabal-build
* make sure to run `cabal update` once if it is newly installed
```
git clone https://github.com/HE7086/Fourier.git
cd Fourier
cabal build
```
* you can find the binary inside `dist` folder
##### CLI Flags
| short   | long          | description                   |
|---------|---------------|-------------------------------|
| -g      | --gif         | generate gif files            |
| -p      | --png         | generate png files            |
| -c      | --combined    | combine the images afterwards |
| -o PATH | --output PATH | specify output path           |
| -i PATH | --input  PATH | specify input path/file       |
| -v      | --version     | print version message         |
| -h      | --help        | print help message            |

* by default help messages will be printed

##### Example
* generate gifs from all the files in a directory
```
cabal run -- Drawer --gif --input ./data --output ./out --combined
```
* generate one png from a file
```
cabal run -- Drawer --png --input ./data/1.json --output ./out
```
* run with threaded mode (significantly increase performance on multicore cpus)
```
cabal run -- Drawer --png --input ./data/1.json --output ./out +RTS -N
```

### Customization
* In the file `src/Settings.hs` are several option that can be customized easily
    * `width`, `height` : the height and width of the generated image
    * `white`, `black` : the standard pixel color
    * `pixelArt` : the function that dertermines color and by default draws black
    * `pointCount` : the number of points to be painted
    * `gifStep` : how many pixels will be rendered each frame
        * warning : terrible runtime if set too small
    * `scaleFactor` : how much the final image will be scaled
* These customization options will be merged into cli flags in the future

### TODO list
- [ ] optimize performance
- [ ] adjustable line thickness
- [ ] adjustable fore-/background color
- [ ] coordinates converter: convert coordinates to forms
