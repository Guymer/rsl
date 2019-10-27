# Rising Sea Levels (RSL)

This project aims to map the effects of rising sea levels on Great Britain.

## Workflow

1. Download for yourself [the "OS Terrain 50" dataset](https://www.ordnancesurvey.co.uk/business-and-government/products/terrain-50.html), in the "ASCII Grid and GML (Grid)" file format, from the Ordnance Survey to provide the "terr50_gagg_gb.zip" file that is used here
2. Convert the ZIP file of [the "OS Terrain 50" dataset](https://www.ordnancesurvey.co.uk/business-and-government/products/terrain-50.html) to a BIN file (by running [convertZIPtoBIN.py](convertZIPtoBIN.py))
3. Compile the FORTRAN programs (by running [compile.sh](compile.sh))
4. Create an elevation map of Great Britain (by running [convertBINtoPPM](convertBINtoPPM.F90))
5. Flood Great Britain (by running [createFlood](createFlood.F90))
6. Convert all generated PPM images to PNG images (by running [convertPPMtoPNG.sh](convertPPMtoPNG.sh))
7. Convert the sequence of PPM images to a GIF animation (by running `convert createFlood_????m.png createFlood.gif`)

## Method

The method that is used here has been stolen from [Where Can Pregnant Women Go? (WCPWG)](https://github.com/Guymer/wcpwg) as it is exactly the same numerical problem.

## Dependencies

RSL requires the following Python modules to be installed and available in your `PYTHONPATH`.

* [numpy](https://pypi.org/project/numpy)

## To Do

* Obviously, the [compile.sh](compile.sh) needs to be replaced by a real Makefile at some point.

## Bugs

* If you look at the image with a sea level of 0m (shown below) then you will notice that there are some large lakes in East Anglia that are not there in real life. In real life they are below sea level, with [Holme Fen](https://en.wikipedia.org/wiki/Holme_Fen) being the lowest point in GB. However, in real life there are numerous dykes that keep the sea out so that these low-lying areas are not flooded. [The "OS Terrain 50" dataset](https://www.ordnancesurvey.co.uk/business-and-government/products/terrain-50.html) is gridded on 50m by 50m pixels, therefore, I conclude that the reason why I am incorrectly flooding these areas with a sea level of 0m is because the dykes that keep the water out are less than 50m by 50m in real life.

![GB with a sea level of 0m](bug.png)
