#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # NOTE: I downloaded the "OS Terrain 50" dataset in the "ASCII Grid and GML
    #       (Grid)" file format from the Ordnance Survey, see:
    #         * https://www.ordnancesurvey.co.uk/business-and-government/products/terrain-50.html
    #       This gave me the "terr50_gagg_gb.zip" file that is used here.

    # NOTE: I downloaded the "OS Terrain 50" dataset in the "ESRI Shape
    #       (Contours)" file format from the Ordnance Survey, see:
    #         * https://www.ordnancesurvey.co.uk/business-and-government/products/terrain-50.html
    #       This gave me the "terr50_cesh_gb.zip" file that is used here.

    # NOTE: https://scitools.org.uk/cartopy/docs/latest/crs/projections.html#osgb

    # **************************************************************************

    # Import standard modules ...
    import io
    import re
    import zipfile

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Import my modules ...
    try:
        import pyguymer3
        import pyguymer3.osterrain
    except:
        raise Exception("\"pyguymer3\" is not installed; run \"pip install --user PyGuymer3\"") from None

    # **************************************************************************

    # Specify the path to the "OS Terrain 50" dataset ...
    zname = "terr50_gagg_gb.zip"

    # Find extent of the "OS Terrain 50" dataset ...
    x1, x2, y1, y2 = pyguymer3.osterrain.findExtent(zname)

    # Set array sizes ...
    nx = x2 - x1
    ny = y2 - y1

    # Make map ...
    elev = numpy.zeros((ny, nx), dtype = numpy.float32)                         # [m]

    # Compile regex to save time ...
    pattern = re.compile(r"data/[a-z]+/[a-z]+[0-9]+_OST50GRID_[0-9]+.zip")

    # Load dataset ...
    with zipfile.ZipFile(zname, "r") as fObj0:
        # Loop over members ...
        for fname1 in fObj0.namelist():
            # Skip this member if it is not a sub-dataset ...
            if pattern.match(fname1) is None:
                continue

            # Determine sub-dataset key ...
            key = fname1.split("/")[-1].split("_")[0].upper()

            # Read sub-dataset into RAM so that it becomes seekable ...
            # NOTE: https://stackoverflow.com/a/12025492
            zipObj = io.BytesIO(fObj0.read(fname1))

            # Load sub-dataset ...
            with zipfile.ZipFile(zipObj, "r") as fObj1:
                # Read ASCII dataset into RAM so that it becomes seekable ...
                # NOTE: https://stackoverflow.com/a/12025492
                ascObj = io.BytesIO(fObj1.read(f"{key}.asc"))

                # Load header and contents of ASCII dataset ...
                hdr = pyguymer3.osterrain.loadASCIIheader(ascObj)
                cont = pyguymer3.osterrain.loadASCIIcontents(ascObj, hdr["length"]) # [m]

                # Determine indexes (from the upper-left corner not the
                # lower-left corner) ...
                ix1 = hdr["xllcorner"] // hdr["cellsize"]
                ix2 = hdr["xllcorner"] // hdr["cellsize"] + hdr["ncols"]
                iy1 = ny - (hdr["yllcorner"] // hdr["cellsize"] + hdr["nrows"])
                iy2 = ny - (hdr["yllcorner"] // hdr["cellsize"])

                # Populate array ...
                elev[iy1:iy2, ix1:ix2] = cont[:, :]                             # [m]

    # Save BIN ...
    elev.tofile("terr50_gagg_gb.bin")
