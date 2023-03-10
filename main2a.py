#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.11/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
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
    # import sys
    import zipfile

    # Import my modules ...
    import rsl

    # Specify the path to the "OS Terrain 50" dataset ...
    fname0 = "terr50_gagg_gb.zip"

    # Initialize limits ...
    maxX = -2**31
    maxY = -2**31
    minX =  2**31
    minY =  2**31

    # Compile regex to save time ...
    pattern = re.compile(r"data/[a-z]+/[a-z]+[0-9]+_OST50GRID_[0-9]+.zip")

    # Load dataset ...
    with zipfile.ZipFile(fname0, "r") as fObj0:
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
                # Read files into RAM so that they become seekable ...
                # NOTE: https://stackoverflow.com/a/12025492
                ascObj = io.BytesIO(fObj1.read(f"{key}.asc"))
                auxObj = io.BytesIO(fObj1.read(f"{key}.asc.aux.xml"))
                gmlObj = io.BytesIO(fObj1.read(f"{key}.gml"))
                prjObj = io.BytesIO(fObj1.read(f"{key}.prj"))
                xmlObj = io.BytesIO(fObj1.read(f"Metadata_{key}.xml"))

                # Load header of ASCII dataset ...
                hdr = rsl.loadASCIIheader(ascObj)

                # Increment limits ...
                maxX = max(maxX, hdr["xllcorner"] // hdr["cellsize"] + hdr["ncols"])
                maxY = max(maxY, hdr["yllcorner"] // hdr["cellsize"] + hdr["nrows"])
                minX = min(minX, hdr["xllcorner"] // hdr["cellsize"])
                minY = min(minY, hdr["yllcorner"] // hdr["cellsize"])

                # # Save a copy of the files locally for inspection ...
                # open(f"{key}.asc", "wb").write(ascObj.read())
                # open(f"{key}.asc.aux.xml", "wb").write(auxObj.read())
                # open(f"{key}.gml", "wb").write(gmlObj.read())
                # open(f"{key}.prj", "wb").write(prjObj.read())
                # open(f"Metadata_{key}.xml", "wb").write(xmlObj.read())
                # sys.exit()

                # Clean up ...
                del ascObj
                del auxObj
                del gmlObj
                del prjObj
                del xmlObj
                del hdr

            # Clean up ...
            del zipObj

    print(minX, maxX)
    print(minY, maxY)
