#!/usr/bin/env python3

# NOTE: I downloaded the "OS Terrain 50" dataset in the "ASCII Grid and GML
#       (Grid)" file format from the Ordnance Survey, see:
#         * https://www.ordnancesurvey.co.uk/business-and-government/products/terrain-50.html
#       This gave me the "terr50_gagg_gb.zip" file that is used here.

# NOTE: I downloaded the "OS Terrain 50" dataset in the "ESRI Shape (Contours)"
#       file format from the Ordnance Survey, see:
#         * https://www.ordnancesurvey.co.uk/business-and-government/products/terrain-50.html
#       This gave me the "terr50_cesh_gb.zip" file that is used here.

# ******************************************************************************

# Import standard modules ...
import io
import re
import zipfile

# Import special modules ...
try:
    import numpy
except:
    raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

# NOTE: https://scitools.org.uk/cartopy/docs/latest/crs/projections.html#osgb

# Define function ...
def loadASCIIcontents(fobj, n):
    # Read ASCII dataset (and reset pointer) ...
    contents = numpy.loadtxt(fobj, delimiter = " ", dtype = numpy.float32, skiprows = n)
    fobj.seek(0)

    # Return contents ...
    return contents

# Define function ...
def loadASCIIheader(fobj):
    # Read lines from ASCII dataset (and reset pointer) ...
    lines = fobj.readlines()
    fobj.seek(0)

    # Populate header ...
    header = {
            "ncols" : int(lines[0].decode("ascii").strip().split()[1]),
            "nrows" : int(lines[1].decode("ascii").strip().split()[1]),
        "xllcorner" : int(lines[2].decode("ascii").strip().split()[1]),
        "yllcorner" : int(lines[3].decode("ascii").strip().split()[1]),
         "cellsize" : int(lines[4].decode("ascii").strip().split()[1]),
    }

    # Determine length of header ...
    header["length"] = len(lines) - header["nrows"]

    # Clean up ...
    del lines

    # Return header ...
    return header

# Define function ...
def findExtent(fname0):
    # Initialize limits ...
    maxX = -2**31
    maxY = -2**31
    minX =  2**31
    minY =  2**31

    # Compile regex to save time ...
    pattern = re.compile(r"data/[a-z]+/[a-z]+[0-9]+_OST50GRID_[0-9]+.zip")

    # Load dataset ...
    with zipfile.ZipFile(fname0, "r") as fobj0:
        # Loop over members ...
        for fname1 in fobj0.namelist():
            # Skip this member if it is not a sub-dataset ...
            if pattern.match(fname1) is None:
                continue

            # Determine sub-dataset key ...
            key = fname1.split("/")[-1].split("_")[0].upper()

            # Read sub-dataset into RAM so that it becomes seekable ...
            # NOTE: https://stackoverflow.com/a/12025492
            zipObj = io.BytesIO(fobj0.read(fname1))

            # Load sub-dataset ...
            with zipfile.ZipFile(zipObj, "r") as fobj1:
                # Read ASCII dataset into RAM so that it becomes seekable ...
                # NOTE: https://stackoverflow.com/a/12025492
                ascObj = io.BytesIO(fobj1.read(f"{key}.asc"))

                # Load header of ASCII dataset ...
                hdr = loadASCIIheader(ascObj)

                # Clean up ...
                del ascObj

                # Increment limits ...
                maxX = max(maxX, hdr["xllcorner"] // hdr["cellsize"] + hdr["ncols"])
                maxY = max(maxY, hdr["yllcorner"] // hdr["cellsize"] + hdr["nrows"])
                minX = min(minX, hdr["xllcorner"] // hdr["cellsize"])
                minY = min(minY, hdr["yllcorner"] // hdr["cellsize"])

            # Clean up ...
            del zipObj

    # Return answers ...
    return minX, maxX, minY, maxY

# ******************************************************************************

# Specify the path to the "OS Terrain 50" dataset ...
zname = "terr50_gagg_gb.zip"

# Find extent of the "OS Terrain 50" dataset ...
x1, x2, y1, y2 = findExtent(zname)

# Set array sizes ...
nx = x2 - x1
ny = y2 - y1

# Make map ...
elev = numpy.zeros((ny, nx), dtype = numpy.float32)                             # [m]

# Compile regex to save time ...
pattern = re.compile(r"data/[a-z]+/[a-z]+[0-9]+_OST50GRID_[0-9]+.zip")

# Load dataset ...
with zipfile.ZipFile(zname, "r") as fobj0:
    # Loop over members ...
    for fname1 in fobj0.namelist():
        # Skip this member if it is not a sub-dataset ...
        if pattern.match(fname1) is None:
            continue

        # Determine sub-dataset key ...
        key = fname1.split("/")[-1].split("_")[0].upper()

        # Read sub-dataset into RAM so that it becomes seekable ...
        # NOTE: https://stackoverflow.com/a/12025492
        zipObj = io.BytesIO(fobj0.read(fname1))

        # Load sub-dataset ...
        with zipfile.ZipFile(zipObj, "r") as fobj1:
            # Read ASCII dataset into RAM so that it becomes seekable ...
            # NOTE: https://stackoverflow.com/a/12025492
            ascObj = io.BytesIO(fobj1.read(f"{key}.asc"))

            # Load header and contents of ASCII dataset ...
            hdr = loadASCIIheader(ascObj)
            cont = loadASCIIcontents(ascObj, hdr["length"])                     # [m]

            # Clean up ...
            del ascObj

            # Determine indexes (from the upper-left corner not the lower-left
            # corner) ...
            ix1 = hdr["xllcorner"] // hdr["cellsize"]
            ix2 = hdr["xllcorner"] // hdr["cellsize"] + hdr["ncols"]
            iy1 = ny - (hdr["yllcorner"] // hdr["cellsize"] + hdr["nrows"])
            iy2 = ny - (hdr["yllcorner"] // hdr["cellsize"])

            # Populate array ...
            elev[iy1:iy2, ix1:ix2] = cont[:, :]                                 # [m]

            # Clean up ...
            del hdr
            del cont

        # Clean up ...
        del zipObj

# Save BIN ...
elev.tofile("terr50_gagg_gb.bin")
