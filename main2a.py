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
# import sys
import zipfile

# NOTE: https://scitools.org.uk/cartopy/docs/latest/crs/projections.html#osgb

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

# Specify the path to the "OS Terrain 50" dataset ...
fname0 = "terr50_gagg_gb.zip"

# Initizalize limits ...
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
            # Read files into RAM so that they become seekable ...
            # NOTE: https://stackoverflow.com/a/12025492
            ascObj = io.BytesIO(fobj1.read(key + ".asc"))
            auxObj = io.BytesIO(fobj1.read(key + ".asc.aux.xml"))
            gmlObj = io.BytesIO(fobj1.read(key + ".gml"))
            prjObj = io.BytesIO(fobj1.read(key + ".prj"))
            xmlObj = io.BytesIO(fobj1.read("Metadata_" + key + ".xml"))

            # Load header of ASCII dataset ...
            hdr = loadASCIIheader(ascObj)

            # Increment limits ...
            maxX = max(maxX, hdr["xllcorner"] // hdr["cellsize"] + hdr["ncols"])
            maxY = max(maxY, hdr["yllcorner"] // hdr["cellsize"] + hdr["nrows"])
            minX = min(minX, hdr["xllcorner"] // hdr["cellsize"])
            minY = min(minY, hdr["yllcorner"] // hdr["cellsize"])

            # # Save a copy of the files locally for inspection ...
            # open(key + ".asc", "wb").write(ascObj.read())
            # open(key + ".asc.aux.xml", "wb").write(auxObj.read())
            # open(key + ".gml", "wb").write(gmlObj.read())
            # open(key + ".prj", "wb").write(prjObj.read())
            # open("Metadata_" + key + ".xml", "wb").write(xmlObj.read())
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