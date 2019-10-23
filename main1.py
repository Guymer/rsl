#!/usr/bin/env python3

# NOTE: I downloaded the "OS Terrain 50" dataset in the "ESRI Shape (Contours)"
#       file format from the Ordnance Survey, see:
#         * https://www.ordnancesurvey.co.uk/business-and-government/products/terrain-50.html
#       This gave me the "terr50_cesh_gb.zip" file that is used here.

# Specify the path to the "OS Terrain 50" dataset ...
fname0 = "terr50_cesh_gb.zip"

# ******************************************************************************

# Import standard modules ...
import io
import re
import zipfile

# Import special modules ...
try:
    import cartopy
except:
    raise Exception("run \"pip install --user cartopy\"")
try:
    import shapefile
except:
    raise Exception("run \"pip install --user pyshp\"")

# Compile regex to save time ...
pat1 = re.compile(r"data/[a-z]+/[a-z]+[0-9]+_OST50CONT_[0-9]+.zip")

# Load dataset ...
with zipfile.ZipFile(fname0, "r") as fobj0:
    # Loop over members ...
    for fname1 in fobj0.namelist():
        # Skip this member if it is not a sub-dataset ...
        if pat1.match(fname1) is None:
            continue

        # Determine sub-dataset key ...
        key = fname1.split("/")[-1].split("_")[0].upper()

        # Read sub-dataset into RAM so that it becomes seekable ...
        # NOTE: https://stackoverflow.com/a/12025492
        zip_src = io.BytesIO(fobj0.read(fname1))

        # Load sub-dataset ...
        with zipfile.ZipFile(zip_src, "r") as fobj1:
            # Read files into RAM so that they become seekable ...
            # NOTE: https://stackoverflow.com/a/12025492
            dbf_src = io.BytesIO(fobj1.read(key + "_line.dbf"))
            shp_src = io.BytesIO(fobj1.read(key + "_line.shp"))
            shx_src = io.BytesIO(fobj1.read(key + "_line.shx"))

            print(fobj1.read(key + "_line.prj"))

            # Open shapefile ...
            fobj2 = shapefile.Reader(dbf = dbf_src, shp = shp_src, shx = shx_src)

            # Loop over shape+record pairs ...
            for shapeRecord in fobj2.shapeRecords():
                # Crash if this shape+record is not a polyline ...
                # NOTE: The shapefile is only supposed to contain contours
                #       and tide marks.
                if shapeRecord.shape.shapeType != shapefile.POLYLINE:
                    raise Exception("shape is not a POLYLINE")

                # Skip this shape+record if it is not a contour ...
                # NOTE: It is probably a tide mark.
                if shapeRecord.record[1] != "ContourLine":
                    continue

                print(shapeRecord.record)
                print(shapeRecord.shape)
                exit()
