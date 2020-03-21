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
    import shapefile
except:
    raise Exception("run \"pip install --user pyshp\"")

# NOTE: https://scitools.org.uk/cartopy/docs/latest/crs/projections.html#osgb

# Specify the path to the "OS Terrain 50" dataset ...
fname0 = "terr50_cesh_gb.zip"

# Compile regex to save time ...
pattern = re.compile(r"data/[a-z]+/[a-z]+[0-9]+_OST50CONT_[0-9]+.zip")

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
            dbfObj = io.BytesIO(fobj1.read(key + "_line.dbf"))
            shpObj = io.BytesIO(fobj1.read(key + "_line.shp"))
            shxObj = io.BytesIO(fobj1.read(key + "_line.shx"))

            print(fobj1.read(key + "_line.prj"))

            # Open shapefile ...
            fobj2 = shapefile.Reader(dbf = dbfObj, shp = shpObj, shx = shxObj)

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

            # Clean up ...
            del fobj2
            del dbfObj
            del shpObj
            del shxObj

        # Clean up ...
        del zipObj
