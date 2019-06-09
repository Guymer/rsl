#!/usr/bin/env python2
# -*- coding: utf-8 -*-

# NOTE: I downloaded the "OS Terrain 50" dataset in the "ESRI Shape (Contours)"
#       file format from the Ordnance Survey, see:
#         * https://www.ordnancesurvey.co.uk/business-and-government/products/terrain-50.html
#       This gave me the "terr50_cesh_gb.zip" file that is used here.

# Specify the path to the "OS Terrain 50" dataset ...
fname0 = "terr50_cesh_gb.zip"

# ******************************************************************************

# Import modules ...
import cartopy
import io
import re
import shapefile
import zipfile

# ******************************************************************************
# *                     PART 1: FIND OUT HOW BIG THE UK IS                     *
# ******************************************************************************

# Find file containing all the country shapes ...
shape_file = cartopy.io.shapereader.natural_earth(
    resolution = "10m",
    category = "cultural",
    name = "admin_0_countries"
)

# Loop over records ...
for record in cartopy.io.shapereader.Reader(shape_file).records():
    # Skip this record if it is not for a country in the list ...
    if record.attributes["NAME"] != "United Kingdom":
        continue

    # Find the bounding box ...
    lon_min, lat_min, lon_max, lat_max = record.bounds

# NOTE: By manual inspection the UK is 15.462484 wide and 10.938273 tall (don't
#       forget Rockall).
print "{:7.3f} <= x <= {:7.3f} ({:.6f} wide)".format(lon_min, lon_max, lon_max - lon_min)
print "{:7.3f} <= y <= {:7.3f} ({:.6f} tall)".format(lat_min, lat_max, lat_max - lat_min)

# ******************************************************************************
# *                PART 2: LOAD THE OS CONTOURS AND MAKE MASKS                 *
# ******************************************************************************

# NOTE: https://scitools.org.uk/cartopy/docs/latest/crs/projections.html#osgb

# Compile regex to save time ...
pattern = re.compile(r"data/[a-z]+/[a-z]+[0-9]+_OST50CONT_[0-9]+.zip")

# Initizalize list and dictionary ...
# NOTE: By manual inspection on 2019-01-19 the limits are -11 and 134.
levels = []                                                                     # [10m]
contours = {}
for level in xrange(-11, 135):
    levels.append(level)                                                        # [10m]
    contours[level] = 0
    #contours[level] = []

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
        zip_src = io.BytesIO(fobj0.read(fname1))

        # Load sub-dataset ...
        with zipfile.ZipFile(zip_src, "r") as fobj1:
            # Read files into RAM so that they become seekable ...
            # NOTE: https://stackoverflow.com/a/12025492
            dbf_src = io.BytesIO(fobj1.read(key + "_line.dbf"))
            shp_src = io.BytesIO(fobj1.read(key + "_line.shp"))
            shx_src = io.BytesIO(fobj1.read(key + "_line.shx"))

            #print fobj1.read(key + "_line.prj")

            # Load files as a shapefile ...
            with shapefile.Reader(dbf = dbf_src, shp = shp_src, shx = shx_src) as fobj2:
                # Loop over shape+record pairs ...
                for shapeRecord in fobj2.shapeRecords():
                    # Crash if this shape+record is not a polyline ...
                    # NOTE: The shapefile is only supposed to contain contours
                    #       and tide marks.
                    if shapeRecord.shape.shapeTypeName != "POLYLINE":
                        raise Exception("shape is not a POLYLINE")

                    # Skip this shape+record if it is not a contour ...
                    # NOTE: It is probably a tide mark.
                    if shapeRecord.record.FEAT_TYPE != "ContourLine":
                        continue

                    # Convert elevation into index and append to list ...
                    level = int(shapeRecord.record.PROP_VALUE) / 10             # [10m]
                    contours[level] += 1
                    #contours[level].append(shapeRecord.shape)

            # Clean up ...
            del dbf_src
            del shp_src
            del shx_src

        # Clean up ...
        del zip_src

# NOTE: By manual inspection there are 146 levels. On a MacBook Pro with 8 GB of
#       RAM that is upto 52.26 MiB of RAM per level, before swapping occurs. On
#       a NAS with 16 GB of RAM that is upto 104.51 MiB of RAM per level, before
#       swapping occurs (ignoring the ZFS caches). Lets go for 40,000 pixels by
#       60,000 pixels (with a pixel being approximately 0.25 millidegrees).

print "{:d} elevation levels".format(len(levels))
for level in levels:
    print level, contours[level]
