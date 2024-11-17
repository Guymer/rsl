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
    import os
    import re
    import zipfile

    # Import special modules ...
    try:
        import cartopy
        cartopy.config.update(
            {
                "cache_dir" : os.path.expanduser("~/.local/share/cartopy_cache"),
            }
        )
    except:
        raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None
    try:
        import shapefile
    except:
        raise Exception("\"shapefile\" is not installed; run \"pip install --user pyshp\"") from None

    # Import my modules ...
    try:
        import pyguymer3
        import pyguymer3.geo
    except:
        raise Exception("\"pyguymer3\" is not installed; you need to have the Python module from https://github.com/Guymer/PyGuymer3 located somewhere in your $PYTHONPATH") from None

    # **************************************************************************
    # *                   PART 1: FIND OUT HOW BIG THE UK IS                   *
    # **************************************************************************

    # Find file containing all the country shapes ...
    sfile = cartopy.io.shapereader.natural_earth(
        resolution = "10m",
          category = "cultural",
              name = "admin_0_countries",
    )

    # Loop over records ...
    for record in cartopy.io.shapereader.Reader(sfile).records():
        # Create short-hand ...
        neName = pyguymer3.geo.getRecordAttribute(record, "NAME")

        # Skip this record if it is not for the UK ...
        if neName != "United Kingdom":
            continue

        # Find the bounding box ...
        lon_min, lat_min, lon_max, lat_max = record.bounds                      # [°], [°], [°], [°]

    # NOTE: By manual inspection on 2019-01-19, the UK is 15.462484° wide and
    #       10.938273° tall (don't forget Rockall).
    print(f"{lon_min:7.3f}° <= lon <= {lon_max:7.3f}° ({lon_max - lon_min:.6f}° wide)")
    print(f"{lat_min:7.3f}° <= lat <= {lat_max:7.3f}° ({lat_max - lat_min:.6f}° tall)")

    # **************************************************************************
    # *              PART 2: LOAD THE OS CONTOURS AND MAKE MASKS               *
    # **************************************************************************

    # Specify the path to the "OS Terrain 50" dataset ...
    fname0 = "terr50_cesh_gb.zip"

    # Compile regex to save time ...
    pattern = re.compile(r"data/[a-z]+/[a-z]+[0-9]+_OST50CONT_[0-9]+.zip")

    # Initialize list and dictionary ...
    # NOTE: By manual inspection on 2019-01-19, the limits are -11 and 134.
    levels = []                                                                 # [10m]
    contours = {}
    for level in range(-11, 135):
        levels.append(level)                                                    # [10m]
        contours[level] = 0                                                     # [#]

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
                dbfObj = io.BytesIO(fObj1.read(f"{key}_line.dbf"))
                shpObj = io.BytesIO(fObj1.read(f"{key}_line.shp"))
                shxObj = io.BytesIO(fObj1.read(f"{key}_line.shx"))

                # Open shapefile ...
                fObj2 = shapefile.Reader(
                    dbf = dbfObj,
                    shp = shpObj,
                    shx = shxObj,
                )

                # Loop over shape+record pairs ...
                for shapeRecord in fObj2.shapeRecords():
                    # Crash if this shape+record is not a polyline ...
                    # NOTE: The shapefile is only supposed to contain contours
                    #       and tide marks.
                    if shapeRecord.shape.shapeType != shapefile.POLYLINE:
                        raise Exception("shape is not a POLYLINE") from None

                    # Skip this shape+record if it is not a contour ...
                    # NOTE: It is probably a tide mark.
                    if shapeRecord.record[1] != "ContourLine":
                        continue

                    # Convert elevation into index and append to list ...
                    level = round(shapeRecord.record[3] / 10.0)                 # [10m]
                    contours[level] += 1                                        # [#]

    # NOTE: By manual inspection there are 146 levels. On a MacBook Pro with 8
    #       GB of RAM that is upto 52.26 MiB of RAM per level, before swapping
    #       occurs. On a NAS with 16 GB of RAM that is upto 104.51 MiB of RAM
    #       per level, before swapping occurs (ignoring the ZFS caches). Lets go
    #       for 40,000 pixels by 60,000 pixels (with a pixel being approximately
    #       0.25 millidegrees).

    print(f"{len(levels):d} elevation levels")
    for level in levels:
        print(level, contours[level])
