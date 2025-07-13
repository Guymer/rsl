#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import glob
    import json
    import re

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Import my modules ...
    try:
        import pyguymer3
        import pyguymer3.image
    except:
        raise Exception("\"pyguymer3\" is not installed; run \"pip install --user PyGuymer3\"") from None

    # **************************************************************************

    # Create argument parser and parse the arguments ...
    parser = argparse.ArgumentParser(
           allow_abbrev = False,
            description = "Convert BIN files to PNG images.",
        formatter_class = argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "--debug",
        action = "store_true",
          help = "print debug messages",
    )
    parser.add_argument(
        "--maximum-size",
        default = 250.0,
           dest = "maxSize",
           help = "the maximum size of image to make a PNG for (in mega-pixels)",
           type = float,
    )
    args = parser.parse_args()

    # **************************************************************************

    # Load colour tables and create short-hand ...
    with open(f"{pyguymer3.__path__[0]}/data/json/colourTables.json", "rt", encoding = "utf-8") as fObj:
        colourTables = json.load(fObj)
    turbo = numpy.array(colourTables["turbo"]).astype(numpy.uint8)

    # Define the size of the dataset ...
    nx = 13200                                                                  # [px]
    ny = 24600                                                                  # [px]

    # Find files ...
    fNames = sorted(glob.glob("*.*") + glob.glob("output/*.*"))

    # Loop over files ...
    for bName in fNames:
        # Skip this file if it is not a BIN file ...
        if not bName.endswith(".bin"):
            continue

        # Create short-hand and skip this BIN file if the associated PNG file
        # already exists ...
        pName = f'{bName.removesuffix(".bin")}.png'
        if pName in fNames:
            print(f"Skipping \"{bName}\" (the PNG already exists).")
            continue

        # Figure out what to do with it ...
        match bName:
            case str(x) if re.fullmatch(r"output/[0-9][0-9][0-9][0-9]m_[0-9][0-9][0-9]x.bin", x):
                # Find scale ...
                scale = int(bName.split("_")[1][:3])

                # Find out how many mega-pixels there are and skip this BIN if
                # the PNG would be too big ...
                mega = float((nx // scale) * (ny // scale)) / 1.0e6             # [Mpx]
                if mega > args.maxSize:
                    print(f"Skipping \"{bName}\" (the PNG would be {mega:,.1f} Mpx).")
                    continue

                print(f"Making \"{pName}\" ...")

                # Load data ...
                data = numpy.fromfile(
                    bName,
                    dtype = numpy.int8,
                ).astype(numpy.uint8).reshape(ny // scale, nx // scale, 1)

                # Make PNG ...
                src = pyguymer3.image.makePng(
                    data,
                    calcAdaptive = True,
                     calcAverage = True,
                        calcNone = True,
                       calcPaeth = True,
                         calcSub = True,
                          calcUp = True,
                         choices = "all",
                           debug = args.debug,
                             dpi = None,
                          levels = [9,],
                       memLevels = [9,],
                         modTime = None,
                        palUint8 = numpy.array(
                        [
                            [  0,   0, 255],                                    # Blue
                            [255,   0,   0],                                    # Red
                            [  0, 255,   0],                                    # Green
                        ],
                        dtype = numpy.uint8,
                    ),
                      strategies = None,
                          wbitss = [15,],
                )
                with open(pName, "wb") as fObj:
                    fObj.write(src)
            case str(x) if x.startswith("terr50_gagg_gb"):
                # Find scale ...
                if bName.endswith("x.bin"):
                    scale = int(bName.split("_")[3][:3])
                else:
                    scale = 1

                # Find out how many mega-pixels there are and skip this BIN if
                # the PNG would be too big ...
                mega = float((nx // scale) * (ny // scale)) / 1.0e6             # [Mpx]
                if mega > args.maxSize:
                    print(f"Skipping \"{bName}\" (the PNG would be {mega:,.1f} Mpx).")
                    continue

                print(f"Making \"{pName}\" ...")

                # Load data ...
                data = numpy.fromfile(
                    bName,
                    dtype = numpy.float32,
                ).reshape(ny // scale, nx // scale, 1)

                # Scale data from 0 to 255, mapping it from 0 m to 1,300 m ...
                data = 255.0 * (data / 1300.0)
                numpy.place(data, data <   0.0,   0.0)
                numpy.place(data, data > 255.0, 255.0)
                data = data.astype(numpy.uint8)

                # Make PNG ...
                src = pyguymer3.image.makePng(
                    data,
                    calcAdaptive = True,
                     calcAverage = True,
                        calcNone = True,
                       calcPaeth = True,
                         calcSub = True,
                          calcUp = True,
                         choices = "all",
                           debug = args.debug,
                             dpi = None,
                          levels = [9,],
                       memLevels = [9,],
                         modTime = None,
                        palUint8 = turbo,
                      strategies = None,
                          wbitss = [15,],
                )
                with open(pName, "wb") as fObj:
                    fObj.write(src)
            case _:
                raise ValueError(f"there is no case which matches \"{bName}\"") from None
