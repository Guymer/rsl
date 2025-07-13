#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import glob

    # Import special modules ...
    try:
        import matplotlib
        matplotlib.rcParams.update(
            {
                       "backend" : "Agg",                                       # NOTE: See https://matplotlib.org/stable/gallery/user_interfaces/canvasagg.html
                    "figure.dpi" : 300,
                "figure.figsize" : (9.6, 7.2),                                  # NOTE: See https://github.com/Guymer/misc/blob/main/README.md#matplotlib-figure-sizes
                     "font.size" : 8,
            }
        )
        import matplotlib.pyplot
    except:
        raise Exception("\"matplotlib\" is not installed; run \"pip install --user matplotlib\"") from None
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
            description = "Plot the convergence.",
        formatter_class = argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "--debug",
        action = "store_true",
          help = "print debug messages",
    )
    parser.add_argument(
        "--timeout",
        default = 60.0,
           help = "the timeout for any requests/subprocess calls (in seconds)",
           type = float,
    )
    args = parser.parse_args()

    # **************************************************************************

    # Find the CSV output and create short-hands ...
    cNames = sorted(glob.glob("output/????m_010x.csv"))
    maxX = 0                                                                    # [#]
    maxY = None                                                                 # [px]
    minY = None                                                                 # [px]
    n = len(cNames)                                                             # [#]

    # Create figure ...
    fg = matplotlib.pyplot.figure()

    # Create axis ...
    ax = fg.add_subplot()

    # Loop over CSVs ...
    for elev, cName in enumerate(cNames):
        # Load data ...
        iIter, tot = numpy.loadtxt(
            cName,
            delimiter = ",",
                dtype = numpy.uint64,
             skiprows = 1,
               unpack = True,
        )                                                                       # [#], [px]

        # Increment limits ...
        maxX = max(maxX, int(iIter[-1]))                                        # [#]
        if elev == 0:
            minY = tot[1]                                                       # [px]
        if elev == n - 1:
            maxY = tot[-1]                                                      # [px]

        # Plot data ...
        if elev % 250 == 0:
            ax.plot(
                iIter,
                tot,
                color = matplotlib.colormaps["turbo"](float(elev) / float(n - 1)),
                label = f"{elev:,d} m sea level rise",
            )
        else:
            ax.plot(
                iIter,
                tot,
                color = matplotlib.colormaps["turbo"](float(elev) / float(n - 1)),
            )

    # Configure axis ...
    ax.grid()
    ax.legend(loc = "upper right")
    ax.set_xlabel("Iteration [#]")
    ax.set_xlim(0, maxX)
    ax.set_xticks(range(maxX + 1))
    ax.set_ylabel("Pixels Allowed [px]")
    ax.set_ylim(minY, maxY)

    # Configure figure ...
    fg.tight_layout()

    # Save figure ...
    fg.savefig("step9.png")
    matplotlib.pyplot.close(fg)

    # Optimize PNG ...
    pyguymer3.image.optimise_image(
        "step9.png",
          debug = args.debug,
          strip = True,
        timeout = args.timeout,
    )
