#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.11/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import glob

    # Import my modules ...
    try:
        import pyguymer3
        import pyguymer3.media
    except:
        raise Exception("\"pyguymer3\" is not installed; you need to have the Python module from https://github.com/Guymer/PyGuymer3 located somewhere in your $PYTHONPATH") from None

    # **************************************************************************

    # Find the frames ...
    frames = sorted(glob.glob("output/0[0-4][0-9][0-9]m.png"))

    # **************************************************************************

    print("Making \"createFlood.webp\" ...")

    # Save 25fps WEBP ...
    pyguymer3.media.images2webp(
        frames,
        "createFlood.webp",
    )

    # **************************************************************************

    # Set maximum sizes ...
    # NOTE: By inspection, the PNG frames are 2,460 px tall.
    maxSizes = [512, 1024, 2048]                                                # [px]

    # Loop over maximum sizes ...
    for maxSize in maxSizes:
        print(f"Making \"createFlood{maxSize:04d}px.webp\" ...")

        # Save 25fps WEBP ...
        pyguymer3.media.images2webp(
            frames,
            f"createFlood{maxSize:04d}px.webp",
            screenHeight = maxSize,
             screenWidth = maxSize,
        )
