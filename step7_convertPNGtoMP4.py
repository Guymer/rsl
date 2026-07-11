#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.13/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import glob
    import platform
    import shutil

    # Import special modules ...
    try:
        import matplotlib
        matplotlib.rcParams.update(
            {
                       "axes.xmargin" : 0.01,
                       "axes.ymargin" : 0.01,
                            "backend" : "Agg",                                  # NOTE: See https://matplotlib.org/stable/gallery/user_interfaces/canvasagg.html
                         "figure.dpi" : 300,
                     "figure.figsize" : (9.6, 7.2),                             # NOTE: See https://github.com/Guymer/misc/blob/main/README.md#matplotlib-figure-sizes
                          "font.size" : 8,
                "image.interpolation" : "none",                                 # NOTE: See https://matplotlib.org/stable/gallery/images_contours_and_fields/interpolation_methods.html
                     "image.resample" : False,
            }
        )
        import matplotlib.pyplot
    except:
        raise Exception("\"matplotlib\" is not installed; run \"pip install --user matplotlib\"") from None
    try:
        import PIL
        import PIL.Image
        import PIL.ImageDraw
        import PIL.ImageFont
        PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                         # [px]
    except:
        raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None

    # Import my modules ...
    try:
        import pyguymer3
        import pyguymer3.media
    except:
        raise Exception("\"pyguymer3\" is not installed; run \"pip install --user PyGuymer3\"") from None

    # **************************************************************************

    # Create argument parser and parse the arguments ...
    parser = argparse.ArgumentParser(
           allow_abbrev = False,
            description = "Convert PNG images to MP4 videos.",
        formatter_class = argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "--debug",
        action = "store_true",
          help = "print debug messages",
    )
    parser.add_argument(
        "--ffmpeg-path",
        default = shutil.which("ffmpeg7") if platform.system() == "Darwin" else shutil.which("ffmpeg"),
           dest = "ffmpegPath",
           help = "the path to the \"ffmpeg\" binary",
           type = str,
    )
    parser.add_argument(
        "--ffprobe-path",
        default = shutil.which("ffprobe7") if platform.system() == "Darwin" else shutil.which("ffprobe"),
           dest = "ffprobePath",
           help = "the path to the \"ffprobe\" binary",
           type = str,
    )
    parser.add_argument(
        "--timeout",
        default = 60.0,
           help = "the timeout for any requests/subprocess calls (in seconds)",
           type = float,
    )
    args = parser.parse_args()

    # **************************************************************************

    # Create short-hands ...
    fontPath = matplotlib.font_manager.findfont("DejaVu Sans Mono")
    fontSize = 72                                                               # [px]
    font = PIL.ImageFont.truetype(fontPath, fontSize)

    # Find the frames ...
    frames = sorted(glob.glob("output/????m_010x.png"))

    # Make images with the sea level overlaid ...
    images = []
    for frame in frames:
        level = int(frame.split("/")[1].split("_")[0].removesuffix("m"))        # [m]
        with PIL.Image.open(frame) as iObj:
            image = iObj.convert("RGB")
        draw = PIL.ImageDraw.Draw(image)
        draw.text(
            (image.size[0] - 100, 100),
            f"{level:,d} m sea level rise",
            anchor = "rs",
              fill = (255, 255, 255),
              font = font,
        )
        images.append(image)

    # **************************************************************************

    print("Making \"createFlood.mp4\" ...")

    # Save 25fps MP4 ...
    vname = pyguymer3.media.images2mp4(
        images,
              debug = args.debug,
        ffprobePath = args.ffprobePath,
         ffmpegPath = args.ffmpegPath,
            timeout = args.timeout,
    )
    shutil.move(vname, "createFlood.mp4")

    # **************************************************************************

    # Set maximum sizes ...
    # NOTE: By inspection, the PNG frames are 2,460 px tall.
    maxSizes = [256, 512, 1024, 2048]                                           # [px]

    # Loop over maximum sizes ...
    for maxSize in maxSizes:
        print(f"Making \"createFlood{maxSize:04d}px.mp4\" ...")

        # Save 25fps MP4 ...
        vname = pyguymer3.media.images2mp4(
            images,
                   debug = args.debug,
             ffprobePath = args.ffprobePath,
              ffmpegPath = args.ffmpegPath,
            screenHeight = maxSize,
             screenWidth = maxSize,
                 timeout = args.timeout,
        )
        shutil.move(vname, f"createFlood{maxSize:04d}px.mp4")
