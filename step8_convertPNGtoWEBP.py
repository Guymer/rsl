#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import glob

    # Import special modules ...
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

    # Create short-hand ...
    font = PIL.ImageFont.truetype("SFNSMono.ttf", 72)

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

    print("Making \"createFlood.webp\" ...")

    # Save 25fps WEBP ...
    pyguymer3.media.images2webp(
        images,
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
            images,
            f"createFlood{maxSize:04d}px.webp",
            screenHeight = maxSize,
             screenWidth = maxSize,
        )
