#!/usr/bin/env python3

# Import standard modules ...
import glob

# Import special modules ...
try:
    import PIL.Image
except:
    raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"")

# Import my modules ...
try:
    import pyguymer3
except:
    raise Exception("\"pyguymer3\" is not installed; you need to have the Python module from https://github.com/Guymer/PyGuymer3 located somewhere in your $PYTHONPATH")

# Set scale ...
scale = 2

# Initizalize list ...
images = []

# Loop over frames (0,000m to 0,499m) ...
for frame in sorted(glob.glob("createFlood_0[0-4][0-9][0-9]m.png")):
    # Open image as RGB (even if it is paletted) ...
    image = PIL.Image.open(frame).convert("RGB")

    # Check that it can be scaled correctly ...
    if image.size[0] % scale != 0:
        raise Exception("width is not an integer multiple of \"scale\"")
    if image.size[1] % scale != 0:
        raise Exception("height is not an integer multiple of \"scale\"")

    # Downscale the image and append it to the list ...
    images.append(image.resize((image.size[0] // scale, image.size[1] // scale), resample = PIL.Image.LANCZOS))

# Save GIF ...
images[0].save("createFlood.gif", save_all = True, append_images = images[1:], duration = 40, loop = 0)
pyguymer3.exiftool("createFlood.gif")
pyguymer3.gifsicle("createFlood.gif")
