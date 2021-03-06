#!/usr/bin/env python3

# Import standard modules ...
import glob

# Import special modules ...
try:
    import PIL
    import PIL.Image
except:
    raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None

# Import my modules ...
try:
    import pyguymer3
except:
    raise Exception("\"pyguymer3\" is not installed; you need to have the Python module from https://github.com/Guymer/PyGuymer3 located somewhere in your $PYTHONPATH") from None

# Configure PIL to open images up to 1 GiP ...
PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                                 # [px]

# ******************************************************************************

# Initizalize list ...
images = []

# Loop over frames (0,000m to 0,499m) ...
for frame in sorted(glob.glob("createFlood_0[0-4][0-9][0-9]m.png")):
    # Open image as RGB (even if it is paletted) ...
    image = PIL.Image.open(frame).convert("RGB")

    # Append it to the list ...
    images.append(image)

# Save 25fps GIF ...
images[0].save("createFlood.gif", save_all = True, append_images = images[1:], duration = 40, loop = 0)
pyguymer3.optimize_image("createFlood.gif", strip = True)

# Clean up ...
for image in images:
    image.close()
del images

# ******************************************************************************

# Set widths ...
# NOTE: By inspection, the PNG frames are 1320 wide.
widths = [512, 1024]                                                            # [px]

# Loop over widths ...
for width in widths:
    # Initizalize list ...
    images = []

    # Loop over frames (0,000m to 0,499m) ...
    for frame in sorted(glob.glob("createFlood_0[0-4][0-9][0-9]m.png")):
        # Open image as RGB (even if it is paletted) ...
        image = PIL.Image.open(frame).convert("RGB")

        # Calculate height ...
        ratio = float(image.size[0]) / float(image.size[1])                     # [px/px]
        height = round(float(width) / ratio)                                    # [px]

        # Downscale the image and append it to the list ...
        images.append(image.resize((width, height), resample = PIL.Image.LANCZOS))

    # Save 25fps GIF ...
    images[0].save("createFlood{:04d}px.gif".format(width), save_all = True, append_images = images[1:], duration = 40, loop = 0)
    pyguymer3.optimize_image("createFlood{:04d}px.gif".format(width), strip = True)

    # Clean up ...
    for image in images:
        image.close()
    del images
