#!/usr/bin/env python3

# Import standard modules ...
import glob

# Import special modules ...
try:
    import PIL
    import PIL.Image
except:
    raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None

# Configure PIL to open images up to 1 GiP ...
PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                                 # [px]

# ******************************************************************************

print("Making \"createFlood.webp\" ...")

# Initialize list ...
images = []

# Loop over frames (0,000m to 0,499m) ...
for frame in sorted(glob.glob("createFlood_0[0-4][0-9][0-9]m.png")):
    # Open image as RGB (even if it is paletted) ...
    image = PIL.Image.open(frame).convert("RGB")

    # Append it to the list ...
    images.append(image)

# Save 25fps WEBP ...
images[0].save("createFlood.webp", lossless = True, quality = 100, method = 6, save_all = True, append_images = images[1:], duration = 40, loop = 0, minimize_size = True)

# Clean up ...
for image in images:
    image.close()
del images

# ******************************************************************************

# Set heights ...
# NOTE: By inspection, the PNG frames are 2460px tall.
heights = [256, 512, 1024, 2048]                                                # [px]

# Loop over heights ...
for height in heights:
    print(f"Making \"createFlood{height:04d}px.webp\" ...")

    # Initialize list ...
    images = []

    # Loop over frames (0,000m to 0,499m) ...
    for frame in sorted(glob.glob("createFlood_0[0-4][0-9][0-9]m.png")):
        # Open image as RGB (even if it is paletted) ...
        image = PIL.Image.open(frame).convert("RGB")

        # Calculate width ...
        ratio = float(image.width) / float(image.height)                        # [px/px]
        width = round(ratio * float(height))                                    # [px]

        # Downscale the image and append it to the list ...
        images.append(image.resize((width, height), resample = PIL.Image.Resampling.LANCZOS))

    # Save 25fps WEBP ...
    images[0].save(f"createFlood{height:04d}px.webp", lossless = True, quality = 100, method = 6, save_all = True, append_images = images[1:], duration = 40, loop = 0, minimize_size = True)

    # Clean up ...
    for image in images:
        image.close()
    del images
