# Define function ...
def loadASCIIheader(fObj):
    # Read lines from ASCII dataset (and reset pointer) ...
    lines = fObj.readlines()
    fObj.seek(0)

    # Populate header ...
    header = {
            "ncols" : int(lines[0].decode("ascii").strip().split()[1]),
            "nrows" : int(lines[1].decode("ascii").strip().split()[1]),
        "xllcorner" : int(lines[2].decode("ascii").strip().split()[1]),
        "yllcorner" : int(lines[3].decode("ascii").strip().split()[1]),
         "cellsize" : int(lines[4].decode("ascii").strip().split()[1]),
    }

    # Determine length of header ...
    header["length"] = len(lines) - header["nrows"]

    # Clean up ...
    del lines

    # Return header ...
    return header
