# Define function ...
def loadASCIIcontents(fObj, n):
    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Read ASCII dataset (and reset pointer) ...
    contents = numpy.loadtxt(fObj, delimiter = " ", dtype = numpy.float32, skiprows = n)
    fObj.seek(0)

    # Return contents ...
    return contents
