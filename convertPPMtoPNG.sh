#!/usr/bin/env bash

# Check that non-standard programs are installed. "standard" programs are
# anything that is specified in the POSIX.1-2008 standard (and the IEEE Std
# 1003.1 standard) or that is a BASH builtin command. Therefore, "non-standard"
# programs are anything that does not appear on the following two lists:
#   * https://pubs.opengroup.org/onlinepubs/9699919799/idx/utilities.html
#   * https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html
if ! type convert &> /dev/null; then
    echo "ERROR: \"convert\" is not installed." >&2
    exit 1
fi
if ! type exiftool &> /dev/null; then
    echo "ERROR: \"exiftool\" is not installed." >&2
    exit 1
fi
if ! type optipng &> /dev/null; then
    echo "ERROR: \"optipng\" is not installed." >&2
    exit 1
fi

# Convert dataset map ...
ppm="terr50_gagg_gb.ppm"
png="terr50_gagg_gb.png"
if [[ -f $ppm ]]; then
    if [[ $ppm -nt $png ]]; then
        echo "Making \"$png\" ..."
        convert "$ppm" "$png"
        optipng "$png"
        exiftool -overwrite_original -all= "$png"
        rm "$ppm"
    fi
fi

# Loop over PPM images ...
for ppm in output/*.ppm; do
    # Skip those that do not exist ...
    [[ ! -f $ppm ]] && continue

    # Deduce PNG image ...
    png="${ppm%.ppm}.png"

    # Check if the PNG needs making ...
    if [[ $ppm -nt $png ]]; then
        echo "Making \"$png\" ..."

        # Extract sea level from file name and make title ...
        str="${ppm%.ppm}"
        str="${str#output/}"
        str="${str:0:1},${str:1:5} sea level rise"

        # Make PNG ...
        convert "$ppm" -gravity north -stroke none -fill white -font Courier -pointsize 72 -annotate 0 "$str" "$png"
        optipng "$png"
        exiftool -overwrite_original -all= "$png"
    fi

    # Check if the PPM needs removing ...
    if [[ -f $png ]]; then
        echo "Removing \"$ppm\" ..."

        # Remove PPM ...
        rm "$ppm"
    fi
done
