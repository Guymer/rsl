#!/usr/bin/env bash

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
for ppm in createFlood_*.ppm; do
    # Skip those that do not exist ...
    [[ ! -f $ppm ]] && continue

    # Deduce PNG image ...
    png="${ppm%.ppm}.png"

    # Check if the PNG needs making ...
    if [[ $ppm -nt $png ]]; then
        echo "Making \"$png\" ..."

        # Extract sea level from file name and make title ...
        str="${ppm%.ppm}"
        str="${str#createFlood_}"
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
