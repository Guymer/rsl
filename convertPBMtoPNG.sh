#!/usr/bin/env bash

# Loop over PBM images ...
for pbm in *.pbm; do
    # Skip those that do not exist ...
    [[ ! -f $pbm ]] && continue

    # Deduce PNG image ...
    png="${pbm%.pbm}.png"

    # Check if the PNG needs making ...
    if [[ $pbm -nt $png ]]; then
        echo "Making \"$png\" ..."

        convert $pbm $png
        exiftool -overwrite_original -all= $png
        optipng $png
    fi
done
