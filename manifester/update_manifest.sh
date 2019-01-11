#!/bin/bash
# Inserts data from odyssey.yaml into src/main.rs where needed.
# Should be run after updating odyssey.yaml with new data, before building the manifest

countries=$(rg -oN '^\w+' odyssey.yaml | sed 's/^/        /;s/$/,/');

echo "Updating Countries list in manifester"
awk -v countries="$countries" '
    BEGIN       {p=1}
    /^    enum Country {/   {print;print countries;p=0}
    /^    }/     {p=1}
    p' src/main.rs > new_countries.rs

locations=$(rg -oN '^\s+(\w+)' -r '$1' odyssey.yaml | rg -v 'local' | sed 's/^/        /;s/$/,/');

echo "Updating Locations list in manifester"
awk -v locations="$locations" '
    BEGIN       {p=1}
    /^    enum Locations {/   {print;print locations;p=0}
    /^    }/     {p=1}
    p' new_countries.rs > new_main.rs

mv new_main.rs src/main.rs
rm -f new_countries.rs
