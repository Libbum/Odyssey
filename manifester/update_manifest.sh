#!/bin/bash
# Inserts data from odyssey.yaml into src/main.rs where needed.
# Should be run after updating odyssey.yaml with new data, before building the manifest

countries=$(rg -oN '^\s{2}(\w+)' -r '$1' odyssey.yaml | sort | sed 's/^/        /;s/$/,/');

echo "Updating Countries list in manifester"
awk -v countries="$countries" '
    BEGIN       {p=1}
    /^    enum Country {/   {print;print countries;p=0}
    /^    }/     {p=1}
    p' src/main.rs > new_countries.rs

locations=$(rg -oN '^\s+{4}(\w+)' -r '$1' odyssey.yaml | rg -v 'Local|cities|dates|description' | sort | sed 's/^/        /;s/$/,/');
local="        Local,\n"

echo "Updating Locations list in manifester"
awk -v locations="$local$locations" '
    BEGIN       {p=1}
    /^    enum Location {/   {print;print locations;p=0}
    /^    }/     {p=1}
    p' new_countries.rs > new_main.rs

mv new_main.rs src/main.rs
rm -f new_countries.rs
