#!/bin/bash

node cityToCoords.js
topojson -o ../dist/assets/world.json  --id-property su_a3 --properties name,localname,country -- data/countries.json cities.json trips.json
rm trips.json
