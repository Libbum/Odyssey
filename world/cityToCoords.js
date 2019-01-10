function getCoords(obj,place) {
  for (var i = 0, len = obj.features.length; i < len; i++) {
      if (obj.features[i].properties.name === place)
      return obj.features[i].geometry.coordinates; // Return as soon as the object is found
  }
  console.log("Warning: '"+place+"' not found.");
  return null; // The object was not found
}
function getCitiesFromTrip(obj,trip) {
  for (var i = 0, len = obj.trips.length; i < len; i++) {
      if (obj.trips[i].name === trip)
      return obj.trips[i].cities; // Return as soon as the object is found
  }
  return null; // The object was not found
}

fs = require('fs'),
tripData = require("./tripcities.json"),
citiesData = require("./cities.json");

route = '{ "type": "FeatureCollection", "features": [ ',


tripData.trips.forEach(function(t) {
  route += '{ "type": "Feature", "properties": { "name": "' + t.name + '" }, "geometry": { "type": "LineString", "coordinates": [',
  cities = getCitiesFromTrip(tripData,t.name),
  cities.forEach(function(p) {
     route += '[' + getCoords(citiesData,p) + '], '
  }),
  route = route.slice(0,-2) + ' ] } }, '
}),
route = route.slice(0,-2) + ' ] }',


fs.writeFile("./trips.json", route, function(err) {
    if(err) {
        return console.log(err);
    }
});
