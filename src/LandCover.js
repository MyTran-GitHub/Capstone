/// First import table from non-overlapping wildfires

Map.setCenter(-120.6, 36.4, 8);
var globcover = ee.Image("ESA/GLOBCOVER_L4_200901_200912_V2_3");
var geometry = table;
          
// Extract the landcover band
var landcover = globcover.select('landcover');

// Clip the image to the polygon geometry
var landcover_roi = landcover.clip(table);

// Add a map layer of the landcover clipped to the polygon.
Map.addLayer(landcover_roi);

// Print out the frequency of landcover occurrence for the polygon.
var frequency = landcover.reduceRegion({
  reducer:ee.Reducer.frequencyHistogram(),
  geometry:geometry,
  scale:1000
});
var dict = ee.Dictionary(frequency.get('landcover'));
var sum = ee.Array(dict.values()).reduce(ee.Reducer.sum(),[0]).get([0]);
var new_dict = dict.map(function(k,v) {
  return ee.Number(v).divide(sum).multiply(100);
});
print('Frequency', frequency);
print('Dict ', dict);
print('Land Cover (%)',new_dict);
