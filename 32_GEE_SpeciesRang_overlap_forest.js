// Earth engine 
//evaluating overlapping forest and soy-suitable forest with species ranges

var MapBiomas = ee.Image ('projects/mapbiomas-workspace/public/collection5/mapbiomas_collection50_integration_v1')
var cerrado = ee.FeatureCollection('users/floriangollnow/IUCN/cerrado_iucn2')//threatened_cerrado')
var amazon = ee.FeatureCollection('users/floriangollnow/IUCN/amazon_iucn2')//threatened_amazon')

var Mapb_2018 = MapBiomas.select('classification_2018');
var Forest_2018 = Mapb_2018.eq(3).or(Mapb_2018.eq(4)).or(Mapb_2018.eq(5));

// GAEZ apt
var GAEZSuit = ee.Image('users/floriangollnow/GAEZ/GAEZ_SoySuitability')
var GAEZSuit_m = GAEZSuit.gte(1).and(GAEZSuit.lte(4))

//var apt = ee.Image ('users/floriangollnow/UFMT/_aptidao_soja_wgs84')//500m
var apt = ee.Image('users/floriangollnow/UFMT/apt_mechanized_crop_60')//60m
var apt_rp = apt.reproject({
    crs: 'EPSG:4326',
    scale: 60
}) 
var apt_m = apt_rp.gt(0)
var suit_mask = GAEZSuit_m.eq(1).and(apt_m.eq(1));
var SuitForest = Forest_2018.multiply(suit_mask);

var forestA = Forest_2018.multiply(ee.Image.pixelArea()).rename('forest');
var suitForestA =SuitForest.multiply(ee.Image.pixelArea()).rename('suit_forest');

var forestTA =  forestA.addBands(suitForestA)
//
var cerrado_reduced_grid= forestTA.reduceRegions({
  reducer: ee.Reducer.sum(),//.unweighted(),
  collection: cerrado,
  scale:30
});

var cerrado_reduced_export = cerrado_reduced_grid.select(['.*'],null,false);

Export.table.toDrive({
  collection: cerrado_reduced_export,
  description: 'Cerrado_species2',
  folder: 'MapBiomas',
  fileFormat: 'CSV'
}); 
//amazon
var amazon_reduced_grid= forestTA.reduceRegions({
  reducer: ee.Reducer.sum(),//.unweighted(),
  collection: amazon,
  scale:30
});

var amazon_reduced_export = amazon_reduced_grid.select(['.*'],null,false);

Export.table.toDrive({
  collection: amazon_reduced_export,
  description: 'Amazon_species2',
  folder: 'MapBiomas',
  fileFormat: 'CSV'
}); 

