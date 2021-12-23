// assessing soy suitable forest by combining forest cover (Mapbiomas), GAEZ soy suitability and aptitude (Soares-Filho et al 2014 dataset) for mechanized crop production.

var shape = ee.FeatureCollection('users/floriangollnow/AdminBr/BRMUE250GC_WGS84')
var grid = ee.FeatureCollection('users/floriangollnow/Fede/ERA5_grid_buf')

//print(shape)
var GAEZSuit = ee.Image('users/floriangollnow/GAEZ/GAEZ_SoySuitability')
var GAEZSuit_m = GAEZSuit.gte(1).and(GAEZSuit.lte(4))

var apt = ee.Image('users/floriangollnow/UFMT/apt_mechanized_crop_60')//60m
var apt_rp = apt.reproject({
  crs: 'EPSG:4326',
  scale: 60
}) 

var apt_m = apt_rp.gt(0)

var trans_a = ee.Image('projects/mapbiomas-workspace/public/collection5/mapbiomas_collection50_transitions_v1')

var GAEZ_apt =  GAEZSuit_m.eq(1).and(apt_m.eq(1))

var Mapb_2000 = trans_a.select('transition_2000_2001')
var fo_2000 = Mapb_2000.gt(299).and(Mapb_2000.lt(600))
//Map.addLayer(fo_2000,{}, "forest2000")

var Mapb_2001 = trans_a.select('transition_2001_2002')
var fo_2001 =  fo_2000.eq(1).and(Mapb_2001.gt(299).and(Mapb_2001.lt(600)))
//Map.addLayer(fo_2001,{}, "forest2001")

var Mapb_2002 = trans_a.select('transition_2002_2003')
var fo_2002 =  fo_2001.eq(1).and(Mapb_2002.gt(299).and(Mapb_2002.lt(600)))
//Map.addLayer(fo_2002,{}, "forest2002")

var Mapb_2003 = trans_a.select('transition_2003_2004')
var fo_2003 =  fo_2002.eq(1).and(Mapb_2003.gt(299).and(Mapb_2003.lt(600)))
//Map.addLayer(fo_2003,{}, "forest2003")

var Mapb_2004 = trans_a.select('transition_2004_2005')
var fo_2004 =  fo_2003.eq(1).and(Mapb_2004.gt(299).and(Mapb_2004.lt(600)))
//Map.addLayer(fo_2004,{}, "forest2004")

var Mapb_2005 = trans_a.select('transition_2005_2006')
var fo_2005 =  fo_2004.eq(1).and(Mapb_2005.gt(299).and(Mapb_2005.lt(600)))
//Map.addLayer(fo_2005,{}, "forest2005")

var Mapb_2006 = trans_a.select('transition_2006_2007')
var fo_2006 =  fo_2005.eq(1).and(Mapb_2006.gt(299).and(Mapb_2006.lt(600)))
//Map.addLayer(fo_2006,{}, "forest2006")

var Mapb_2007 = trans_a.select('transition_2007_2008')
var fo_2007 =  fo_2006.eq(1).and(Mapb_2007.gt(299).and(Mapb_2007.lt(600)))
//Map.addLayer(fo_2007,{}, "forest2007")

var Mapb_2008 = trans_a.select('transition_2008_2009')
var fo_2008 =  fo_2007.eq(1).and(Mapb_2008.gt(299).and(Mapb_2008.lt(600)))
//Map.addLayer(fo_2008,{}, "forest2008")

var Mapb_2009 = trans_a.select('transition_2009_2010')
var fo_2009 =  fo_2008.eq(1).and(Mapb_2009.gt(299).and(Mapb_2009.lt(600)))
//Map.addLayer(fo_2009,{}, "forest2009")

var Mapb_2010 = trans_a.select('transition_2010_2011')
var fo_2010 =  fo_2009.eq(1).and(Mapb_2010.gt(299).and(Mapb_2010.lt(600)))
//Map.addLayer(fo_2010,{}, "forest2010")

var Mapb_2011 = trans_a.select('transition_2011_2012')
var fo_2011 =  fo_2010.eq(1).and(Mapb_2011.gt(299).and(Mapb_2011.lt(600)))
//Map.addLayer(fo_2011,{}, "forest2011")

var Mapb_2012 = trans_a.select('transition_2012_2013')
var fo_2012 =  fo_2011.eq(1).and(Mapb_2012.gt(299).and(Mapb_2012.lt(600)))
//Map.addLayer(fo_2012,{}, "forest2012")

var Mapb_2013 = trans_a.select('transition_2013_2014')
var fo_2013 =  fo_2012.eq(1).and(Mapb_2013.gt(299).and(Mapb_2013.lt(600)))
//Map.addLayer(fo_2013,{}, "forest2013")

var Mapb_2014 = trans_a.select('transition_2014_2015')
var fo_2014 =  fo_2013.eq(1).and(Mapb_2014.gt(299).and(Mapb_2014.lt(600)))
//Map.addLayer(fo_2014,{}, "forest2014")

var Mapb_2015 = trans_a.select('transition_2015_2016')
var fo_2015 =  fo_2014.eq(1).and(Mapb_2015.gt(299).and(Mapb_2015.lt(600)))
//Map.addLayer(fo_2015,{}, "forest2015")

var Mapb_2016 = trans_a.select('transition_2016_2017')
var fo_2016 =  fo_2015.eq(1).and(Mapb_2016.gt(299).and(Mapb_2016.lt(600)))
//Map.addLayer(fo_2016,{}, "forest2016")

var Mapb_2017 = trans_a.select('transition_2017_2018')
var fo_2017 =  fo_2016.eq(1).and(Mapb_2017.gt(299).and(Mapb_2017.lt(600)))
//Map.addLayer(fo_2017,{}, "forest2017")

var Mapb_2018 = trans_a.select('transition_2018_2019')
var fo_2018 =  fo_2017.eq(1).and(Mapb_2018.gt(299).and(Mapb_2018.lt(600)))
//Map.addLayer(fo_2018,{}, "forest2018")

var Mapb_2019 = trans_a.select('transition_2018_2019')
var fo_2019 =  fo_2018.eq(1).and
(Mapb_2019.eq(303).or(Mapb_2019.eq(304)).or(Mapb_2019.eq(305)).or
  (Mapb_2019.eq(403)).or(Mapb_2019.eq(404)).or(Mapb_2019.eq(405)).or
  (Mapb_2019.eq(503)).or(Mapb_2019.eq(504)).or(Mapb_2019.eq(505))) // all that stays forest
Map.addLayer(fo_2019,{}, "forest2019")

  /// multiply with mask

var fo_2000A = fo_2000.multiply(ee.Image.pixelArea()).multiply(GAEZ__apt).rename(['b2000'])
var fo_2000ab = fo_2000.multiply(ee.Image.pixelArea()).rename(['b2000'])

Map.addLayer(GAEZ_apt,{},"suit");
Map.addLayer(fo_2000A,{},"forest_suit");
Map.addLayer(fo_2000ab,{},"forest");

var fo_2001A = fo_2001.multiply(ee.Image.pixelArea()).multiply(GAEZ_slope_apt).rename(['b2001'])
var fo_2002A = fo_2002.multiply(ee.Image.pixelArea()).multiply(GAEZ_slope_apt).rename(['b2002'])
var fo_2003A = fo_2003.multiply(ee.Image.pixelArea()).multiply(GAEZ_slope_apt).rename(['b2003'])
var fo_2004A = fo_2004.multiply(ee.Image.pixelArea()).multiply(GAEZ_slope_apt).rename(['b2004'])
var fo_2005A = fo_2005.multiply(ee.Image.pixelArea()).multiply(GAEZ_slope_apt).rename(['b2005'])
var fo_2006A = fo_2006.multiply(ee.Image.pixelArea()).multiply(GAEZ_slope_apt).rename(['b2006'])
var fo_2007A = fo_2007.multiply(ee.Image.pixelArea()).multiply(GAEZ_slope_apt).rename(['b2007'])
var fo_2008A = fo_2008.multiply(ee.Image.pixelArea()).multiply(GAEZ_slope_apt).rename(['b2008'])
var fo_2009A = fo_2009.multiply(ee.Image.pixelArea()).multiply(GAEZ_slope_apt).rename(['b2009'])
var fo_2010A = fo_2010.multiply(ee.Image.pixelArea()).multiply(GAEZ_slope_apt).rename(['b2010'])
var fo_2011A = fo_2011.multiply(ee.Image.pixelArea()).multiply(GAEZ_slope_apt).rename(['b2011'])
var fo_2012A = fo_2012.multiply(ee.Image.pixelArea()).multiply(GAEZ_slope_apt).rename(['b2012'])
var fo_2013A = fo_2013.multiply(ee.Image.pixelArea()).multiply(GAEZ_slope_apt).rename(['b2013'])
var fo_2014A = fo_2014.multiply(ee.Image.pixelArea()).multiply(GAEZ_slope_apt).rename(['b2014'])
var fo_2015A = fo_2015.multiply(ee.Image.pixelArea()).multiply(GAEZ_slope_apt).rename(['b2015'])
var fo_2016A = fo_2016.multiply(ee.Image.pixelArea()).multiply(GAEZ_slope_apt).rename(['b2016'])
var fo_2017A = fo_2017.multiply(ee.Image.pixelArea()).multiply(GAEZ_slope_apt).rename(['b2017'])
var fo_2018A = fo_2018.multiply(ee.Image.pixelArea()).multiply(GAEZ_slope_apt).rename(['b2018'])
var fo_2019A = fo_2019.multiply(ee.Image.pixelArea()).multiply(GAEZ_slope_apt).rename(['b2019'])


///reduce
var stacked_fo = fo_2000A.addBands(fo_2001A).addBands(fo_2002A).addBands(fo_2003A).addBands(fo_2004A).
addBands(fo_2005A).addBands(fo_2006A).addBands(fo_2007A).addBands(fo_2008A).
addBands(fo_2009A).addBands(fo_2010A).addBands(fo_2011A).addBands(fo_2012A).
addBands(fo_2013A).addBands(fo_2014A).
addBands(fo_2015A).addBands(fo_2016A).addBands(fo_2017A).addBands(fo_2018A).addBands(fo_2019A);




var suit_region= stacked_fo.reduceRegions({
  reducer: ee.Reducer.sum(),//.unweighted(),
  collection: shape,
  scale:30
});

var regionOut = suit_region.select(['.*'],null,false);

Export.table.toDrive({
  collection: regionOut,
  description: 'GAEZ_apt_forest_area_muni',
  fileFormat: 'CSV',
  folder:'Mapbiomas'
});

var suit_grid= stacked_fo.reduceRegions({
  reducer: ee.Reducer.sum(),//.unweighted(),
  collection: grid,
  scale:30
});

var gridOut = suit_grid.select(['.*'],null,false);

Export.table.toDrive({
  collection: gridOut,
  description: 'GAEZ_apt_forest_area_grid',
  fileFormat: 'CSV',
  folder:'Mapbiomas'
});

