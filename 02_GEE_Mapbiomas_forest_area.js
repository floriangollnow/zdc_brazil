// derive forest area from Mapbomas transition maps from GEE
// forest was defined as forest in 2000, excluding secondary regrowth there after

var trans_a = ee.Image('projects/mapbiomas-workspace/public/collection5/mapbiomas_collection50_transitions_v1')

print(MapBiomas)
print(trans_a)
print (MapBiomas.projection().nominalScale())
print (MapBiomas.projection())
//var shape = ee.FeatureCollection('users/floriangollnow/AdminBr/BRMUE250GC_WGS84_biome_intesect')
var shape = ee.FeatureCollection('users/floriangollnow/AdminBr/BRMUE250GC_WGS84')// Brazil municipalities

// Forest <- primary 2000
// transition maps include forest to forest transition. no need for lucc class

var Mapb_2000 = trans_a.select('transition_2000_2001')
var Mapb_a_2000 = Mapb_2000.gt(299).and(Mapb_2000.lt(600))
//Map.addLayer(Mapb_a_2000,{}, "forest2000")

var Mapb_2001 = trans_a.select('transition_2001_2002')
var Mapb_a_2001 =  Mapb_a_2000.eq(1).and(Mapb_2001.gt(299).and(Mapb_2001.lt(600)))
//Map.addLayer(Mapb_a_2001,{}, "forest2001")

var Mapb_2002 = trans_a.select('transition_2002_2003')
var Mapb_a_2002 =  Mapb_a_2001.eq(1).and(Mapb_2002.gt(299).and(Mapb_2002.lt(600)))
//Map.addLayer(Mapb_a_2002,{}, "forest2002")

var Mapb_2003 = trans_a.select('transition_2003_2004')
var Mapb_a_2003 =  Mapb_a_2002.eq(1).and(Mapb_2003.gt(299).and(Mapb_2003.lt(600)))
//Map.addLayer(Mapb_a_2003,{}, "forest2003")

var Mapb_2004 = trans_a.select('transition_2004_2005')
var Mapb_a_2004 =  Mapb_a_2003.eq(1).and(Mapb_2004.gt(299).and(Mapb_2004.lt(600)))
//Map.addLayer(Mapb_a_2004,{}, "forest2004")

var Mapb_2005 = trans_a.select('transition_2005_2006')
var Mapb_a_2005 =  Mapb_a_2004.eq(1).and(Mapb_2005.gt(299).and(Mapb_2005.lt(600)))
//Map.addLayer(Mapb_a_2005,{}, "forest2005")

var Mapb_2006 = trans_a.select('transition_2006_2007')
var Mapb_a_2006 =  Mapb_a_2005.eq(1).and(Mapb_2006.gt(299).and(Mapb_2006.lt(600)))
//Map.addLayer(Mapb_a_2006,{}, "forest2006")

var Mapb_2007 = trans_a.select('transition_2007_2008')
var Mapb_a_2007 =  Mapb_a_2006.eq(1).and(Mapb_2007.gt(299).and(Mapb_2007.lt(600)))
//Map.addLayer(Mapb_a_2007,{}, "forest2007")

var Mapb_2008 = trans_a.select('transition_2008_2009')
var Mapb_a_2008 =  Mapb_a_2007.eq(1).and(Mapb_2008.gt(299).and(Mapb_2008.lt(600)))
//Map.addLayer(Mapb_a_2008,{}, "forest2008")

var Mapb_2009 = trans_a.select('transition_2009_2010')
var Mapb_a_2009 =  Mapb_a_2008.eq(1).and(Mapb_2009.gt(299).and(Mapb_2009.lt(600)))
//Map.addLayer(Mapb_a_2009,{}, "forest2009")


var Mapb_2010 = trans_a.select('transition_2010_2011')
var Mapb_a_2010 =  Mapb_a_2009.eq(1).and(Mapb_2010.gt(299).and(Mapb_2010.lt(600)))
//Map.addLayer(Mapb_a_2010,{}, "forest2010")

var Mapb_2011 = trans_a.select('transition_2011_2012')
var Mapb_a_2011 =  Mapb_a_2010.eq(1).and(Mapb_2011.gt(299).and(Mapb_2011.lt(600)))
//Map.addLayer(Mapb_a_2011,{}, "forest2011")

var Mapb_2012 = trans_a.select('transition_2012_2013')
var Mapb_a_2012 =  Mapb_a_2011.eq(1).and(Mapb_2012.gt(299).and(Mapb_2012.lt(600)))
//Map.addLayer(Mapb_a_2012,{}, "forest2012")

var Mapb_2013 = trans_a.select('transition_2013_2014')
var Mapb_a_2013 =  Mapb_a_2012.eq(1).and(Mapb_2013.gt(299).and(Mapb_2013.lt(600)))
//Map.addLayer(Mapb_a_2013,{}, "forest2013")

var Mapb_2014 = trans_a.select('transition_2014_2015')
var Mapb_a_2014 =  Mapb_a_2013.eq(1).and(Mapb_2014.gt(299).and(Mapb_2014.lt(600)))
//Map.addLayer(Mapb_a_2014,{}, "forest2014")

var Mapb_2015 = trans_a.select('transition_2015_2016')
var Mapb_a_2015 =  Mapb_a_2014.eq(1).and(Mapb_2015.gt(299).and(Mapb_2015.lt(600)))
//Map.addLayer(Mapb_a_2015,{}, "forest2015")

var Mapb_2016 = trans_a.select('transition_2016_2017')
var Mapb_a_2016 =  Mapb_a_2015.eq(1).and(Mapb_2016.gt(299).and(Mapb_2016.lt(600)))
//Map.addLayer(Mapb_a_2016,{}, "forest2016")

var Mapb_2017 = trans_a.select('transition_2017_2018')
var Mapb_a_2017 =  Mapb_a_2016.eq(1).and(Mapb_2017.gt(299).and(Mapb_2017.lt(600)))
//Map.addLayer(Mapb_a_2017,{}, "forest2017")

var Mapb_2018 = trans_a.select('transition_2018_2019')
var Mapb_a_2018 =  Mapb_a_2017.eq(1).and(Mapb_2018.gt(299).and(Mapb_2018.lt(600)))
//Map.addLayer(Mapb_a_2018,{}, "forest2018")

var Mapb_2019 = trans_a.select('transition_2018_2019')
var Mapb_a_2019 =  Mapb_a_2018.eq(1).and
(Mapb_2019.eq(303).or(Mapb_2019.eq(304)).or(Mapb_2019.eq(305)).or
(Mapb_2019.eq(403)).or(Mapb_2019.eq(404)).or(Mapb_2019.eq(405)).or
(Mapb_2019.eq(503)).or(Mapb_2019.eq(504)).or(Mapb_2019.eq(505))) // all that stays forest
Map.addLayer(Mapb_a_2019,{}, "forest2019")


//calc area
//var Mapb_a_1999A = Mapb_a_1999.multiply(ee.Image.pixelArea()).rename(['b1999'])

var Mapb_a_2000A = Mapb_a_2000.multiply(ee.Image.pixelArea()).rename(['b2000'])
var Mapb_a_2001A = Mapb_a_2001.multiply(ee.Image.pixelArea()).rename(['b2001'])
var Mapb_a_2002A = Mapb_a_2002.multiply(ee.Image.pixelArea()).rename(['b2002'])
var Mapb_a_2003A = Mapb_a_2003.multiply(ee.Image.pixelArea()).rename(['b2003'])

var Mapb_a_2004A = Mapb_a_2004.multiply(ee.Image.pixelArea()).rename(['b2004'])
var Mapb_a_2005A = Mapb_a_2005.multiply(ee.Image.pixelArea()).rename(['b2005'])
var Mapb_a_2006A = Mapb_a_2006.multiply(ee.Image.pixelArea()).rename(['b2006'])

var Mapb_a_2007A = Mapb_a_2007.multiply(ee.Image.pixelArea()).rename(['b2007'])
var Mapb_a_2008A = Mapb_a_2008.multiply(ee.Image.pixelArea()).rename(['b2008'])
var Mapb_a_2009A = Mapb_a_2009.multiply(ee.Image.pixelArea()).rename(['b2009'])
var Mapb_a_2010A = Mapb_a_2010.multiply(ee.Image.pixelArea()).rename(['b2010'])
var Mapb_a_2011A = Mapb_a_2011.multiply(ee.Image.pixelArea()).rename(['b2011'])
var Mapb_a_2012A = Mapb_a_2012.multiply(ee.Image.pixelArea()).rename(['b2012'])
var Mapb_a_2013A = Mapb_a_2013.multiply(ee.Image.pixelArea()).rename(['b2013'])
var Mapb_a_2014A = Mapb_a_2014.multiply(ee.Image.pixelArea()).rename(['b2014'])
var Mapb_a_2015A = Mapb_a_2015.multiply(ee.Image.pixelArea()).rename(['b2015'])
var Mapb_a_2016A = Mapb_a_2016.multiply(ee.Image.pixelArea()).rename(['b2016'])
var Mapb_a_2017A = Mapb_a_2017.multiply(ee.Image.pixelArea()).rename(['b2017'])
var Mapb_a_2018A = Mapb_a_2018.multiply(ee.Image.pixelArea()).rename(['b2018'])
var Mapb_a_2019A = Mapb_a_2019.multiply(ee.Image.pixelArea()).rename(['b2019'])

var stacked_MapB_a = Mapb_a_2000A.addBands(Mapb_a_2001A).
addBands(Mapb_a_2002A).addBands(Mapb_a_2003A).addBands(Mapb_a_2004A).
addBands(Mapb_a_2005A).addBands(Mapb_a_2006A).addBands(Mapb_a_2007A).addBands(Mapb_a_2008A).
addBands(Mapb_a_2009A).addBands(Mapb_a_2010A).addBands(Mapb_a_2011A).addBands(Mapb_a_2012A).
addBands(Mapb_a_2013A).addBands(Mapb_a_2014A).addBands(Mapb_a_2015A).addBands(Mapb_a_2016A).
addBands(Mapb_a_2017A).addBands(Mapb_a_2018A).addBands(Mapb_a_2019A);
print (stacked_MapB_a)
//divide municipality shapes

var shape_1 = shape.filter(ee.Filter.lte('CD_GEOCMU','3146830'))  
var shape_2 = shape.filter(ee.Filter.gt('CD_GEOCMU','3146830'));


var forest_region_1= stacked_MapB_a.reduceRegions({
  reducer: ee.Reducer.sum(),//.unweighted(),
  collection: shape_1,
  scale:30
});
//print (def_region_1)

var forest_region_2= stacked_MapB_a.reduceRegions({
  reducer: ee.Reducer.sum(),//.unweighted(),
  collection: shape_2,
  scale:30
});
Export.table.toDrive({
  collection: forest_region_1,
  description: 'forest_base2000_transitions_region_1',
  selectors:(["CD_GEOCMU","b2000","b2001","b2002","b2003","b2004","b2005","b2006","b2007","b2008","b2009","b2010","b2011","b2012","b2013","b2014","b2015","b2016","b2017","b2018","b2019"]),
  fileFormat: 'CSV',
  folder: 'MapBiomas'
});

Export.table.toDrive({
  collection: forest_region_2,
  description: 'forest_base2000_transitions_region_2',
  selectors:(["CD_GEOCMU","b2000","b2001","b2002","b2003","b2004","b2005","b2006","b2007","b2008","b2009","b2010","b2011","b2012","b2013","b2014","b2015","b2016","b2017","b2018", "b2019"]),
  fileFormat: 'CSV',
  folder: 'MapBiomas'
});


