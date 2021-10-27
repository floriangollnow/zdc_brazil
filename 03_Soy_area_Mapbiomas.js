//Soy area Mapbiomas

var MapBiomas = ee.Image ('projects/mapbiomas-workspace/public/collection5/mapbiomas_collection50_integration_v1')
print(MapBiomas)
print (MapBiomas.projection().nominalScale())
print (MapBiomas.projection())

var shape = ee.FeatureCollection('users/floriangollnow/AdminBr/BRMUE250GC_WGS84')// Brazil Municipalities

//Agriculture (Crop, pasture, mosaic)
var Mapb_2000 = MapBiomas.select('classification_2000')
var Mapb_a_2000 = Mapb_2000.eq(39)

var Mapb_2001 = MapBiomas.select('classification_2001')
var Mapb_a_2001 = Mapb_2001.eq(39)

var Mapb_2002 = MapBiomas.select('classification_2002')
var Mapb_a_2002 = Mapb_2002.eq(39)

var Mapb_2003 = MapBiomas.select('classification_2003')
var Mapb_a_2003 = Mapb_2003.eq(39)

var Mapb_2004 = MapBiomas.select('classification_2004')
var Mapb_a_2004 = Mapb_2004.eq(39)

var Mapb_2005 = MapBiomas.select('classification_2005')
var Mapb_a_2005 = Mapb_2005.eq(39)

var Mapb_2006 = MapBiomas.select('classification_2006')
var Mapb_a_2006 = Mapb_2006.eq(39)

var Mapb_2007 = MapBiomas.select('classification_2007')
var Mapb_a_2007 = Mapb_2007.eq(39)

var Mapb_2008 = MapBiomas.select('classification_2008')
var Mapb_a_2008 = Mapb_2008.eq(39)

var Mapb_2009 = MapBiomas.select('classification_2009')
var Mapb_a_2009 = Mapb_2009.eq(39)

var Mapb_2010 = MapBiomas.select('classification_2010')
var Mapb_a_2010 = Mapb_2010.eq(39)

var Mapb_2011 = MapBiomas.select('classification_2011')
var Mapb_a_2011 = Mapb_2011.eq(39)

var Mapb_2012 = MapBiomas.select('classification_2012')
var Mapb_a_2012 = Mapb_2012.eq(39)

var Mapb_2013 = MapBiomas.select('classification_2013')
var Mapb_a_2013 = Mapb_2013.eq(39)

var Mapb_2014 = MapBiomas.select('classification_2014')
var Mapb_a_2014 = Mapb_2014.eq(39)

var Mapb_2015 = MapBiomas.select('classification_2015')
var Mapb_a_2015 = Mapb_2015.eq(39)

var Mapb_2016 = MapBiomas.select('classification_2016')
var Mapb_a_2016 = Mapb_2016.eq(39)

var Mapb_2017 = MapBiomas.select('classification_2017')
var Mapb_a_2017 = Mapb_2017.eq(39)

var Mapb_2018 = MapBiomas.select('classification_2018')
var Mapb_a_2018 = Mapb_2018.eq(39)



//calc area
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

var stacked_MapB_a = Mapb_a_2000A.addBands(Mapb_a_2001A).addBands(Mapb_a_2002A).addBands(Mapb_a_2003A).addBands(Mapb_a_2004A).
addBands(Mapb_a_2005A).addBands(Mapb_a_2006A).addBands(Mapb_a_2007A).addBands(Mapb_a_2008A).
addBands(Mapb_a_2009A).addBands(Mapb_a_2010A).addBands(Mapb_a_2011A).addBands(Mapb_a_2012A).
addBands(Mapb_a_2013A).addBands(Mapb_a_2014A).addBands(Mapb_a_2015A).addBands(Mapb_a_2016A).
addBands(Mapb_a_2017A).addBands(Mapb_a_2018A);

//divide municipality shapes

var shape_1 = shape.filter(ee.Filter.lte('CD_GEOCMU','3146830'))  
var shape_2 = shape.filter(ee.Filter.gt('CD_GEOCMU','3146830'));


var Soy_region_1= stacked_MapB_a.reduceRegions({
  reducer: ee.Reducer.sum(),//.unweighted(),
  collection: shape_1,
  scale:30
});
//print (def_region_1)

var Soy_region_2= stacked_MapB_a.reduceRegions({
  reducer: ee.Reducer.sum(),//.unweighted(),
  collection: shape_2,
  scale:30
});
Export.table.toDrive({
  collection: Soy_region_1,
  description: 'Soy_region_1',
  selectors:(["CD_GEOCMU","b2000","b2001","b2002","b2003","b2004","b2005","b2006","b2007","b2008","b2009","b2010","b2011","b2012","b2013","b2014","b2015","b2016","b2017","b2018"]),
  fileFormat: 'CSV'
});

Export.table.toDrive({
  collection: Soy_region_2,
  description: 'Soy_region_2',
  selectors:(["CD_GEOCMU","b2000","b2001","b2002","b2003","b2004","b2005","b2006","b2007","b2008","b2009","b2010","b2011","b2012","b2013","b2014","b2015","b2016","b2017","b2018"]),
  fileFormat: 'CSV'
});


