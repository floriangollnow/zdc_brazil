// deriving 5 year deforestation for soy 
// deforestation was derived from Mapbiomas transition Mapbiomas
// soy was derived from Mapbiomas
// deforestation for soy was defined as any forest conversion that had soybeans planted within 5 years post conversion

var MapBiomas = ee.Image ('projects/mapbiomas-workspace/public/collection5/mapbiomas_collection50_integration_v1') // used for soy areas
var trans_a = ee.Image('projects/mapbiomas-workspace/public/collection5/mapbiomas_collection50_transitions_v1') // used for deforestation

print(trans_a)
print (trans_a.projection().nominalScale())
print (trans_a.projection())
//load municipalities of Brazil from private assets
var shape = ee.FeatureCollection('users/floriangollnow/AdminBr/BRMUE250GC_WGS84')

//identify soy in Mapbiomas 
//identify forest (use 2000 as forest base)

// for loop in gee 
//var Mapb_1999 = MapBiomas.select('classification_1999')
//var Mapb_f1999 = trans_a.select('transition_1999_2000')
//var Mapb_fa_1999 =  Mapb_f1999.gt(299).and(Mapb_f1999.lt(600))// trasition class >200 < 600
//Map.addLayer(Mapb_fa_1999,{}, "forest199")

//var Mapb_2000 = MapBiomas.select('classification_2000')
//var Mapb_c_2000 = Mapb_2000.eq(39)
var Mapb_f2000 = trans_a.select('transition_2000_2001')
var Mapb_fa_2000 = Mapb_f2000.gt(299).and(Mapb_f2000.lt(600))//identifies forest in 2000
Map.addLayer(Mapb_fa_2000,{}, "forest2000")

var Mapb_c_2001 = Mapb_f2000.eq(339).or(Mapb_f2000.eq(439)).or(Mapb_f2000.eq(539)).or(Mapb_f2000.eq(939)).or(Mapb_f2000.eq(1139)).
or(Mapb_f2000.eq(239)).or(Mapb_f2000.eq(3239)).or(Mapb_f2000.eq(2939)).or(Mapb_f2000.eq(1339)).or(Mapb_f2000.eq(1539)).or(Mapb_f2000.eq(2039)).
or(Mapb_f2000.eq(4139)).or(Mapb_f2000.eq(4639)).or(Mapb_f2000.eq(2139)).or(Mapb_f2000.eq(2339)).or(Mapb_f2000.eq(2439)).or(Mapb_f2000.eq(3039)).
or(Mapb_f2000.eq(2539)).or(Mapb_f2000.eq(3339)).or(Mapb_f2000.eq(2739)).or(Mapb_f2000.eq(39))
Map.addLayer(Mapb_c_2001,{}, "conv4Soy")

var Mapb_f2001 = trans_a.select('transition_2001_2002')
var Mapb_fa_2001 =  Mapb_fa_2000.eq(1).and(Mapb_f2001.gt(299).and(Mapb_f2001.lt(600)))//identifies forest in 2001 that was also forest in 2000

var Mapb_c_2002 = Mapb_f2001.eq(339).or(Mapb_f2001.eq(439)).or(Mapb_f2001.eq(539)).or(Mapb_f2001.eq(939)).or(Mapb_f2001.eq(1139)).
or(Mapb_f2001.eq(239)).or(Mapb_f2001.eq(3239)).or(Mapb_f2001.eq(2939)).or(Mapb_f2001.eq(1339)).or(Mapb_f2001.eq(1539)).
or(Mapb_f2001.eq(2039)).or(Mapb_f2001.eq(4139)).or(Mapb_f2001.eq(4639)).or(Mapb_f2001.eq(2139)).or(Mapb_f2001.eq(2339)).or(Mapb_f2001.eq(2439)).
or(Mapb_f2001.eq(3039)).or(Mapb_f2001.eq(2539)).or(Mapb_f2001.eq(3339)).or(Mapb_f2001.eq(2739)).or(Mapb_f2001.eq(39))

var Mapb_f2002 = trans_a.select('transition_2002_2003')
var Mapb_fa_2002 =  Mapb_fa_2001.eq(1).and(Mapb_f2002.gt(299).and(Mapb_f2002.lt(600)))

var Mapb_c_2003 = Mapb_f2002.eq(339).or(Mapb_f2002.eq(439)).or(Mapb_f2002.eq(539)).or(Mapb_f2002.eq(939)).or(Mapb_f2002.eq(1139)).
or(Mapb_f2002.eq(239)).or(Mapb_f2002.eq(3239)).or(Mapb_f2002.eq(2939)).or(Mapb_f2002.eq(1339)).or(Mapb_f2002.eq(1539)).
or(Mapb_f2002.eq(2039)).or(Mapb_f2002.eq(4139)).or(Mapb_f2002.eq(4639)).or(Mapb_f2002.eq(2139)).or(Mapb_f2002.eq(2339)).or(Mapb_f2002.eq(2439)).
or(Mapb_f2002.eq(3039)).or(Mapb_f2002.eq(2539)).or(Mapb_f2002.eq(3339)).or(Mapb_f2002.eq(2739)).or(Mapb_f2002.eq(39))

var Mapb_f2003 = trans_a.select('transition_2003_2004')
var Mapb_fa_2003 =  Mapb_fa_2002.eq(1).and(Mapb_f2003.gt(299).and(Mapb_f2003.lt(600)))


var Mapb_c_2004 = Mapb_f2003.eq(339).or(Mapb_f2003.eq(439)).or(Mapb_f2003.eq(539)).or(Mapb_f2003.eq(939)).or(Mapb_f2003.eq(1139)).
or(Mapb_f2003.eq(239)).or(Mapb_f2003.eq(3239)).or(Mapb_f2003.eq(2939)).or(Mapb_f2003.eq(1339)).or(Mapb_f2003.eq(1539)).
or(Mapb_f2003.eq(2039)).or(Mapb_f2003.eq(4139)).or(Mapb_f2003.eq(4639)).or(Mapb_f2003.eq(2139)).or(Mapb_f2003.eq(2339)).or(Mapb_f2003.eq(2439)).
or(Mapb_f2003.eq(3039)).or(Mapb_f2003.eq(2539)).or(Mapb_f2003.eq(3339)).or(Mapb_f2003.eq(2739)).or(Mapb_f2003.eq(39))

var Mapb_f2004 = trans_a.select('transition_2004_2005')
var Mapb_fa_2004 =  Mapb_fa_2003.eq(1).and(Mapb_f2004.gt(299).and(Mapb_f2004.lt(600)))


var Mapb_c_2005 = Mapb_f2004.eq(339).or(Mapb_f2004.eq(439)).or(Mapb_f2004.eq(539)).or(Mapb_f2004.eq(939)).or(Mapb_f2004.eq(1139)).
or(Mapb_f2004.eq(239)).or(Mapb_f2004.eq(3239)).or(Mapb_f2004.eq(2939)).or(Mapb_f2004.eq(1339)).or(Mapb_f2004.eq(1539)).
or(Mapb_f2004.eq(2039)).or(Mapb_f2004.eq(4139)).or(Mapb_f2004.eq(4639)).or(Mapb_f2004.eq(2139)).or(Mapb_f2004.eq(2339)).or(Mapb_f2004.eq(2439)).
or(Mapb_f2004.eq(3039)).or(Mapb_f2004.eq(2539)).or(Mapb_f2004.eq(3339)).or(Mapb_f2004.eq(2739)).or(Mapb_f2004.eq(39))

var Mapb_f2005 = trans_a.select('transition_2005_2006')
var Mapb_fa_2005 =  Mapb_fa_2004.eq(1).and(Mapb_f2005.gt(299).and(Mapb_f2005.lt(600)))

var Mapb_c_2006 = Mapb_f2005.eq(339).or(Mapb_f2005.eq(439)).or(Mapb_f2005.eq(539)).or(Mapb_f2005.eq(939)).or(Mapb_f2005.eq(1139)).
or(Mapb_f2005.eq(239)).or(Mapb_f2005.eq(3239)).or(Mapb_f2005.eq(2939)).or(Mapb_f2005.eq(1339)).or(Mapb_f2005.eq(1539)).
or(Mapb_f2005.eq(2039)).or(Mapb_f2005.eq(4139)).or(Mapb_f2005.eq(4639)).or(Mapb_f2005.eq(2139)).or(Mapb_f2005.eq(2339)).or(Mapb_f2005.eq(2439)).
or(Mapb_f2005.eq(3039)).or(Mapb_f2005.eq(2539)).or(Mapb_f2005.eq(3339)).or(Mapb_f2005.eq(2739)).or(Mapb_f2005.eq(39))

var Mapb_f2006 = trans_a.select('transition_2006_2007')
var Mapb_fa_2006 =  Mapb_fa_2005.eq(1).and(Mapb_f2006.gt(299).and(Mapb_f2006.lt(600)))

var Mapb_c_2007 = Mapb_f2006.eq(339).or(Mapb_f2006.eq(439)).or(Mapb_f2006.eq(539)).or(Mapb_f2006.eq(939)).or(Mapb_f2006.eq(1139)).
or(Mapb_f2006.eq(239)).or(Mapb_f2006.eq(3239)).or(Mapb_f2006.eq(2939)).or(Mapb_f2006.eq(1339)).or(Mapb_f2006.eq(1539)).
or(Mapb_f2006.eq(2039)).or(Mapb_f2006.eq(4139)).or(Mapb_f2006.eq(4639)).or(Mapb_f2006.eq(2139)).or(Mapb_f2006.eq(2339)).or(Mapb_f2006.eq(2439)).
or(Mapb_f2006.eq(3039)).or(Mapb_f2006.eq(2539)).or(Mapb_f2006.eq(3339)).or(Mapb_f2006.eq(2739)).or(Mapb_f2006.eq(39))

var Mapb_f2007 = trans_a.select('transition_2007_2008')
var Mapb_fa_2007 =  Mapb_fa_2006.eq(1).and(Mapb_f2007.gt(299).and(Mapb_f2007.lt(600)))

var Mapb_c_2008 = Mapb_f2007.eq(339).or(Mapb_f2007.eq(439)).or(Mapb_f2007.eq(539)).or(Mapb_f2007.eq(939)).or(Mapb_f2007.eq(1139)).
or(Mapb_f2007.eq(239)).or(Mapb_f2007.eq(3239)).or(Mapb_f2007.eq(2939)).or(Mapb_f2007.eq(1339)).or(Mapb_f2007.eq(1539)).
or(Mapb_f2007.eq(2039)).or(Mapb_f2007.eq(4139)).or(Mapb_f2007.eq(4639)).or(Mapb_f2007.eq(2139)).or(Mapb_f2007.eq(2339)).or(Mapb_f2007.eq(2439)).
or(Mapb_f2007.eq(3039)).or(Mapb_f2007.eq(2539)).or(Mapb_f2007.eq(3339)).or(Mapb_f2007.eq(2739)).or(Mapb_f2007.eq(39))

var Mapb_f2008 = trans_a.select('transition_2008_2009')
var Mapb_fa_2008 =  Mapb_fa_2007.eq(1).and(Mapb_f2008.gt(299).and(Mapb_f2008.lt(600)))


var Mapb_c_2009 = Mapb_f2008.eq(339).or(Mapb_f2008.eq(439)).or(Mapb_f2008.eq(539)).or(Mapb_f2008.eq(939)).or(Mapb_f2008.eq(1139)).
or(Mapb_f2008.eq(239)).or(Mapb_f2008.eq(3239)).or(Mapb_f2008.eq(2939)).or(Mapb_f2008.eq(1339)).or(Mapb_f2008.eq(1539)).
or(Mapb_f2008.eq(2039)).or(Mapb_f2008.eq(4139)).or(Mapb_f2008.eq(4639)).or(Mapb_f2008.eq(2139)).or(Mapb_f2008.eq(2339)).or(Mapb_f2008.eq(2439)).
or(Mapb_f2008.eq(3039)).or(Mapb_f2008.eq(2539)).or(Mapb_f2008.eq(3339)).or(Mapb_f2008.eq(2739)).or(Mapb_f2008.eq(39))

var Mapb_f2009 = trans_a.select('transition_2009_2010')
var Mapb_fa_2009 =  Mapb_fa_2008.eq(1).and(Mapb_f2009.gt(299).and(Mapb_f2009.lt(600)))


var Mapb_c_2010 = Mapb_f2009.eq(339).or(Mapb_f2009.eq(439)).or(Mapb_f2009.eq(539)).or(Mapb_f2009.eq(939)).or(Mapb_f2009.eq(1139)).
or(Mapb_f2009.eq(239)).or(Mapb_f2009.eq(3239)).or(Mapb_f2009.eq(2939)).or(Mapb_f2009.eq(1339)).or(Mapb_f2009.eq(1539)).
or(Mapb_f2009.eq(2039)).or(Mapb_f2009.eq(4139)).or(Mapb_f2009.eq(4639)).or(Mapb_f2009.eq(2139)).or(Mapb_f2009.eq(2339)).or(Mapb_f2009.eq(2439)).
or(Mapb_f2009.eq(3039)).or(Mapb_f2009.eq(2539)).or(Mapb_f2009.eq(3339)).or(Mapb_f2009.eq(2739)).or(Mapb_f2009.eq(39))

var Mapb_f2010 = trans_a.select('transition_2010_2011')
var Mapb_fa_2010 =  Mapb_fa_2009.eq(1).and(Mapb_f2010.gt(299).and(Mapb_f2010.lt(600)))


var Mapb_c_2011 = Mapb_f2010.eq(339).or(Mapb_f2010.eq(439)).or(Mapb_f2010.eq(539)).or(Mapb_f2010.eq(939)).or(Mapb_f2010.eq(1139)).
or(Mapb_f2010.eq(239)).or(Mapb_f2010.eq(3239)).or(Mapb_f2010.eq(2939)).or(Mapb_f2010.eq(1339)).or(Mapb_f2010.eq(1539)).
or(Mapb_f2010.eq(2039)).or(Mapb_f2010.eq(4139)).or(Mapb_f2010.eq(4639)).or(Mapb_f2010.eq(2139)).or(Mapb_f2010.eq(2339)).or(Mapb_f2010.eq(2439)).
or(Mapb_f2010.eq(3039)).or(Mapb_f2010.eq(2539)).or(Mapb_f2010.eq(3339)).or(Mapb_f2010.eq(2739)).or(Mapb_f2010.eq(39))

var Mapb_f2011 = trans_a.select('transition_2011_2012')
var Mapb_fa_2011 =  Mapb_fa_2010.eq(1).and(Mapb_f2011.gt(299).and(Mapb_f2011.lt(600)))


var Mapb_c_2012 = Mapb_f2011.eq(339).or(Mapb_f2011.eq(439)).or(Mapb_f2011.eq(539)).or(Mapb_f2011.eq(939)).or(Mapb_f2011.eq(1139)).
or(Mapb_f2011.eq(239)).or(Mapb_f2011.eq(3239)).or(Mapb_f2011.eq(2939)).or(Mapb_f2011.eq(1339)).or(Mapb_f2011.eq(1539)).
or(Mapb_f2011.eq(2039)).or(Mapb_f2011.eq(4139)).or(Mapb_f2011.eq(4639)).or(Mapb_f2011.eq(2139)).or(Mapb_f2011.eq(2339)).or(Mapb_f2011.eq(2439)).
or(Mapb_f2011.eq(3039)).or(Mapb_f2011.eq(2539)).or(Mapb_f2011.eq(3339)).or(Mapb_f2011.eq(2739)).or(Mapb_f2011.eq(39))

var Mapb_f2012 = trans_a.select('transition_2012_2013')
var Mapb_fa_2012 =  Mapb_fa_2011.eq(1).and(Mapb_f2012.gt(299).and(Mapb_f2012.lt(600)))


var Mapb_c_2013 = Mapb_f2012.eq(339).or(Mapb_f2012.eq(439)).or(Mapb_f2012.eq(539)).or(Mapb_f2012.eq(939)).or(Mapb_f2012.eq(1139)).
or(Mapb_f2012.eq(239)).or(Mapb_f2012.eq(3239)).or(Mapb_f2012.eq(2939)).or(Mapb_f2012.eq(1339)).or(Mapb_f2012.eq(1539)).
or(Mapb_f2012.eq(2039)).or(Mapb_f2012.eq(4139)).or(Mapb_f2012.eq(4639)).or(Mapb_f2012.eq(2139)).or(Mapb_f2012.eq(2339)).or(Mapb_f2012.eq(2439)).
or(Mapb_f2012.eq(3039)).or(Mapb_f2012.eq(2539)).or(Mapb_f2012.eq(3339)).or(Mapb_f2012.eq(2739)).or(Mapb_f2012.eq(39))

var Mapb_f2013 = trans_a.select('transition_2013_2014')
var Mapb_fa_2013 =  Mapb_fa_2012.eq(1).and(Mapb_f2013.gt(299).and(Mapb_f2013.lt(600)))


var Mapb_c_2014 = Mapb_f2013.eq(339).or(Mapb_f2013.eq(439)).or(Mapb_f2013.eq(539)).or(Mapb_f2013.eq(939)).or(Mapb_f2013.eq(1139)).
or(Mapb_f2013.eq(239)).or(Mapb_f2013.eq(3239)).or(Mapb_f2013.eq(2939)).or(Mapb_f2013.eq(1339)).or(Mapb_f2013.eq(1539)).
or(Mapb_f2013.eq(2039)).or(Mapb_f2013.eq(4139)).or(Mapb_f2013.eq(4639)).or(Mapb_f2013.eq(2139)).or(Mapb_f2013.eq(2339)).or(Mapb_f2013.eq(2439)).
or(Mapb_f2013.eq(3039)).or(Mapb_f2013.eq(2539)).or(Mapb_f2013.eq(3339)).or(Mapb_f2013.eq(2739)).or(Mapb_f2013.eq(39))


var Mapb_f2014 = trans_a.select('transition_2014_2015')
var Mapb_fa_2014 =  Mapb_fa_2013.eq(1).and(Mapb_f2014.gt(299).and(Mapb_f2014.lt(600)))


var Mapb_c_2015 = Mapb_f2014.eq(339).or(Mapb_f2014.eq(439)).or(Mapb_f2014.eq(539)).or(Mapb_f2014.eq(939)).or(Mapb_f2014.eq(1139)).
or(Mapb_f2014.eq(239)).or(Mapb_f2014.eq(3239)).or(Mapb_f2014.eq(2939)).or(Mapb_f2014.eq(1339)).or(Mapb_f2014.eq(1539)).
or(Mapb_f2014.eq(2039)).or(Mapb_f2014.eq(4139)).or(Mapb_f2014.eq(4639)).or(Mapb_f2014.eq(2139)).or(Mapb_f2014.eq(2339)).or(Mapb_f2014.eq(2439)).
or(Mapb_f2014.eq(3039)).or(Mapb_f2014.eq(2539)).or(Mapb_f2014.eq(3339)).or(Mapb_f2014.eq(2739)).or(Mapb_f2014.eq(39))

var Mapb_f2015 = trans_a.select('transition_2015_2016')
var Mapb_fa_2015 =  Mapb_fa_2014.eq(1).and(Mapb_f2015.gt(299).and(Mapb_f2015.lt(600)))

var Mapb_c_2016 = Mapb_f2015.eq(339).or(Mapb_f2015.eq(439)).or(Mapb_f2015.eq(539)).or(Mapb_f2015.eq(939)).or(Mapb_f2015.eq(1139)).
or(Mapb_f2015.eq(239)).or(Mapb_f2015.eq(3239)).or(Mapb_f2015.eq(2939)).or(Mapb_f2015.eq(1339)).or(Mapb_f2015.eq(1539)).
or(Mapb_f2015.eq(2039)).or(Mapb_f2015.eq(4139)).or(Mapb_f2015.eq(4639)).or(Mapb_f2015.eq(2139)).or(Mapb_f2015.eq(2339)).or(Mapb_f2015.eq(2439)).
or(Mapb_f2015.eq(3039)).or(Mapb_f2015.eq(2539)).or(Mapb_f2015.eq(3339)).or(Mapb_f2015.eq(2739)).or(Mapb_f2015.eq(39))

var Mapb_f2016 = trans_a.select('transition_2016_2017')
var Mapb_fa_2016 =  Mapb_fa_2015.eq(1).and(Mapb_f2016.gt(299).and(Mapb_f2016.lt(600)))


var Mapb_c_2017 = Mapb_f2016.eq(339).or(Mapb_f2016.eq(439)).or(Mapb_f2016.eq(539)).or(Mapb_f2016.eq(939)).or(Mapb_f2016.eq(1139)).
or(Mapb_f2016.eq(239)).or(Mapb_f2016.eq(3239)).or(Mapb_f2016.eq(2939)).or(Mapb_f2016.eq(1339)).or(Mapb_f2016.eq(1539)).
or(Mapb_f2016.eq(2039)).or(Mapb_f2016.eq(4139)).or(Mapb_f2016.eq(4639)).or(Mapb_f2016.eq(2139)).or(Mapb_f2016.eq(2339)).or(Mapb_f2016.eq(2439)).
or(Mapb_f2016.eq(3039)).or(Mapb_f2016.eq(2539)).or(Mapb_f2016.eq(3339)).or(Mapb_f2016.eq(2739)).or(Mapb_f2016.eq(39))

var Mapb_f2017 = trans_a.select('transition_2017_2018')
var Mapb_fa_2017 =  Mapb_fa_2016.eq(1).and(Mapb_f2017.gt(299).and(Mapb_f2017.lt(600)))



var Mapb_c_2018 = Mapb_f2017.eq(339).or(Mapb_f2017.eq(439)).or(Mapb_f2017.eq(539)).or(Mapb_f2017.eq(939)).or(Mapb_f2017.eq(1139)).
or(Mapb_f2017.eq(239)).or(Mapb_f2017.eq(3239)).or(Mapb_f2017.eq(2939)).or(Mapb_f2017.eq(1339)).or(Mapb_f2017.eq(1539)).
or(Mapb_f2017.eq(2039)).or(Mapb_f2017.eq(4139)).or(Mapb_f2017.eq(4639)).or(Mapb_f2017.eq(2139)).or(Mapb_f2017.eq(2339)).or(Mapb_f2017.eq(2439)).
or(Mapb_f2017.eq(3039)).or(Mapb_f2017.eq(2539)).or(Mapb_f2017.eq(3339)).or(Mapb_f2017.eq(2739)).or(Mapb_f2017.eq(39))

var Mapb_f2018 = trans_a.select('transition_2018_2019')
var Mapb_fa_2018 =  Mapb_fa_2017.eq(1).and(Mapb_f2018.gt(299).and(Mapb_f2018.lt(600)))
Map.addLayer(Mapb_fa_2018,{}, "forest2018")

var Mapb_c_2019 = Mapb_f2018.eq(339).or(Mapb_f2018.eq(439)).or(Mapb_f2018.eq(539)).or(Mapb_f2018.eq(939)).or(Mapb_f2018.eq(1139)).
or(Mapb_f2018.eq(239)).or(Mapb_f2018.eq(3239)).or(Mapb_f2018.eq(2939)).or(Mapb_f2018.eq(1339)).or(Mapb_f2018.eq(1539)).
or(Mapb_f2018.eq(2039)).or(Mapb_f2018.eq(4139)).or(Mapb_f2018.eq(4639)).or(Mapb_f2018.eq(2139)).or(Mapb_f2018.eq(2339)).or(Mapb_f2018.eq(2439)).
or(Mapb_f2018.eq(3039)).or(Mapb_f2018.eq(2539)).or(Mapb_f2018.eq(3339)).or(Mapb_f2018.eq(2739)).or(Mapb_f2018.eq(39))

//identify deforestation for soybeans within 5 years past deforestation (forest in t-1 and turned into cropland at t,t+1, t+2, t+3, t+4, t+5)
// corrected to only count the year specific deforestation events (prior only forest and soy defoed, not non-forest in the subsequent year)
//var defC_2000_test = (Mapb_fa_1999.eq(1).and(Mapb_fa_2000.neq(1)));
//Map.addLayer(defC_2000_test,{}, 'def');

//var defC_2000 = (Mapb_fa_1999.eq(1).and(Mapb_fa_2000.neq(1))).and((Mapb_c_2000.eq(1)).or(Mapb_c_2001.eq(1)).or(Mapb_c_2002.eq(1)).or(Mapb_c_2003.eq(1)).or(Mapb_c_2004.eq(1)))//.or(Mapb_c_2005.eq(1)))
//Map.addLayer(defC_2000,{}, 'def_soy');
//Map.addLayer(Mapb_c_2000,{}, 'soy2000');
//Map.addLayer(Mapb_c_2001,{}, 'soy2001');
//Map.addLayer(Mapb_c_2002,{}, 'soy2002');
//Map.addLayer(Mapb_c_2003,{}, 'soy2003');
//Map.addLayer(Mapb_c_2004,{}, 'soy2004');

var defC_2001 = (Mapb_fa_2000.eq(1).and(Mapb_fa_2001.neq(1))).and((Mapb_c_2001.eq(1)).or(Mapb_c_2002.eq(1)).or(Mapb_c_2003.eq(1)).or(Mapb_c_2004.eq(1)).or(Mapb_c_2005.eq(1)))//.or(Mapb_c_2006.eq(1)))
var defC_2002 = (Mapb_fa_2001.eq(1).and(Mapb_fa_2002.neq(1))).and((Mapb_c_2002.eq(1)).or(Mapb_c_2003.eq(1)).or(Mapb_c_2004.eq(1)).or(Mapb_c_2005.eq(1)).or(Mapb_c_2006.eq(1)))//.or(Mapb_c_2007.eq(1)))
var defC_2003 = (Mapb_fa_2002.eq(1).and(Mapb_fa_2003.neq(1))).and((Mapb_c_2003.eq(1)).or(Mapb_c_2004.eq(1)).or(Mapb_c_2005.eq(1)).or(Mapb_c_2006.eq(1)).or(Mapb_c_2007.eq(1)))//.or(Mapb_c_2008.eq(1)))


var defC_2004 = (Mapb_fa_2003.eq(1).and(Mapb_fa_2004.neq(1))).and((Mapb_c_2004.eq(1)).or(Mapb_c_2005.eq(1)).or(Mapb_c_2006.eq(1)).or(Mapb_c_2007.eq(1)).or(Mapb_c_2008.eq(1)))//.or(Mapb_c_2009.eq(1)))
var defC_2005 = (Mapb_fa_2004.eq(1).and(Mapb_fa_2005.neq(1))).and((Mapb_c_2005.eq(1)).or(Mapb_c_2006.eq(1)).or(Mapb_c_2007.eq(1)).or(Mapb_c_2008.eq(1)).or(Mapb_c_2009.eq(1)))//.or(Mapb_c_2010.eq(1)))
var defC_2006 = (Mapb_fa_2005.eq(1).and(Mapb_fa_2006.neq(1))).and((Mapb_c_2006.eq(1)).or(Mapb_c_2007.eq(1)).or(Mapb_c_2008.eq(1)).or(Mapb_c_2009.eq(1)).or(Mapb_c_2010.eq(1)))//.or(Mapb_c_2011.eq(1)))
var defC_2007 = (Mapb_fa_2006.eq(1).and(Mapb_fa_2007.neq(1))).and((Mapb_c_2007.eq(1)).or(Mapb_c_2008.eq(1)).or(Mapb_c_2009.eq(1)).or(Mapb_c_2010.eq(1)).or(Mapb_c_2011.eq(1)))//.or(Mapb_c_2012.eq(1)))
var defC_2008 = (Mapb_fa_2007.eq(1).and(Mapb_fa_2008.neq(1))).and((Mapb_c_2008.eq(1)).or(Mapb_c_2009.eq(1)).or(Mapb_c_2010.eq(1)).or(Mapb_c_2011.eq(1)).or(Mapb_c_2012.eq(1)))//.or(Mapb_c_2013.eq(1)))
var defC_2009 = (Mapb_fa_2008.eq(1).and(Mapb_fa_2009.neq(1))).and((Mapb_c_2009.eq(1)).or(Mapb_c_2010.eq(1)).or(Mapb_c_2011.eq(1)).or(Mapb_c_2012.eq(1)).or(Mapb_c_2013.eq(1)))//.or(Mapb_c_2014.eq(1)))
var defC_2010 = (Mapb_fa_2009.eq(1).and(Mapb_fa_2010.neq(1))).and((Mapb_c_2010.eq(1)).or(Mapb_c_2011.eq(1)).or(Mapb_c_2012.eq(1)).or(Mapb_c_2013.eq(1)).or(Mapb_c_2014.eq(1)))//.or(Mapb_c_2015.eq(1)))
var defC_2011 = (Mapb_fa_2010.eq(1).and(Mapb_fa_2011.neq(1))).and((Mapb_c_2011.eq(1)).or(Mapb_c_2012.eq(1)).or(Mapb_c_2013.eq(1)).or(Mapb_c_2014.eq(1)).or(Mapb_c_2015.eq(1)))//.or(Mapb_c_2016.eq(1)))
var defC_2012 = (Mapb_fa_2011.eq(1).and(Mapb_fa_2012.neq(1))).and((Mapb_c_2012.eq(1)).or(Mapb_c_2013.eq(1)).or(Mapb_c_2014.eq(1)).or(Mapb_c_2015.eq(1)).or(Mapb_c_2016.eq(1)))//.or(Mapb_c_2017.eq(1)))
var defC_2013 = (Mapb_fa_2012.eq(1).and(Mapb_fa_2013.neq(1))).and((Mapb_c_2013.eq(1)).or(Mapb_c_2014.eq(1)).or(Mapb_c_2015.eq(1)).or(Mapb_c_2016.eq(1)).or(Mapb_c_2017.eq(1)))//.or(Mapb_c_2018.eq(1)))
var defC_2014 = (Mapb_fa_2013.eq(1).and(Mapb_fa_2014.neq(1))).and((Mapb_c_2014.eq(1)).or(Mapb_c_2015.eq(1)).or(Mapb_c_2016.eq(1)).or(Mapb_c_2017.eq(1)).or(Mapb_c_2018.eq(1)))//.or(Mapb_c_2019.eq(1)))
var defC_2015 = (Mapb_fa_2014.eq(1).and(Mapb_fa_2015.neq(1))).and((Mapb_c_2015.eq(1)).or(Mapb_c_2016.eq(1)).or(Mapb_c_2017.eq(1)).or(Mapb_c_2018.eq(1)).or(Mapb_c_2019.eq(1)))
var defC_2016 = (Mapb_fa_2015.eq(1).and(Mapb_fa_2016.neq(1))).and((Mapb_c_2016.eq(1)).or(Mapb_c_2017.eq(1)).or(Mapb_c_2018.eq(1)).or(Mapb_c_2019.eq(1)))//Mapb_c_2016.eq(1).or
var defC_2017 = (Mapb_fa_2016.eq(1).and(Mapb_fa_2017.neq(1))).and((Mapb_c_2017.eq(1)).or(Mapb_c_2018.eq(1)).or(Mapb_c_2019.eq(1)))//Mapb_c_2017.eq(1).or
var defC_2018 = (Mapb_fa_2017.eq(1).and(Mapb_fa_2018.neq(1))).and((Mapb_c_2018.eq(1)).or(Mapb_c_2019.eq(1)))//Mapb_c_2017.eq(1).or
var defC_2019 = Mapb_fa_2018.eq(1).and(Mapb_c_2019.eq(1))//not needed -> and(Mapb_fa_2019.neq(1))).



//assign pixel area in sqaure meter to those pixels identified as deforestation for croplands (1) 
//var defC_2000A = defC_2000.multiply(ee.Image.pixelArea()).rename(['b2000'])
var defC_2001A = defC_2001.multiply(ee.Image.pixelArea()).rename(['b2001'])
var defC_2002A = defC_2002.multiply(ee.Image.pixelArea()).rename(['b2002'])
var defC_2003A = defC_2003.multiply(ee.Image.pixelArea()).rename(['b2003'])


var defC_2004A = defC_2004.multiply(ee.Image.pixelArea()).rename(['b2004'])
var defC_2005A = defC_2005.multiply(ee.Image.pixelArea()).rename(['b2005'])
var defC_2006A = defC_2006.multiply(ee.Image.pixelArea()).rename(['b2006'])

var defC_2007A = defC_2007.multiply(ee.Image.pixelArea()).rename(['b2007'])
var defC_2008A = defC_2008.multiply(ee.Image.pixelArea()).rename(['b2008'])
var defC_2009A = defC_2009.multiply(ee.Image.pixelArea()).rename(['b2009'])
var defC_2010A = defC_2010.multiply(ee.Image.pixelArea()).rename(['b2010'])
var defC_2011A = defC_2011.multiply(ee.Image.pixelArea()).rename(['b2011'])
var defC_2012A = defC_2012.multiply(ee.Image.pixelArea()).rename(['b2012'])
var defC_2013A = defC_2013.multiply(ee.Image.pixelArea()).rename(['b2013'])
var defC_2014A = defC_2014.multiply(ee.Image.pixelArea()).rename(['b2014'])
var defC_2015A = defC_2015.multiply(ee.Image.pixelArea()).rename(['b2015'])
var defC_2016A = defC_2016.multiply(ee.Image.pixelArea()).rename(['b2016'])
var defC_2017A = defC_2017.multiply(ee.Image.pixelArea()).rename(['b2017'])
var defC_2018A = defC_2018.multiply(ee.Image.pixelArea()).rename(['b2018'])
var defC_2019A = defC_2019.multiply(ee.Image.pixelArea()).rename(['b2019'])

Map.addLayer (defC_2016)

var stacked_DefC = defC_2001A.addBands(defC_2002A).addBands(defC_2003A).addBands(defC_2004A).
addBands(defC_2005A).addBands(defC_2006A).addBands(defC_2007A).addBands(defC_2008A).
addBands(defC_2009A).addBands(defC_2010A).addBands(defC_2011A).addBands(defC_2012A).addBands(defC_2013A).addBands(defC_2014A).
addBands(defC_2015A).addBands(defC_2016A).addBands(defC_2017A).addBands(defC_2018A).addBands(defC_2019A);

print('stacked_DefC', stacked_DefC);

//divide municipality shapes for summarizing deforestation for croplands 
var shape_1 = shape.filter(ee.Filter.lte('CD_GEOCMU','3146830'))  
var shape_2 = shape.filter(ee.Filter.gt('CD_GEOCMU','3146830'));

var def_region_1= stacked_DefC.reduceRegions({
  reducer: ee.Reducer.sum(),//.unweighted(),
  collection: shape_1,
  scale:30
});
var def_region_2= stacked_DefC.reduceRegions({
  reducer: ee.Reducer.sum(),//.unweighted(),
  collection: shape_2,
  scale:30
});

Export.table.toDrive({
  collection: def_region_1,
  description: 'defSoy_MapB2000_TransSOY_5_region_1',
  selectors:(["CD_GEOCMU","b2001","b2002","b2003","b2004","b2005","b2006","b2007","b2008","b2009","b2010","b2011","b2012","b2013","b2014","b2015","b2016","b2017","b2018","b2019"]),
  fileFormat: 'CSV',
  folder:'MapBiomas'
});

Export.table.toDrive({
  collection: def_region_2,
  description: 'defSoy_MapB2000_TransSOY_5_region_2',
  selectors:(["CD_GEOCMU","b2001","b2002","b2003","b2004","b2005","b2006","b2007","b2008","b2009","b2010","b2011","b2012","b2013","b2014","b2015","b2016","b2017","b2018", "b2019"]),
  fileFormat: 'CSV',
  folder:'MapBiomas'
});

