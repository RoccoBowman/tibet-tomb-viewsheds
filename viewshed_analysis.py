#Author: Rocco Bowman
#Contact: rbowman2@ucmerced.edu

class Unbuffered(object):
   def __init__(self, stream):
       self.stream = stream
   def write(self, data):
       self.stream.write(data)
       self.stream.flush()
   def writelines(self, datas):
       self.stream.writelines(datas)
       self.stream.flush()
   def __getattr__(self, attr):
       return getattr(self.stream, attr)

import sys
sys.stdout = Unbuffered(sys.stdout)


# Begin Script ####################################
print('Script started...')

# Establish working directories ####################################

# Change these paths to those on your local machine where the initial data resides and where you want your outputs to go
inpath = 'C:/Users/bowma/Documents/Tibet Tombs Article/Test/Data/'
outpath = 'C:/Users/bowma/Documents/Tibet Tombs Article/Test/'

# Load packages ####################################

import os
import processing
from PyQt5.QtGui import *
from qgis.core import QgsVectorFileWriter
import glob

print('Splitting tomb shapefile...')
input = inpath + 'tombs_albers.shp'
field = 'Name'
output = outpath + 'Points/'

processing.run(
    "qgis:splitvectorlayer",
    {'INPUT':input,
    'FIELD':field,
    'OUTPUT':output})

print('Complete.')

print('Making a buffer for each tomb point...')

points = 'C:/Users/bowma/Documents/Tibet Tombs Article/Test/Points/'

os.chdir(points)

for lyr in glob.glob('*.shp'):
    output5 = outpath + '5kbuffers/' + '5kbuff_' + lyr
    output10 = outpath + '10kbuffers/' + '10kbuff_' + lyr
    output30 = outpath + '30mbuffers/' + '30kbuff_' + lyr
    print('Buffering ' + lyr)
    processing.run("native:buffer", {'INPUT':str(outpath+'Points/'+lyr),'DISTANCE':10000,'SEGMENTS':50,'END_CAP_STYLE':0,'JOIN_STYLE':0,'MITER_LIMIT':2,'DISSOLVE':False,'OUTPUT':output10})
    processing.run("native:buffer", {'INPUT':str(outpath+'Points/'+lyr),'DISTANCE':5000,'SEGMENTS':50,'END_CAP_STYLE':0,'JOIN_STYLE':0,'MITER_LIMIT':2,'DISSOLVE':False,'OUTPUT':output5})
    processing.run("native:buffer", {'INPUT':str(outpath+'Points/'+lyr),'DISTANCE':30,'SEGMENTS':50,'END_CAP_STYLE':0,'JOIN_STYLE':0,'MITER_LIMIT':2,'DISSOLVE':False,'OUTPUT':output30})

print('Buffering Complete.')

print('Extracting 10km neighborhoods...')

buffers_10k = 'C:/Users/bowma/Documents/Tibet Tombs Article/Test/10kbuffers/'
os.chdir(buffers_10k)

for lyr in glob.glob('*shp'):
    output = outpath + 'Extracts/' + 'ex_' + lyr + '.tif'
    print('Extracting ' + lyr)
    processing.run("gdal:cliprasterbymasklayer", {'INPUT':'C:/Users/bowma/Documents/Tibet Tombs Article/Test/DEM/study_area_dem_albers.tif','MASK':lyr,'SOURCE_CRS':None,'TARGET_CRS':None,'NODATA':None,'ALPHA_BAND':False,'CROP_TO_CUTLINE':True,'KEEP_RESOLUTION':False,'SET_RESOLUTION':False,'X_RESOLUTION':None,'Y_RESOLUTION':None,'MULTITHREADING':False,'OPTIONS':'','DATA_TYPE':0,'EXTRA':'','OUTPUT':output})

print('Extraction Complete.')

print('Calculating Total Viewsheds...')

neighborhoods = 'C:/Users/bowma/Documents/Tibet Tombs Article/Test/Extracts/'
os.chdir(neighborhoods)

for rstr in glob.glob('*tif'):
    output = outpath + 'TotalViewsheds/' + 'view_' + rstr
    print('Analyzing ' + rstr)
    processing.run("visibility:visibility_index",
    {'DEM':str(outpath+'Extracts/'+rstr),
    'RADIUS':5000,
    'OBSERVER_HEIGHT':1.6,
    'SAMPLE':1,'DIRECTION':0,
    'INTERPOLATE':True,
    'USE_CURVATURE':True,
    'REFRACTION':0.13,
    'OUTPUT':output})
    
print('Analysis complete.')

print('Clipping viewsheds to final size...')

buffers = 'C:/Users/bowma/Documents/Tibet Tombs Article/Test/5kbuffers/'
viewsheds = 'C:/Users/bowma/Documents/Tibet Tombs Article/Test/TotalViewsheds/'

buffer_list = []

os.chdir(buffers)

for lyr in glob.glob('*.shp'):
    print('Storing ' + lyr + ' to list...')
    buffer_list.append(lyr)

x = -1

os.chdir(viewsheds)

for rstr in glob.glob('*.tif'):
    print('Clipping ' + rstr + '...')
    x = x + 1
    input = rstr
    mask = str(buffers + buffer_list[x])
    output = 'C:/Users/bowma/Documents/Tibet Tombs Article/Test/FinalClips/' + 'final_' + rstr
    processing.run("gdal:cliprasterbymasklayer",
        {'INPUT':input,
        'MASK':mask,
        'SOURCE_CRS':None,
        'TARGET_CRS':None,
        'NODATA':None,
        'ALPHA_BAND':True,
        'CROP_TO_CUTLINE':True,
        'KEEP_RESOLUTION':False,
        'SET_RESOLUTION':False,
        'X_RESOLUTION':None,
        'Y_RESOLUTION':None,
        'MULTITHREADING':False,
        'OPTIONS':'',
        'DATA_TYPE':0,
        'EXTRA':'',
        'OUTPUT':output})

print('Clipping complete.')

print('Removing lakes...')

viewsheds = 'C:/Users/bowma/Documents/Tibet Tombs Article/Test/FinalClips/'
os.chdir(viewsheds)
input = inpath + 'nat_earth_lakes.shp'

for rstr in glob.glob('*tif'):
    print('Burning '+ rstr + "...")
    processing.run("gdal:rasterize_over_fixed_value",
        {'INPUT':input,
        'INPUT_RASTER':str(outpath + 'FinalClips/' + rstr),
        'BURN':0,
        'ADD':False,
        'EXTRA':''})
        
print('Lakes removed from analysis.')

print('Calculating zonal statistics for entire neighborhoods...')

viewsheds = 'C:/Users/bowma/Documents/Tibet Tombs Article/Test/FinalClips/'
os.chdir(viewsheds)

x = -1

for rstr in glob.glob('*.tif'):
    print('Analyzing ' + rstr)
    x = x + 1
    input = rstr
    input_vector = str(buffers + buffer_list[x])
    processing.run("qgis:zonalstatistics",
        {'INPUT_RASTER':input,
        'RASTER_BAND':1,
        'INPUT_VECTOR':input_vector,
        'COLUMN_PREFIX':'zonal_',
        'STATS':[12]})
        
print('Total zones complete.')

print('Calculating zonal statistics for 30m tomb buffers...')

buffers = 'C:/Users/bowma/Documents/Tibet Tombs Article/Test/5kbuffers/'
buffers2 = 'C:/Users/bowma/Documents/Tibet Tombs Article/Test/30mbuffers/'
viewsheds = 'C:/Users/bowma/Documents/Tibet Tombs Article/Test/FinalClips/'

buffer_list2 = []

os.chdir(buffers2)

for lyr in glob.glob('*.shp'):
    print('Storing ' + lyr + ' to list...')
    buffer_list2.append(lyr)

x = -1

os.chdir(viewsheds)

for rstr in glob.glob('*.tif'):
    print('Analyzing ' + rstr + '...')
    x = x + 1
    input = rstr
    input_vector = str(buffers2 + buffer_list2[x])
    processing.run("qgis:zonalstatistics",
        {'INPUT_RASTER':input,
        'RASTER_BAND':1,
        'INPUT_VECTOR':input_vector,
        'COLUMN_PREFIX':'zonal_',
        'STATS':[12]})

print('Zonal statistics complete.')

print('Merging data...')

os.chdir(buffers)

buffer_list = []

for lyr in glob.glob('*.shp'):
    print('Storing ' + lyr + ' to list...')
    addition = str(buffers + lyr)
    buffer_list.append(addition)

output = str(outpath + 'Tabular/' + 'merge5k.shp')
processing.run("native:mergevectorlayers",
    {'LAYERS':buffer_list,
        'CRS':None,
        'OUTPUT':output})

os.chdir(buffers2)

buffer_list2 = []

for lyr in glob.glob('*.shp'):
    print('Storing ' + lyr + ' to list...')
    addition = str(buffers2 + lyr)
    buffer_list2.append(addition)

output2 = str(outpath + 'Tabular/' + 'merge30m.shp')
processing.run("native:mergevectorlayers",
    {'LAYERS':buffer_list2,
        'CRS':None,
        'OUTPUT':output2})

print('Merges complete.')

# Write out final shapefile and data as csv
input_shp = QgsVectorLayer(output,"export","ogr")
input_shp.isValid() 
output = outpath + 'Tabular/' + 'merge_10k.csv'
QgsVectorFileWriter.writeAsVectorFormat(input_shp,output,"UTF-8",input_shp.crs(),"CSV")

input_shp = QgsVectorLayer(output2,"export","ogr")
input_shp.isValid() 
output2 = outpath + 'Tabular/' + 'merge_30m.csv'
QgsVectorFileWriter.writeAsVectorFormat(input_shp,output2,"UTF-8",input_shp.crs(),"CSV")

print('Script completed.')
