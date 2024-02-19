# -*- coding: utf-8 -*-
"""
Created on Thu Feb 15 19:39:41 2024

@author: madou
"""

city_shp.head()
city_shp.dtypes

for col in city_shp.columns:
    print(col)
    
list(city_shp)
list(city_shp.columns)
city_shp.keys()
city_shp.columns.values.tolist()
sorted(city_shp)

# check if two variables are the same
df1.equals(df2)