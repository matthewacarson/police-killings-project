# =====================================================
# Import Libraries
# =====================================================

# import census
# import sys
# from pathlib import Path
# from pyproj import Proj
# import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import os
import geopandas as gpd
from shapely.geometry import Point

pd.set_option('display.max_columns', None)
pd.set_option('display.max_rows', None)
pd.options.display.float_format = '{:.2f}'.format # avoid scientific notation

# %%
home = "C:/Users/madou/OneDrive - UCLA IT Services/1)_PS-Honors/police_killings_github/udp_expansion_matt"  # str(Path.home())
input_path = home+'/data/inputs/'
output_path = home+'/data/outputs/'
# %%

os.chdir(home)

os.getcwd()
# =====================================================
# Crosswalk Files
# =====================================================

# =====================================================
# Read Files
# =====================================================
# Note: Most of the input files are located on google drive.
# UDP suggests downloading [Google's Drive File Stream](https://support.google.com/a/answer/7491144?utm_medium=et&utm_source=aboutdrive&utm_content=getstarted&utm_campaign=en_us)
# app, which doesn't download all Google Drive items to your computer
# but rather pulls them as necessary. This will save a lot of space but compromises speed.
#%% 2 
# Data files #####

# dtype_dict = {col: float for col in range(11, 51)}
census_90 = pd.read_csv('R13437364_SL140_1990.csv') #, dtype=dtype_dict)
# census_90 = pd.read_csv('R13437364_SL140_subset.csv', index_col = 0)
# dtype_dict = {col: float for col in range(1, 51)}
census_00 = pd.read_csv('merged_2000.csv')#, dtype=dtype_dict)

# Crosswalk files
# dtype_dict = {col: float for col in range(0, 51)}
xwalk_90_10 = pd.read_csv(input_path+'crosswalk_1990_2010.csv')#, dtype=dtype_dict)
xwalk_00_10 = pd.read_csv(input_path+'crosswalk_2000_2010.csv')#, dtype=dtype_dict)
# ####

# =====================================================
# Create Crosswalk Functions / Files
# =====================================================

#%% 3
def crosswalk_files (df, xwalk, counts, medians, df_fips_base, xwalk_fips_base, xwalk_fips_horizon):
    # merge dataframe with xwalk file
    df_merge = df.merge(xwalk[['weight', xwalk_fips_base, xwalk_fips_horizon]], left_on = df_fips_base, right_on = xwalk_fips_base, how='left')
    df = df_merge
    # apply interpolation weight
    new_var_list = list(counts)+(medians)
    for var in new_var_list:
        df[var] = df[var]*df['weight']
    # aggregate by horizon census tracts fips
    df = df.groupby(xwalk_fips_horizon).sum().reset_index()
    # rename trtid10 to FIPS & FIPS to trtid_base
    df = df.rename(columns = {'FIPS':'trtid_base',
                              'trtid10':'FIPS'})
    # fix state, county and fips code
    df ['state'] = df['FIPS'].astype('int64').astype(str).str.zfill(11).str[0:2]
    df ['county'] = df['FIPS'].astype('int64').astype(str).str.zfill(11).str[2:5]
    df ['tract'] = df['FIPS'].astype('int64').astype(str).str.zfill(11).str[5:]
    # drop weight column
    df = df.drop(columns = ['weight'])
    return df


#%% 4
# Crosswalking
# --------------------------------------------------
# 9/11: I am running into some problems with multiplying non-integers (see error message below)
# TypeError: can't multiply sequence by non-int of type 'float'
# Use .dtypes to check the data storage types
# Check lists with type(list_name)
## 1990 Census Data

counts = census_90.columns.drop(['mrent_90', 'mhval_90', 'hinc_90', "NAME", "QName", "SUMLEV", "GEOCOMP", "REGION", "DIVISION", "FIPS", "state", "county", "tract"])
medians = ['mrent_90', 'mhval_90', 'hinc_90']
df_fips_base = 'FIPS'
xwalk_fips_base = 'trtid90'
xwalk_fips_horizon = 'trtid10'
census_90_xwalked = crosswalk_files (census_90, xwalk_90_10,  counts, medians, df_fips_base, xwalk_fips_base, xwalk_fips_horizon )

del [census_90, xwalk_90_10]
#%% 5
## 2000 Census Data

counts = census_00.columns.drop(['mrent_00', 'mhval_00', 'hinc_00', 'NAME', 'FIPS'])
medians = ['mrent_00', 'mhval_00', 'hinc_00']
df_fips_base = 'FIPS'
xwalk_fips_base = 'trtid00'
xwalk_fips_horizon = 'trtid10'
census_00_xwalked = crosswalk_files (census_00, xwalk_00_10,  counts, medians, df_fips_base, xwalk_fips_base, xwalk_fips_horizon )

del [census_00, xwalk_00_10]
#%% 6
# =====================================================
# Variable Creation
# =====================================================
# =====================================================
# Setup / Read Files (inputs needed)
# =====================================================
# Note: Below is the Google File Drive Stream pathway for a Mac.
# input_path = '~/git/displacement-typologies/data/inputs/'
# Use this to draw in the 'input_path' variable needed below
# You will need to redesignate this path if you have a Windows
# output_path = output_path

# ################################################################ #
# I named the merged 2012 and 2018 acs files to merged_2012_2018.csv
# The data below was downloaded using matts_download_tidycensus.R
# ################################################################ #

census_2012_2018 = pd.read_csv('merged_2012_2018.csv')#, index_col = 0)
# census_2012_2018 = census_2012_2018.drop(columns = ['county_y', 'state_y', 'tract_y'])
#%% 7
census_2012_2018 = census_2012_2018.rename(columns = {'county_x': 'county',
                                    'state_x': 'state',
                                    'tract_x': 'tract',
                                    'GEOID': 'FIPS'}) # 9/11/23: I added this because tidycensus downloaded as GEOID
#%% 8
# Bring in PUMS data
# =====================================================


pums_r = pd.read_csv(input_path+'nhgis0002_ds233_20175_2017_tract.csv', encoding = "ISO-8859-1")
pums_o = pd.read_csv(input_path+'nhgis0002_ds234_20175_2017_tract.csv', encoding = "ISO-8859-1")
pums = pums_r.merge(pums_o, on = 'GISJOIN')
pums = pums.rename(columns = {'YEAR_x':'YEAR',
                               'STATE_x':'STATE',
                               'STATEA_x':'STATEA',
                               'COUNTY_x':'COUNTY',
                               'COUNTYA_x':'COUNTYA',
                               'TRACTA_x':'TRACTA',
                               'NAME_E_x':'NAME_E'})
pums = pums.dropna(axis = 1)

del [pums_r, pums_o]
#%% 9
# Bring in Zillow, Rail, Hospital, Unversity, LIHTC, PH dat
# =====================================================
# Note: Make sure your city/county is included in these overlay files

## Zillow data
zillow = pd.read_csv(input_path+'Zip_Zhvi_AllHomes.csv', encoding = "ISO-8859-1")
zillow_xwalk = pd.read_csv(input_path+'TRACT_ZIP_032015.csv')
#%% 10

## Rail data
rail = pd.read_csv(input_path+'tod_database_download.csv')

## Hospitals (not using)
hospitals = pd.read_csv(input_path+'Hospitals.csv')

## Universities (not using)
university = pd.read_csv(input_path+'university_HD2016.csv')

## LIHTC
# dtype_dict = {col: float for col in range(0, 51)}
lihtc = pd.read_csv(input_path+'LowIncome_Housing_Tax_Credit_Properties.csv', na_values=[''])

## Public housing
pub_hous = pd.read_csv(input_path+'Public_Housing_Buildings.csv.gz')
#%% 11
# pub_hous.head(9)

# =====================================================
# Read Shapefile Data (inputs needed)
# =====================================================
# Note: Similar to above, add a 'elif' for you city here
# Pull cartographic boundary files from here:
# https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.2017.html
import glob

# Directory containing the Shapefiles
directory = r'C:\Users\madou\OneDrive - UCLA IT Services\1)_PS-Honors\police-killings-project_union_PC\udp_expansion_matt\data\inputs\shp'

# Search for all .shp files in the directory
file_paths = glob.glob(directory + '\\*.shp')

# run in parallel
from concurrent.futures import ThreadPoolExecutor

def combine_shapefile(file_path):
    """Function to combine a single Shapefile."""
    return gpd.read_file(file_path)

# Create a ThreadPoolExecutor with max_workers set to the number of CPU cores
with ThreadPoolExecutor(max_workers=None) as executor:
    # Submit tasks to read each Shapefile asynchronously
    futures = [executor.submit(combine_shapefile, file) for file in file_paths]
    
    # Wait for all tasks to complete and retrieve the results
    shapefiles = [future.result() for future in futures]

# Combine the Shapefiles into a single GeoDataFrame
city_shp = gpd.GeoDataFrame(pd.concat(shapefiles, ignore_index=True))

# Save the combined GeoDataFrame as a new Shapefile
city_shp.to_file('combined_output.shp')
city_shp = gpd.read_file('C:/Users/madou/OneDrive - UCLA IT Services/1)_PS-Honors/police_killings_github/udp_expansion_matt/combined_output.shp')
# =====================================================
# Income Interpolation
# =====================================================

# Merge census data in single file
# =====================================================
# Changed the data frame names below because I did not filter regions of interest. I used all FIPS codes.
census = census_2012_2018.merge(census_00_xwalked, on = 'FIPS', how = 'outer').merge(census_90_xwalked, on = 'FIPS', how = 'outer')

# del [census_2012_2018, census_00_xwalked, census_90_xwalked]
## CPI indexing values
## This is based on the yearly CPI average
## Add in new CPI based on current year: https://www.bls.gov/data/inflation_calculator.htm
CPI_89_18 = 2.08
CPI_99_18 = 1.53
CPI_12_18 = 1.11

## This is used for the Zillow data, where january values are compared
CPI_0115_0119 = 1.077
#%% 12

# Income Interpolation

census['hinc_18'][census['hinc_18']<0]=np.nan
census['hinc_00'][census['hinc_00']<0]=np.nan
census['hinc_90'][census['hinc_90']<0]=np.nan

census.loc[census['hinc_18'] < 0, 'hinc_18'] = np.nan
census.loc[census['hinc_00'] < 0, 'hinc_00'] = np.nan
census.loc[census['hinc_90'] < 0, 'hinc_90'] = np.nan

# census.shape

hinc_columns = census.filter(like='hinc')
hinc_columns.to_csv(output_path+'databases/hinc_columns.csv')
# del hinc_columns

#%% 14
## Calculate regional medians (note that these are not indexed)
rm_hinc_18 = np.nanmedian(census['hinc_18'])
rm_hinc_00 = np.nanmedian(census['hinc_00'])
rm_hinc_90 = np.nanmedian(census['hinc_90'])
rm_iinc_18 = np.nanmedian(census['iinc_18'])
rm_iinc_12 = np.nanmedian(census['iinc_12'])
#%% 15
# print(rm_hinc_18, rm_hinc_00, rm_hinc_90, rm_iinc_18, rm_iinc_12)

## Income Interpolation Function
## This function interpolates population counts using income buckets provided by the Census
#%% 16 
def income_interpolation (census, year, cutoff, mhinc, tot_var, var_suffix, out):
    name = []
    for c in list(census.columns):
        if (c[0]==var_suffix):
            if c.split('_')[2]==year:
                name.append(c)
    name.append('FIPS')
    name.append(tot_var)
    income_cat = census[name]
    income_group = income_cat.drop(columns = ['FIPS', tot_var]).columns
    income_group = income_group.str.split('_')
    number = []
    for i in range (0, len(income_group)):
        number.append(income_group[i][1])
    column = []
    for i in number:
        column.append('prop_'+str(i))
        income_cat['prop_'+str(i)] = income_cat[var_suffix+'_'+str(i)+'_'+year]/income_cat[tot_var]
    reg_median_cutoff = cutoff*mhinc
    cumulative = out+str(int(cutoff*100))+'_cumulative'
    income = out+str(int(cutoff*100))+'_'+year
    df = income_cat
    df[cumulative] = 0
    df[income] = 0
    for i in range(0,(len(number)-1)):
        a = (number[i])
        b = float(number[i+1])-0.01
        prop = str(number[i+1])
        df[cumulative] = df[cumulative]+df['prop_'+a]
        if (reg_median_cutoff>=int(a))&(reg_median_cutoff<b):
            df[income] = ((reg_median_cutoff - int(a))/(b-int(a)))*df['prop_'+prop] + df[cumulative]
    df = df.drop(columns = [cumulative])
    prop_col = df.columns[df.columns.str[0:4]=='prop']
    df = df.drop(columns = prop_col)
    census = census.merge (df[['FIPS', income]], on = 'FIPS')
    return census

census = income_interpolation (census, '18', 0.8, rm_hinc_18, 'hh_18', 'I', 'inc')
census = income_interpolation (census, '18', 1.2, rm_hinc_18, 'hh_18', 'I', 'inc')
census = income_interpolation (census, '00', 0.8, rm_hinc_00, 'hh_00', 'I', 'inc')
census = income_interpolation (census, '00', 1.2, rm_hinc_00, 'hh_00', 'I', 'inc')
census = income_interpolation (census, '90', 0.8, rm_hinc_90, 'hh_00', 'I', 'inc')

income_col = census.columns[census.columns.str[0:2]=='I_']
census = census.drop(columns = income_col)

#%% 29 STOP HERE FOR NOW!!!!
# Not sure what to do with the line below yet (9/25/23)
# census = census.drop(columns = income_col)

#%% 30
# =====================================================
# Generate Income Categories
# =====================================================

# Create Category Function + Run
# --------------------------------------------------
#%% 31
def income_categories (df, year, mhinc, hinc):
    df['hinc_'+year] = np.where(df['hinc_'+year]<0, 0, df['hinc_'+year])
    reg_med_inc80 = 0.8*mhinc
    reg_med_inc120 = 1.2*mhinc
    low = 'low_80120_'+year
    mod = 'mod_80120_'+year
    high = 'high_80120_'+year
    df[low] = df['inc80_'+year]
    df[mod] = df['inc120_'+year] - df['inc80_'+year]
    df[high] = 1 - df['inc120_'+year]
    ## Low income
    df['low_pdmt_medhhinc_'+year] = np.where((df['low_80120_'+year]>=0.55)&(df['mod_80120_'+year]<0.45)&(df['high_80120_'+year]<0.45),1,0)
    ## High income
    df['high_pdmt_medhhinc_'+year] = np.where((df['low_80120_'+year]<0.45)&(df['mod_80120_'+year]<0.45)&(df['high_80120_'+year]>=0.55),1,0)
    ## Moderate income
    df['mod_pdmt_medhhinc_'+year] = np.where((df['low_80120_'+year]<0.45)&(df['mod_80120_'+year]>=0.55)&(df['high_80120_'+year]<0.45),1,0)
    ## Mixed-Low income
    df['mix_low_medhhinc_'+year] = np.where((df['low_pdmt_medhhinc_'+year]==0)&
                                                  (df['mod_pdmt_medhhinc_'+year]==0)&
                                                  (df['high_pdmt_medhhinc_'+year]==0)&
                                                  (df[hinc]<reg_med_inc80),1,0)
    ## Mixed-Moderate income
    df['mix_mod_medhhinc_'+year] = np.where((df['low_pdmt_medhhinc_'+year]==0)&
                                                  (df['mod_pdmt_medhhinc_'+year]==0)&
                                                  (df['high_pdmt_medhhinc_'+year]==0)&
                                                  (df[hinc]>=reg_med_inc80)&
                                                  (df[hinc]<reg_med_inc120),1,0)
    ## Mixed-High income
    df['mix_high_medhhinc_'+year] = np.where((df['low_pdmt_medhhinc_'+year]==0)&
                                                  (df['mod_pdmt_medhhinc_'+year]==0)&
                                                  (df['high_pdmt_medhhinc_'+year]==0)&
                                                  (df[hinc]>=reg_med_inc120),1,0)
    df['inc_cat_medhhinc_'+year] = 0
    df.loc[df['low_pdmt_medhhinc_'+year]==1, 'inc_cat_medhhinc_'+year] = 1
    df.loc[df['mix_low_medhhinc_'+year]==1, 'inc_cat_medhhinc_'+year] = 2
    df.loc[df['mod_pdmt_medhhinc_'+year]==1, 'inc_cat_medhhinc_'+year] = 3
    df.loc[df['mix_mod_medhhinc_'+year]==1, 'inc_cat_medhhinc_'+year] = 4
    df.loc[df['mix_high_medhhinc_'+year]==1, 'inc_cat_medhhinc_'+year] = 5
    df.loc[df['high_pdmt_medhhinc_'+year]==1, 'inc_cat_medhhinc_'+year] = 6
    df['inc_cat_medhhinc_encoded'+year] = 0
    df.loc[df['low_pdmt_medhhinc_'+year]==1, 'inc_cat_medhhinc_encoded'+year] = 'low_pdmt'
    df.loc[df['mix_low_medhhinc_'+year]==1, 'inc_cat_medhhinc_encoded'+year] = 'mix_low'
    df.loc[df['mod_pdmt_medhhinc_'+year]==1, 'inc_cat_medhhinc_encoded'+year] = 'mod_pdmt'
    df.loc[df['mix_mod_medhhinc_'+year]==1, 'inc_cat_medhhinc_encoded'+year] = 'mix_mod'
    df.loc[df['mix_high_medhhinc_'+year]==1, 'inc_cat_medhhinc_encoded'+year] = 'mix_high'
    df.loc[df['high_pdmt_medhhinc_'+year]==1, 'inc_cat_medhhinc_encoded'+year] = 'high_pdmt'
    df.loc[df['hinc_'+year]==0, 'low_pdmt_medhhinc_'+year] = np.nan
    df.loc[df['hinc_'+year]==0, 'mix_low_medhhinc_'+year] = np.nan
    df.loc[df['hinc_'+year]==0, 'mod_pdmt_medhhinc_'+year] = np.nan
    df.loc[df['hinc_'+year]==0, 'mix_mod_medhhinc_'+year] = np.nan
    df.loc[df['hinc_'+year]==0, 'mix_high_medhhinc_'+year] = np.nan
    df.loc[df['hinc_'+year]==0, 'high_pdmt_medhhinc_'+year] = np.nan
    df.loc[df['hinc_'+year]==0, 'inc_cat_medhhinc_'+year] = np.nan
    return census


census = income_categories(census, '18', rm_hinc_18, 'hinc_18')
census = income_categories(census, '00', rm_hinc_00, 'hinc_00')

# census.groupby('inc_cat_medhhinc_00').count()['FIPS']

# census.groupby('inc_cat_medhhinc_18').count()['FIPS']

## Percentage & total low-income households - under 80% AMI
census ['per_all_li_90'] = census['inc80_90']
census ['per_all_li_00'] = census['inc80_00']
census ['per_all_li_18'] = census['inc80_18']

census['all_li_count_90'] = census['per_all_li_90']*census['hh_90']
census['all_li_count_00'] = census['per_all_li_00']*census['hh_00']
census['all_li_count_18'] = census['per_all_li_18']*census['hh_18']
#%% 
# len(census)
#%%
# =====================================================
# Rent, Median income, Home Value Data
# =====================================================

census['real_mhval_90'] = census['mhval_90']*CPI_89_18
census['real_mrent_90'] = census['mrent_90']*CPI_89_18
census['real_hinc_90'] = census['hinc_90']*CPI_89_18

census['real_mhval_00'] = census['mhval_00']*CPI_99_18
census['real_mrent_00'] = census['mrent_00']*CPI_99_18
census['real_hinc_00'] = census['hinc_00']*CPI_99_18

census['real_mhval_12'] = census['mhval_12']*CPI_12_18
census['real_mrent_12'] = census['mrent_12']*CPI_12_18
# census['real_hinc_12'] = census['hinc_12']*CPI_12_18 # this isn't calculated yet (2020.03.29)

census['real_mhval_18'] = census['mhval_18']
census['real_mrent_18'] = census['mrent_18']
census['real_hinc_18'] = census['hinc_18']

# =====================================================
# Demographic Data
# =====================================================

df = census

# % of non-white
# =====================================================

## 1990
df['per_nonwhite_90'] = 1 - df['white_90']/df['pop_90']

## 2000
df['per_nonwhite_00'] = 1 - df['white_00']/df['pop_00']

## 2018
df['per_nonwhite_18'] = 1 - df['white_18']/df['pop_18']

# % of owner and renter-occupied housing units
# =====================================================

## 1990
df['hu_90'] = df['ohu_90']+df['rhu_90']
df['per_rent_90'] = df['rhu_90']/df['hu_90']

## 2000
df['per_rent_00'] = df['rhu_00']/df['hu_00']

## 2018
df['hu_18'] = df['ohu_18']+df['rhu_18']
df['per_rent_18'] = df['rhu_18']/df['hu_18']

# % of college educated
# =====================================================

## 1990
var_list = ['total_25_col_9th_90',
            'total_25_col_12th_90',
            'total_25_col_hs_90',
            'total_25_col_sc_90',
            'total_25_col_ad_90',
            'total_25_col_bd_90',
            'total_25_col_gd_90']
df['total_25_90'] = df[var_list].sum(axis = 1)
df['per_col_90'] = (df['total_25_col_bd_90']+df['total_25_col_gd_90'])/(df['total_25_90'])

## 2000
df['male_25_col_00'] = (df['male_25_col_bd_00']+
                        df['male_25_col_md_00']+
                        df['male_25_col_phd_00'])
df['female_25_col_00'] = (df['female_25_col_bd_00']+
                          df['female_25_col_md_00']+
                          df['female_25_col_phd_00'])
df['total_25_col_00'] = df['male_25_col_00']+df['female_25_col_00']
df['per_col_00'] = df['total_25_col_00']/df['total_25_00']

## 2018
df['per_col_18'] = (df['total_25_col_bd_18']+
                    df['total_25_col_md_18']+
                    df['total_25_col_pd_18']+
                    df['total_25_col_phd_18'])/df['total_25_18']

# Housing units built
# =====================================================

df['per_units_pre50_18'] = (df['units_40_49_built_18']+df['units_39_early_built_18'])/df['tot_units_built_18']

## Percent of people who have moved who are low-income
# This function interpolates in mover population counts using income buckets provided by the Census

def income_interpolation_movein (census, year, cutoff, rm_iinc):
    # SUM EVERY CATEGORY BY INCOME
    ## Filter only move-in variables
    name = []
    for c in list(census.columns):
        if (c[0:3] == 'mov') & (c[-2:]==year):
            name.append(c)
    name.append('FIPS')
    income_cat = census[name]
    ## Pull income categories
    income_group = income_cat.drop(columns = ['FIPS']).columns
    number = []
    for c in name[:9]:
        number.append(c.split('_')[2])
    ## Sum move-in in last 5 years by income category, including total w/ income
    column_name_totals = []
    for i in number:
        column_name = []
        for j in income_group:
            if j.split('_')[2] == i:
                column_name.append(j)
        if i == 'w':
            i = 'w_income'
        income_cat['mov_tot_'+i+'_'+year] = income_cat[column_name].sum(axis = 1)
        column_name_totals.append('mov_tot_'+i+'_'+year)
    # DO INCOME INTERPOLATION
    column = []
    number = [n for n in number if n != 'w'] ## drop total
    for i in number:
        column.append('prop_mov_'+i)
        income_cat['prop_mov_'+i] = income_cat['mov_tot_'+i+'_'+year]/income_cat['mov_tot_w_income_'+year]
    reg_median_cutoff = cutoff*rm_iinc
    cumulative = 'inc'+str(int(cutoff*100))+'_cumulative'
    per_limove = 'per_limove_'+year
    df = income_cat
    df[cumulative] = 0
    df[per_limove] = 0
    for i in range(0,(len(number)-1)):
        a = (number[i])
        b = float(number[i+1])-0.01
        prop = str(number[i+1])
        df[cumulative] = df[cumulative]+df['prop_mov_'+a]
        if (reg_median_cutoff>=int(a))&(reg_median_cutoff<b):
            df[per_limove] = ((reg_median_cutoff - int(a))/(b-int(a)))*df['prop_mov_'+prop] + df[cumulative]
    df = df.drop(columns = [cumulative])
    prop_col = df.columns[df.columns.str[0:4]=='prop']
    df = df.drop(columns = prop_col)
    col_list = [per_limove]+['mov_tot_w_income_'+year]
    census = census.merge (df[['FIPS'] + col_list], on = 'FIPS')
    return census

census = income_interpolation_movein (census, '18', 0.8, rm_iinc_18)
census = income_interpolation_movein (census, '12', 0.8, rm_iinc_12)

# len(census)

# =====================================================
# Housing Affordability Variables
# =====================================================

# def filter_PUMS(df, FIPS):
#     if (city_name not in ('Memphis', 'Boston')):
#         FIPS = [int(x) for x in FIPS]
#         df = df[(df['STATEA'] == int(state))&(df['COUNTYA'].isin(FIPS))].reset_index(drop = True)
#     else:
#         fips_list = []
#         for i in state:
#             county = FIPS[i]
#             county = [int(x) for x in county]
#             a = list((df['GISJOIN'][(pums['STATEA']==int(i))&(df['COUNTYA'].isin(county))]))
#             fips_list += a
#             df = df[df['GISJOIN'].isin(fips_list)].reset_index(drop = True)
#     return df
# 
# pums = filter_PUMS(pums, FIPS)
pums['FIPS'] = ((pums['STATEA'].astype(str).str.zfill(2))+
                (pums['COUNTYA'].astype(str).str.zfill(3))+
                (pums['TRACTA'].astype(str).str.zfill(6)))

pums = pums.rename(columns = {"AH5QE002":"rhu_18_wcash",
                                "AH5QE003":"R_100_18",
                                "AH5QE004":"R_150_18",
                                "AH5QE005":"R_200_18",
                                "AH5QE006":"R_250_18",
                                "AH5QE007":"R_300_18",
                                "AH5QE008":"R_350_18",
                                "AH5QE009":"R_400_18",
                                "AH5QE010":"R_450_18",
                                "AH5QE011":"R_500_18",
                                "AH5QE012":"R_550_18",
                                "AH5QE013":"R_600_18",
                                "AH5QE014":"R_650_18",
                                "AH5QE015":"R_700_18",
                                "AH5QE016":"R_750_18",
                                "AH5QE017":"R_800_18",
                                "AH5QE018":"R_900_18",
                                "AH5QE019":"R_1000_18",
                                "AH5QE020":"R_1250_18",
                                "AH5QE021":"R_1500_18",
                                "AH5QE022":"R_2000_18",
                                "AH5QE023":"R_2500_18",
                                "AH5QE024":"R_3000_18",
                                "AH5QE025":"R_3500_18",
                                "AH5QE026":"R_3600_18",
                                "AH5QE027":"rhu_18_wocash",
                                "AIMUE001":"ohu_tot_18",
                                "AIMUE002":"O_200_18",
                                "AIMUE003":"O_300_18",
                                "AIMUE004":"O_400_18",
                                "AIMUE005":"O_500_18",
                                "AIMUE006":"O_600_18",
                                "AIMUE007":"O_700_18",
                                "AIMUE008":"O_800_18",
                                "AIMUE009":"O_900_18",
                                "AIMUE010":"O_1000_18",
                                "AIMUE011":"O_1250_18",
                                "AIMUE012":"O_1500_18",
                                "AIMUE013":"O_2000_18",
                                "AIMUE014":"O_2500_18",
                                "AIMUE015":"O_3000_18",
                                "AIMUE016":"O_3500_18",
                                "AIMUE017":"O_4000_18",
                                "AIMUE018":"O_4100_18"})

aff_18 = rm_hinc_18*0.3/12
pums = income_interpolation (pums, '18', 0.6, aff_18, 'rhu_18_wcash', 'R', 'rent')
pums = income_interpolation (pums, '18', 1.2, aff_18, 'rhu_18_wcash', 'R', 'rent')

pums = income_interpolation (pums, '18', 0.6, aff_18, 'ohu_tot_18', 'O', 'own')
pums = income_interpolation (pums, '18', 1.2, aff_18, 'ohu_tot_18', 'O', 'own')

pums['FIPS'] = pums['FIPS'].astype(float).astype('int64')
pums = pums.merge(census[['FIPS', 'mmhcosts_18']], on = 'FIPS')

pums['rlow_18'] = pums['rent60_18']*pums['rhu_18_wcash']+pums['rhu_18_wocash'] ## includes no cash rent
pums['rmod_18'] = pums['rent120_18']*pums['rhu_18_wcash']-pums['rent60_18']*pums['rhu_18_wcash']
pums['rhigh_18'] = pums['rhu_18_wcash']-pums['rent120_18']*pums['rhu_18_wcash']

pums['olow_18'] = pums['own60_18']*pums['ohu_tot_18']
pums['omod_18'] = pums['own120_18']*pums['ohu_tot_18'] - pums['own60_18']*pums['ohu_tot_18']
pums['ohigh_18'] = pums['ohu_tot_18'] - pums['own120_18']*pums['ohu_tot_18']

pums['hu_tot_18'] = pums['rhu_18_wcash']+pums['rhu_18_wocash']+pums['ohu_tot_18']

pums['low_tot_18'] = pums['rlow_18']+pums['olow_18']
pums['mod_tot_18'] = pums['rmod_18']+pums['omod_18']
pums['high_tot_18'] = pums['rhigh_18']+pums['ohigh_18']

pums['pct_low_18'] = pums['low_tot_18']/pums['hu_tot_18']
pums['pct_mod_18'] = pums['mod_tot_18']/pums['hu_tot_18']
pums['pct_high_18'] = pums['high_tot_18']/pums['hu_tot_18']

# Classifying tracts by housing afforablde by income
# =====================================================

## Low income
pums['predominantly_LI'] = np.where((pums['pct_low_18']>=0.55)&
                                       (pums['pct_mod_18']<0.45)&
                                       (pums['pct_high_18']<0.45),1,0)

## High income
pums['predominantly_HI'] = np.where((pums['pct_low_18']<0.45)&
                                       (pums['pct_mod_18']<0.45)&
                                       (pums['pct_high_18']>=0.55),1,0)

## Moderate income
pums['predominantly_MI'] = np.where((pums['pct_low_18']<0.45)&
                                       (pums['pct_mod_18']>=0.55)&
                                       (pums['pct_high_18']<0.45),1,0)

## Mixed-Low income
pums['mixed_low'] = np.where((pums['predominantly_LI']==0)&
                              (pums['predominantly_MI']==0)&
                              (pums['predominantly_HI']==0)&
                              (pums['mmhcosts_18']<aff_18*0.6),1,0)

## Mixed-Moderate income
pums['mixed_mod'] = np.where((pums['predominantly_LI']==0)&
                              (pums['predominantly_MI']==0)&
                              (pums['predominantly_HI']==0)&
                              (pums['mmhcosts_18']>=aff_18*0.6)&
                              (pums['mmhcosts_18']<aff_18*1.2),1,0)

## Mixed-High income
pums['mixed_high'] = np.where((pums['predominantly_LI']==0)&
                              (pums['predominantly_MI']==0)&
                              (pums['predominantly_HI']==0)&
                              (pums['mmhcosts_18']>=aff_18*1.2),1,0)

pums['lmh_flag_encoded'] = 0
pums.loc[pums['predominantly_LI']==1, 'lmh_flag_encoded'] = 1
pums.loc[pums['predominantly_MI']==1, 'lmh_flag_encoded'] = 2
pums.loc[pums['predominantly_HI']==1, 'lmh_flag_encoded'] = 3
pums.loc[pums['mixed_low']==1, 'lmh_flag_encoded'] = 4
pums.loc[pums['mixed_mod']==1, 'lmh_flag_encoded'] = 5
pums.loc[pums['mixed_high']==1, 'lmh_flag_encoded'] = 6

pums['lmh_flag_category'] = 0
pums.loc[pums['lmh_flag_encoded']==1, 'lmh_flag_category'] = 'aff_predominantly_LI'
pums.loc[pums['lmh_flag_encoded']==2, 'lmh_flag_category'] = 'aff_predominantly_MI'
pums.loc[pums['lmh_flag_encoded']==3, 'lmh_flag_category'] = 'aff_predominantly_HI'
pums.loc[pums['lmh_flag_encoded']==4, 'lmh_flag_category'] = 'aff_mix_low'
pums.loc[pums['lmh_flag_encoded']==5, 'lmh_flag_category'] = 'aff_mix_mod'
pums.loc[pums['lmh_flag_encoded']==6, 'lmh_flag_category'] = 'aff_mix_high'

# pums.groupby('lmh_flag_category').count()['FIPS']

census = census.merge(pums[['FIPS', 'lmh_flag_encoded', 'lmh_flag_category']], on = 'FIPS')

# len(census)
pums.to_csv(output_path+'databases/pums.csv')
del pums
# =====================================================
# Setting 'Market Types'
# ==========================================================================

census['pctch_real_mhval_00_18'] = (census['real_mhval_18']-census['real_mhval_00'])/census['real_mhval_00']
census['pctch_real_mrent_12_18'] = (census['real_mrent_18']-census['real_mrent_12'])/census['real_mrent_12']
rm_pctch_real_mhval_00_18_increase=np.nanmedian(census['pctch_real_mhval_00_18'][census['pctch_real_mhval_00_18']>0.05])
rm_pctch_real_mrent_12_18_increase=np.nanmedian(census['pctch_real_mrent_12_18'][census['pctch_real_mrent_12_18']>0.05])
census['rent_decrease'] = np.where((census['pctch_real_mrent_12_18']<=-0.05), 1, 0)
census['rent_marginal'] = np.where((census['pctch_real_mrent_12_18']>-0.05)&
                                          (census['pctch_real_mrent_12_18']<0.05), 1, 0)
census['rent_increase'] = np.where((census['pctch_real_mrent_12_18']>=0.05)&
                                          (census['pctch_real_mrent_12_18']<rm_pctch_real_mrent_12_18_increase), 1, 0)
census['rent_rapid_increase'] = np.where((census['pctch_real_mrent_12_18']>=0.05)&
                                          (census['pctch_real_mrent_12_18']>=rm_pctch_real_mrent_12_18_increase), 1, 0)

census['house_decrease'] = np.where((census['pctch_real_mhval_00_18']<=-0.05), 1, 0)
census['house_marginal'] = np.where((census['pctch_real_mhval_00_18']>-0.05)&
                                          (census['pctch_real_mhval_00_18']<0.05), 1, 0)
census['house_increase'] = np.where((census['pctch_real_mhval_00_18']>=0.05)&
                                          (census['pctch_real_mhval_00_18']<rm_pctch_real_mhval_00_18_increase), 1, 0)
census['house_rapid_increase'] = np.where((census['pctch_real_mhval_00_18']>=0.05)&
                                          (census['pctch_real_mhval_00_18']>=rm_pctch_real_mhval_00_18_increase), 1, 0)

census['tot_decrease'] = np.where((census['rent_decrease']==1)|(census['house_decrease']==1), 1, 0)
census['tot_marginal'] = np.where((census['rent_marginal']==1)|(census['house_marginal']==1), 1, 0)
census['tot_increase'] = np.where((census['rent_increase']==1)|(census['house_increase']==1), 1, 0)
census['tot_rapid_increase'] = np.where((census['rent_rapid_increase']==1)|(census['house_rapid_increase']==1), 1, 0)

census['change_flag_encoded'] = 0
census.loc[(census['tot_decrease']==1)|(census['tot_marginal']==1), 'change_flag_encoded'] = 1
census.loc[census['tot_increase']==1, 'change_flag_encoded'] = 2
census.loc[census['tot_rapid_increase']==1, 'change_flag_encoded'] = 3

census['change_flag_category'] = 0
census.loc[census['change_flag_encoded']==1, 'change_flag_category'] = 'ch_decrease_marginal'
census.loc[census['change_flag_encoded']==2, 'change_flag_category'] = 'ch_increase'
census.loc[census['change_flag_encoded']==3, 'change_flag_category'] = 'ch_rapid_increase'

# census.groupby('change_flag_category').count()['FIPS']

# census.groupby(['change_flag_category', 'lmh_flag_category']).count()['FIPS']

# len(census)
# save 'census' to csv
census.to_csv(output_path+'databases/all_cities_2018.csv', index=False)
# census.columns
# =====================================================
# Zillow Data
# =====================================================

# Load Zillow Data
# --------------------------------------------------

# def filter_ZILLOW(df, FIPS):
#     if (city_name not in ('Memphis', 'Boston')):
#         FIPS_pre = [state+county for county in FIPS]
#         df = df[(df['FIPS'].astype(str).str.zfill(11).str[:5].isin(FIPS_pre))].reset_index(drop = True)
#     else:
#         fips_list = []
#         for i in state:
#             county = FIPS[str(i)]
#             FIPS_pre = [str(i)+county for county in county]
#         df = df[(df['FIPS'].astype(str).str.zfill(11).str[:5].isin(FIPS_pre))].reset_index(drop = True)
#     return df

## Import Zillow data
zillow = pd.read_csv(input_path+'Zip_Zhvi_AllHomes.csv', encoding = "ISO-8859-1")
zillow_xwalk = pd.read_csv(input_path+'TRACT_ZIP_032015.csv')

# Calculate Zillow Measures
# --------------------------------------------------

## Compute change over time
zillow['ch_zillow_12_18'] = zillow['2018-01'] - zillow['2012-01']*CPI_12_18
zillow['per_ch_zillow_12_18'] = zillow['ch_zillow_12_18']/zillow['2012-01']
# zillow = zillow[zillow['State'].isin(state_init)].reset_index(drop = True)
zillow = zillow_xwalk[['TRACT', 'ZIP', 'RES_RATIO']].merge(zillow[['RegionName', 'ch_zillow_12_18', 'per_ch_zillow_12_18']], left_on = 'ZIP', right_on = 'RegionName', how = "outer")
zillow = zillow.rename(columns = {'TRACT':'FIPS'})

# Filter only data of interest
# zillow = filter_ZILLOW(zillow, FIPS)

## Keep only data for largest xwalk value, based on residential ratio
zillow = zillow.sort_values(by = ['FIPS', 'RES_RATIO'], ascending = False).groupby('FIPS').first().reset_index(drop = False)

## Compute 90th percentile change in region
percentile_90 = zillow['per_ch_zillow_12_18'].quantile(q = 0.9)
# print(percentile_90)

# Create Flags
# --------------------------------------------------

## Change over 50% of change in region
zillow['ab_50pct_ch'] = np.where(zillow['per_ch_zillow_12_18']>0.5, 1, 0)

## Change over 90th percentile change
zillow['ab_90percentile_ch'] = np.where(zillow['per_ch_zillow_12_18']>percentile_90, 1, 0)
census_zillow = census.merge(zillow[['FIPS', 'per_ch_zillow_12_18', 'ab_50pct_ch', 'ab_90percentile_ch']], on = 'FIPS')
# census_zillow.head()
# census_zillow.info()
# census.info()

## Create 90th percentile for rent
census_zillow['rent_50pct_ch'] = np.where(census_zillow['pctch_real_mrent_12_18']>=0.5, 1, 0)
census_zillow['rent_90percentile_ch'] = np.where(census_zillow['pctch_real_mrent_12_18']>=0.9, 1, 0)

# =====================================================
# Calculate Regional Medians
# =====================================================

# Calculate medians necessary for typology designation
# --------------------------------------------------

rm_per_all_li_90 = np.nanmedian(census_zillow['per_all_li_90'])
rm_per_all_li_00 = np.nanmedian(census_zillow['per_all_li_00'])
rm_per_all_li_18 = np.nanmedian(census_zillow['per_all_li_18'])
rm_per_nonwhite_90 = np.nanmedian(census_zillow['per_nonwhite_90'])
rm_per_nonwhite_00 = np.nanmedian(census_zillow['per_nonwhite_00'])
rm_per_nonwhite_18 = np.nanmedian(census_zillow['per_nonwhite_18'])
rm_per_col_90 = np.nanmedian(census_zillow['per_col_90'])
rm_per_col_00 = np.nanmedian(census_zillow['per_col_00'])
rm_per_col_18 = np.nanmedian(census_zillow['per_col_18'])
rm_per_rent_90= np.nanmedian(census_zillow['per_rent_90'])
rm_per_rent_00= np.nanmedian(census_zillow['per_rent_00'])
rm_per_rent_18= np.nanmedian(census_zillow['per_rent_18'])
rm_real_mrent_90 = np.nanmedian(census_zillow['real_mrent_90'])
rm_real_mrent_00 = np.nanmedian(census_zillow['real_mrent_00'])
rm_real_mrent_12 = np.nanmedian(census_zillow['real_mrent_12'])
rm_real_mrent_18 = np.nanmedian(census_zillow['real_mrent_18'])
rm_real_mhval_90 = np.nanmedian(census_zillow['real_mhval_90'])
rm_real_mhval_00 = np.nanmedian(census_zillow['real_mhval_00'])
rm_real_mhval_18 = np.nanmedian(census_zillow['real_mhval_18'])
rm_real_hinc_90 = np.nanmedian(census_zillow['real_hinc_90'])
rm_real_hinc_00 = np.nanmedian(census_zillow['real_hinc_00'])
rm_real_hinc_18 = np.nanmedian(census_zillow['real_hinc_18'])
rm_per_units_pre50_18 = np.nanmedian(census_zillow['per_units_pre50_18'])
rm_per_ch_zillow_12_18 = np.nanmedian(census_zillow['per_ch_zillow_12_18'])
rm_pctch_real_mrent_12_18 = np.nanmedian(census_zillow['pctch_real_mrent_12_18'])

## Above regional median change home value and rent
census_zillow['hv_abrm_ch'] = np.where(census_zillow['per_ch_zillow_12_18'] > rm_per_ch_zillow_12_18, 1, 0)
census_zillow['rent_abrm_ch'] = np.where(census_zillow['pctch_real_mrent_12_18'] > rm_pctch_real_mrent_12_18, 1, 0)

## Percent changes

census_zillow['pctch_real_mhval_90_00'] = (census_zillow['real_mhval_00']-census_zillow['real_mhval_90'])/census_zillow['real_mhval_90']
census_zillow['pctch_real_mrent_90_00'] = (census_zillow['real_mrent_00']-census_zillow['real_mrent_90'])/census_zillow['real_mrent_90']
census_zillow['pctch_real_hinc_90_00'] = (census_zillow['real_hinc_00']-census_zillow['real_hinc_90'])/census_zillow['real_hinc_90']

census_zillow['pctch_real_mhval_00_18'] = (census_zillow['real_mhval_18']-census_zillow['real_mhval_00'])/census_zillow['real_mhval_00']
census_zillow['pctch_real_mrent_00_18'] = (census_zillow['real_mrent_18']-census_zillow['real_mrent_00'])/census_zillow['real_mrent_00']
census_zillow['pctch_real_mrent_12_18'] = (census_zillow['real_mrent_18']-census_zillow['real_mrent_12'])/census_zillow['real_mrent_12']
census_zillow['pctch_real_hinc_00_18'] = (census_zillow['real_hinc_18']-census_zillow['real_hinc_00'])/census_zillow['real_hinc_00']

## Regional Medians

pctch_rm_real_mhval_90_00 = (rm_real_mhval_00-rm_real_mhval_90)/rm_real_mhval_90
pctch_rm_real_mrent_90_00 = (rm_real_mrent_00-rm_real_mrent_90)/rm_real_mrent_90
pctch_rm_real_mhval_00_18 = (rm_real_mhval_18-rm_real_mhval_00)/rm_real_mhval_00
pctch_rm_real_mrent_00_18 = (rm_real_mrent_18-rm_real_mrent_00)/rm_real_mrent_00
pctch_rm_real_mrent_12_18 = (rm_real_mrent_18-rm_real_mrent_12)/rm_real_mrent_12
pctch_rm_real_hinc_90_00 = (rm_real_hinc_00-rm_real_hinc_90)/rm_real_hinc_90
pctch_rm_real_hinc_00_18 = (rm_real_hinc_18-rm_real_hinc_00)/rm_real_hinc_00

## Absolute changes

census_zillow['ch_all_li_count_90_00'] = census_zillow['all_li_count_00']-census_zillow['all_li_count_90']
census_zillow['ch_all_li_count_00_18'] = census_zillow['all_li_count_18']-census_zillow['all_li_count_00']
census_zillow['ch_per_col_90_00'] = census_zillow['per_col_00']-census_zillow['per_col_90']
census_zillow['ch_per_col_00_18'] = census_zillow['per_col_18']-census_zillow['per_col_00']
census_zillow['ch_per_limove_12_18'] = census_zillow['per_limove_18'] - census_zillow['per_limove_12']

## Regional Medians
ch_rm_per_col_90_00 = rm_per_col_00-rm_per_col_90
ch_rm_per_col_00_18 = rm_per_col_18-rm_per_col_00

# Calculate flags
# --------------------------------------------------

df = census_zillow
df['pop00flag'] = np.where(df['pop_00']>500, 1, 0)
df['aboverm_per_all_li_90'] = np.where(df['per_all_li_90']>=rm_per_all_li_90, 1, 0)
df['aboverm_per_all_li_00'] = np.where(df['per_all_li_00']>=rm_per_all_li_00, 1, 0)
df['aboverm_per_all_li_18'] = np.where(df['per_all_li_18']>=rm_per_all_li_18, 1, 0)
df['aboverm_per_nonwhite_18'] = np.where(df['per_nonwhite_18']>=rm_per_nonwhite_18, 1, 0)
df['aboverm_per_nonwhite_90'] = np.where(df['per_nonwhite_90']>=rm_per_nonwhite_90, 1, 0)
df['aboverm_per_nonwhite_00'] = np.where(df['per_nonwhite_00']>=rm_per_nonwhite_00, 1, 0)
df['aboverm_per_rent_90'] = np.where(df['per_rent_90']>=rm_per_rent_90, 1, 0)
df['aboverm_per_rent_00'] = np.where(df['per_rent_00']>=rm_per_rent_00, 1, 0)
df['aboverm_per_rent_18'] = np.where(df['per_rent_18']>=rm_per_rent_18, 1, 0)
df['aboverm_per_col_90'] = np.where(df['per_col_90']>=rm_per_col_90, 1, 0)
df['aboverm_per_col_00'] = np.where(df['per_col_00']>=rm_per_col_00, 1, 0)
df['aboverm_per_col_18'] = np.where(df['per_col_18']>=rm_per_col_18, 1, 0)
df['aboverm_real_mrent_90'] = np.where(df['real_mrent_90']>=rm_real_mrent_90, 1, 0)
df['aboverm_real_mrent_00'] = np.where(df['real_mrent_00']>=rm_real_mrent_00, 1, 0)
df['aboverm_real_mrent_12'] = np.where(df['real_mrent_12']>=rm_real_mrent_12, 1, 0)
df['aboverm_real_mrent_18'] = np.where(df['real_mrent_18']>=rm_real_mrent_18, 1, 0)
df['aboverm_real_mhval_90'] = np.where(df['real_mhval_90']>=rm_real_mhval_90, 1, 0)
df['aboverm_real_mhval_00'] = np.where(df['real_mhval_00']>=rm_real_mhval_00, 1, 0)
df['aboverm_real_mhval_18'] = np.where(df['real_mhval_18']>=rm_real_mhval_18, 1, 0)
df['aboverm_pctch_real_mhval_00_18'] = np.where(df['pctch_real_mhval_00_18']>=pctch_rm_real_mhval_00_18, 1, 0)
df['aboverm_pctch_real_mrent_00_18'] = np.where(df['pctch_real_mrent_00_18']>=pctch_rm_real_mrent_00_18, 1, 0)
df['aboverm_pctch_real_mrent_12_18'] = np.where(df['pctch_real_mrent_12_18']>=pctch_rm_real_mrent_12_18, 1, 0)
df['aboverm_pctch_real_mhval_90_00'] = np.where(df['pctch_real_mhval_90_00']>=pctch_rm_real_mhval_90_00, 1, 0)
df['aboverm_pctch_real_mrent_90_00'] = np.where(df['pctch_real_mrent_90_00']>=pctch_rm_real_mrent_90_00, 1, 0)
df['lostli_00'] = np.where(df['ch_all_li_count_90_00']<0, 1, 0)
df['lostli_18'] = np.where(df['ch_all_li_count_00_18']<0, 1, 0)
df['aboverm_pctch_real_hinc_90_00'] = np.where(df['pctch_real_hinc_90_00']>pctch_rm_real_hinc_90_00, 1, 0)
df['aboverm_pctch_real_hinc_00_18'] = np.where(df['pctch_real_hinc_00_18']>pctch_rm_real_hinc_00_18, 1, 0)
df['aboverm_ch_per_col_90_00'] = np.where(df['ch_per_col_90_00']>ch_rm_per_col_90_00, 1, 0)
df['aboverm_ch_per_col_00_18'] = np.where(df['ch_per_col_00_18']>ch_rm_per_col_00_18, 1, 0)
df['aboverm_per_units_pre50_18'] = np.where(df['per_units_pre50_18']>rm_per_units_pre50_18, 1, 0)

df.to_csv(output_path+'databases/df.csv')
del df


census_zillow_tract_list = census_zillow['FIPS'].astype(str).str.zfill(11)
city_shp = city_shp[city_shp['GEOID'].isin(census_zillow_tract_list)].reset_index(drop = True)
# Shapefiles
# --------------------------------------------------

## Create single region polygon
city_poly = city_shp.dissolve(by = 'STATEFP')
city_poly = city_poly.reset_index(drop = True)

census_zillow_tract_list.describe()

# =====================================================
# Overlay Variables (Rail + Housing)
# =====================================================

# Rail
# --------------------------------------------------

## Filter only existing rail
rail = rail[rail['Year Opened']=='Pre-2000'].reset_index(drop = True)

## Filter by city
rail = rail[rail['Agency'].isin(rail_agency)].reset_index(drop = True)
rail = gpd.GeoDataFrame(rail, geometry=[Point(xy) for xy in zip (rail['Longitude'], rail['Latitude'])])

## sets coordinate system to WGS84
rail.crs = {'init' :'epsg:4269'}

## creates UTM projection
## zone is defined under define city specific variables
projection = '+proj=utm +zone='+zone+', +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

## project to UTM coordinate system
rail_proj = rail.to_crs(projection)

## create buffer around anchor institution in meters
rail_buffer = rail_proj.buffer(804.672)

## convert buffer back to WGS84
rail_buffer_wgs = rail_buffer.to_crs(epsg=4326)

## crate flag
city_shp['rail'] = np.where(city_shp.intersects(rail_buffer_wgs.unary_union) == True, 1, 0)

# Subsidized Housing
# --------------------------------------------------
## Convert to geodataframe

import multiprocessing
import geopandas as gpd
from shapely.geometry import Point

def process_dataframe(data):
    data = gpd.GeoDataFrame(data, geometry=[Point(xy) for xy in zip(data['X'], data['Y'])])
    return data

if __name__ == '__main__':
    # Assuming `lihtc` and `pub_hous` are your dataframes

    # Create a multiprocessing pool
    pool = multiprocessing.Pool(processes=2)  # Adjust the number of processes as needed

    # Map the processing function to each dataframe
    results = pool.map(process_dataframe, [lihtc, pub_hous])

    # Close the pool to release resources
    pool.close()
    pool.join()

    # Retrieve the results
    lihtc_processed, pub_hous_processed = results

    # Now `lihtc_processed` and `pub_hous_processed` contain the processed dataframes

# lihtc = gpd.GeoDataFrame(lihtc, geometry=[Point(xy) for xy in zip (lihtc['X'], lihtc['Y'])])
# pub_hous = gpd.GeoDataFrame(pub_hous, geometry=[Point(xy) for xy in zip (pub_hous['X'], pub_hous['Y'])])
# lihtc.to_csv(output_path+'databases/lihtc.csv')
# del lihtc
## LIHTC clean
# lihtc = lihtc[lihtc['geometry'].within(city_poly.loc[0, 'geometry'])].reset_index(drop = True)

## Public housing
# pub_hous = pub_hous[pub_hous['geometry'].within(city_poly.loc[0, 'geometry'])].reset_index(drop = True)

## Merge Datasets
# presence_ph_LIHTC = lihtc[['geometry']].append(pub_hous[['geometry']])

## check whether census_zillow tract contains public housing or LIHTC station
## and create public housing flag
# city_shp['presence_ph_LIHTC'] = city_shp.intersects(presence_ph_LIHTC.unary_union)

####
# Begin Map Plot
####
# ax = city_shp.plot(color = 'grey')
# city_shp.plot(ax = ax, column = 'presence_ph_LIHTC')
# presence_ph_LIHTC.plot(ax = ax)
# plt.show()
####
# End Map Plot
####


# ==========================================================================
# Merge Census and Zillow Data
# ==========================================================================

city_shp['GEOID'] = city_shp['GEOID'].astype('int64')

census_zillow = census_zillow.merge(city_shp[['GEOID','geometry','rail', 'presence_ph_LIHTC']], right_on = 'GEOID', left_on = 'FIPS')
# census_zillow.query("FIPS == 13121011100")
# ==========================================================================
# Export Data
# ==========================================================================


census_zillow.to_csv(output_path+'databases/zillow_database_2018.csv', index=False)
pq.write_table(output_path+'_database.parquet')


# save session/variables
#import shelve

#T='Hiya'
#val=[1,2,3]

#filename='/shelve.out'
#my_shelf = shelve.open(filename,'n') # 'n' for new

#for key in dir():
#    try:
#        my_shelf[key] = globals()[key]
#    except TypeError:
#        #
#        # __builtins__, my_shelf, and imported modules can not be shelved.
#        #
#        print('ERROR shelving: {0}'.format(key))
#my_shelf.close()


#import rpy2
#from rpy2 import robjects
#from rpy2.robjects import pandas2ri
#pandas2ri.activate()

# read .RData file as a pandas dataframe
#def load_rdata_file(filename):
#    r_data = robjects.r['get'](robjects.r['load'](filename))
#    df = pandas2ri.ri2py(r_data)
#    return df

# write pandas dataframe to an .RData file
# def save_rdata_file(df, filename):
#    r_data = pandas2ri.py2ri(df)
#    robjects.r.assign("my_df", r_data)
#    robjects.r("save(my_df, file='{}')".format(filename))

# Loading Los Angeles as an example of what it should look like
# los_angeles = pd.read_csv(output_path+'/databases/LosAngeles_database_2018.csv')

# los_angeles.columns

# all(col in census.columns for col in los_angeles.columns)
# all(col in los_angeles.columns for col in census.columns)

# set(los_angeles.columns) - set(census.columns)
