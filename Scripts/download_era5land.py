# %%
# Reference
# https://cds.climate.copernicus.eu/how-to-api
# using pythonbase as kernel

# %%
#parameters
# path to save output files to
#new_directory = '/Users/yc954/Desktop/Yale/Rotations/Summer/data/cds/hourly' # path to save copernicus data to
new_directory='/Users/rl957/Desktop/repos/climate_aggreg/Data/'
# range of years for downloading files
first_year = 2002
last_year = 2010
dataset = "reanalysis-era5-land"# dataset of interest
# Whole Brazil extention North: 5.27°, West: -73.99°, South: -33.75°, East: -34.79°
coord_nwse = [5.27, -73.99, -33.75,-34.79]# coordinates of interest
myvariable="2m_temperature" # variable of interest

# %%
import os
os.chdir(new_directory)
print(os.getcwd())

# %%
#https://confluence.ecmwf.int/display/CKB/Climate+Data+Store+%28CDS%29+documentation#heading-Efficiencytips
#CDS API Request for hourly data of one variable looping though months and years ( e.g 2m temperature for all months in 2018 and 2019)
    
import cdsapi

client = cdsapi.Client()  # create once, reuse

for year in range(first_year, last_year + 1):
    dataset = "derived-era5-land-daily-statistics"
    request = {
        "variable": ["2m_temperature"],
        "year": str(year),  # <-- fix: cast to string
        "month": ["01","02","03","04","05","06","07","08","09","10","11","12"],
        "day": ["01","02","03","04","05","06","07","08","09","10","11","12",
                "13","14","15","16","17","18","19","20","21","22","23","24",
                "25","26","27","28","29","30","31"],
        "daily_statistic": "daily_mean",
        "time_zone": "utc+00:00",
        "frequency": "1_hourly",
        "area": [5.27, -73.99, -33.75, -34.79]
    }
    target = f"{year}_t2m.grib"
    client.retrieve(dataset, request, target)

