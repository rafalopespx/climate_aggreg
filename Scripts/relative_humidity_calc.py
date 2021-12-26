from metpy.calc import relative_humidity_from_dewpoint
from metpy.units import units
import xarray as xr

meteoDataDF=(xr.load_dataset(f'../Data/Raw_ERA5Land/')
            .
           #.resample(time='1D').mean('time') 
           .assign(rh=xr.apply_ufunc(
                relative_humidity_from_dewpoint, 
                arrayMeteo['t2m']* units.kelvin, 
                arrayMeteo['d2m']* units.kelvin, 
                    )
                 )
            .to_dataframe()
            .assign(t2m=lambda dd: dd.t2m - 273.15)
            .reset_index()
            .drop(columns=['d2m','sp'])
           )
.reset_index().set_index(['time','longitude','latitude']).to_xarray().to_netcdf(f'../Data/ERA5Land_aggregated/relative_humidity_daily_y2009.nc')
