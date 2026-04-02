(weatherDataHowToCopernicusERA5)=

# How To use Copernicus ERA5 weather data in SIMONA

To use weather data from the past within SIMONA we recommend to use the dataset [ERA5 hourly data on single levels from 1940 to present](https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels?tab=download) of [Copernicus Climate Data Store](https://cds.climate.copernicus.eu/).

The following parameter should be used:

- Product type: Reanalysis
- Variables:
  - Temperature and pressure
    - 2m temperature
  - Wind
    - 100m u-component of wind
    - 100m v-component of wind
  - Radiation and heat
    - Total sky direct solar radiation at surface (FDIR)
    - Surface solar radiation downwards (SSRD)
- Data format
  - GRIB or NetCDF4

If exporting in NetCDF4 does not work because of insufficient tokens, choose GRIB instead.

For data conversion, our conversion tool [copernicusWeather2psdmWeather](https://github.com/ie3-institute/copernicusWeather2psdmWeather) can be used.
Both conversions from NetCDF4 and GRIB are supported.

## Pre-Processing solar radiation weather data

Since SIMONAs [PV Model](pv_model) requires direct and diffuse solar radiation, the diffuse solar radiation need to be determined from the ERA5 data.
Diffuse solar radiation (FDIFF) at surface can be calculated by

$$
 FDIFF = SSRD - FDIR
$$

*with*\
**SSRD** = Surface solar radiation downwards\
**FDIR** = Total sky direct solar radiation at surface


**References:**
* {cite:cts}`Radiation_ECMWF`
