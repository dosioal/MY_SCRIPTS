README FILE FOR BIAS CORRECTION
-------------------------------

 SPONSOR:           EU WATCH PROJECT, WORK BLOCK 3
 WORK BLOCK LEADER: S. HAGEMANN
 AUTHORS:           C. PIANI AND J. O. HAERTER
 DATE:              SEPTEMBER 28, 2009
 EMAIL:             cpiani@ictp.it, jan.haerter@zmaw.de

THE BIAS CORRECTION USES TIME SERIES FROM MODEL AND OBSERVATIONS TO CONSTRUCT A MAPPING FUNCTION FOR THE CUMULATIVE DISTRIBUTIONS OF DAILY PRECIPITATION AND TEMPERATURE. THESE MAPPING FUNCTIONS ARE DESCRIBED BY THE COEFFICIENTS OF A FIT, WHICH IS EITHER A LINEAR FIT OR AN EXPONENTIAL FIT. (REFERENCE: Piani et al., Theoretical and Applied Climatology, 2009, DOI: 10.1007/s00704-009-0134-9)

-------------------------------

System requirements: IDL, cdo (climate data operators: http://www.mpimet.mpg.de/fileadmin/software/cdo/).

Please copy all files contained in the tar file into the same directory, this will be your main working directory.
You should edit the file 'definitions.pro' to make all user specific input.
After all information has been entered, please run './makeLogFile'.  
This will produce a textfile "logfile_for_USER_SPECIFIED_DATA ... .txt" that contains all important paths and filenames the user has entered. 
Before running any subsequent programs, please check this file to make sure that all specifications have been made correctly.

-------------------------------

USER INPUT: 
The following must be specified in 'definitions.pro':

    (1) daily observational data and model data in one of three formats:
      (a) compressed WFD format (67420 land points only are stored)
      (b) regular 1/2 degree lat-lon format (720 x 360 points)
      -----
      (c) idl saved files for each month of interest separately, for exactly the years of interest. This type is discouraged unless the user is familiar with idl. (appendix).

    (2) The format of the data has to be supplied in the definitions.pro file, either the format is (0)=compressed, (1)=latlon, (2)=idl saved format. 

    (3) The directories of all data to be read in definitions.pro.
    (4) In case the format of the data is (b) the user has to specify a single timestep latlon netcdf file that contains '1' at every gridpoint where data should be read, i.e. where the user believes a sufficient time series is available both in model and observational data. Our code allow some missing values etc both in temperature and precipitation, or even inconsistent data, such as negative temperatures. However, if too many points are missing, the code will probably crash.
-------------------------------

SWITCHES IS DEFINITIONS.PRO
In principle, all switches for running different parts of the algorithm can be set to 0 or 1. This is useful as the user may want to carry out only parts of the code for whatever purposes, i.e. testing etc. However, this also means that some configurations of switches can lead to crashes, such as turning on apply_T but not performing compute_T first. If compute_T has been performed some time in the past, apply_T however will be possible even if compute_T is now turned OFF.

-------------------------------

RUN THE CORRECTION:
After all information has been entered, run './run_bias_correction' to carry out the bias correction.

-------------------------------

OUTPUT:
All output can be found in the subdirectory 'output_<runnumber>/'. This subdirectory contains the folder 'factors/' with all BC factors and 'BC_data/' with the corrected data specified by '..BCed..' in the filenames. The folder 'OBS_idl/' is an auxiliary folder which holds the converted OBS. 

-------------------------------

QUESTIONS:
For questions, please send email to cpiani@ictp.it or jan.haerter@zmaw.de .

--------------------------------

SIMPLE TESTING FOR YOUR DATA SET

The BC has been tested for a number of cases and for several data sets. 

However, all data are different and have different problems. To ensure that the BC is working for your data, always perform a trivial test of about a 10-year period and construct and apply the correction to the same period. Then check if your data has been corrected in the right way by comparing mean values of T and P with those of the OBS data. 

To produce netcdf files of the original data for the same period (as a simple way of comparing with the BC data) run the function './convertOrgFiles' in your main working directory. This will output the original model and OBS in the lat-lon netcdf format into the 'OBS_idl/' and 'BC_data/' subdirectories and also compute some mean values.

_______________________________________________________________________________
_______________________________________________________________________________


APPENDIX:

In the case of (c): EXAMPLE: FOR THE PERIOD FROM 1960 TO 1969 (10 YEARS) THE USER MUST PROVIDE THE DATA AS MONTH MM 1960 TO MONTH MM 1969 FOR ALL OTHER MONTHS. THE SAVED FILES MUST BE SAVED IN FILES NAMED: 
[MODELroot]_YYYY_YYYY_MM_[RUNNUMBER].dat

File descriptions:
In the following, 'x' stands for either 'P' or 'T', hence precip or temperature.
Filename                           Usage

main functions:
   definitions.pro                 all user specified information such as path names and file names.
   prepareFiles_T.pro              convert netcdf temperature data to idl readable vectors.
   prepareFiles_P.pro                convert netcdf precipitation data to idl readable vectors.
   calc_x_cor_coeff.pro            computation of the 'x' bias correction coefficients.
   apply_x_cor.pro                 applies the bias correction coefficients to the data of the application period.
   idl2latlon_v1.pro               converts bias corrected idl data back to netcdf format.

auxiliary functions:
   definitions_internal.pro        some internal definitions such as file roots etc.
   functions.pro                   some function necessary for the fit procedures.
   prepareData.pro                 a generic code that extracts netcdf data and stores it in idl format
   ncdf2idl.pro                    the actual procedures used by 'prepareData.pro', an atomic algorithm.
   createNCDF_v1.pro               algorithm to produce lat-lon netcdf files from a given idl array
   create1dNCDF.pro                algorithm to produce compressed OBS netcdf files from a given idl array
   idl2latlon_function.pro         a useful little program that converts a given idl array into a netcdf file.


_______________________________________________________________________________