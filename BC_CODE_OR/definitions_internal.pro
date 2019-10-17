;;THIS IDL FILE CONTAINS SEVERAL DEFINITIONS REQUIRED FOR THE
;;CONSTRUCTION OF THE MONTHLY BIAS CORRECTION COEFFICIENTS.
;;
;; WORK BLOCK LEADER: S. HAGEMANN
;;
;;
;; FILENAME: definitions.pro
;; AUTHORS:  C. PIANI AND J. O. HAERTER
;; DATE:     NOVEMBER 17, 2009
;; PROJECT:  EU WATCH PROJECT
;;
;; ___________________________________________________________
;;
;
;
print,'DEFINITION_INTERNAL.pro'
close,2
; produce a control file to track progress, name can be changed.
LOGFILENAME='logfile_for_USER_SPECIFIED_DATA_'+BC_runnumber+'.txt'
openw,2,LOGFILENAME
;
; specify the months to be bias corrected
month=['01','02','03','04','05','06','07','08','09','10','11','12']
;
; measurement cutoff for precipitation measurements (mm)
; This is now set to the case of the observational data (OBS) where
; measurements less than 1.0mm are assumed to be noise.
precip_cutoff = 1.0
;______________________________________________________________
;______________________________________________________________
; INFORMATION FOR THE APPLICATION OF THE BC FACTORS
;
; define a switch to specify whether the months of the construction
; period have to be extracted
; from a large 10-year(+2months) file or if separate files already
; exist (0->no processing, 1->processing)
SWITCH_EXTRACT_MONTHS_2=1
;
; should the original model data be converted to an idl format first?
; (0 -> don't convert, 1 -> convert)
CONVERTMODEL2IDL_SWITCH_2=1
;
construction_period = CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP
derivation_period   = DERIVATION_PERIOD_START+'_'+DERIVATION_PERIOD_STOP
application_period  = APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP
;
IF (FORMAT_OBS NE 2) THEN CONVERTOBS2IDL_SWITCH=1 ELSE CONVERTOBS2IDL_SWITCH=0
;
; should original data be converted to ncdf for comparison?
CONVERT_ORG_DATA=1
;
;
; the multfactors for Tmin and Tmax are assumed to be the same as for
; T mean.
multf_obs_Tmin   = multf_obs_T
multf_obs_Tmax   = multf_obs_T
;
multf_model_Tmin = multf_model_T
multf_model_Tmax = multf_model_T
;
; pathmodel is the path to the model data
; model data should be provided in the same format as the OBS, hence
; in a format where only land points are stored in a 1D array of NUMLANDPOINTS
; data points.
pathmodel     =  pathdat+BC_runnumber+'/'+'BC_data/'
;
; pathOBS is the path to the observational data.
pathOBS       =  pathdat+BC_runnumber+'/'+'OBS_idl/'
;
; pathFinalData is the path to the final corrected output data
pathFinalData =  pathdat+BC_runnumber+'/'+'finalOutputData/'
;
; filename for the chunk in latlon format (this would likely be a huge
; file of more than 10GB for 40 yrs!)
filenameOBSchunkCompressed      =  filename_OBS_Ptot
filenameOBSchunkCompressed_T    =  filename_OBS_T
filenameOBSchunkCompressed_Tmin =  filename_OBS_Tmin
filenameOBSchunkCompressed_Tmax =  filename_OBS_Tmax
;
filenameOBSchunkLatlon          =  filename_OBS_Ptot
filenameOBSchunkLatlon_T        =  filename_OBS_T
filenameOBSchunkLatlon_Tmin     =  filename_OBS_Tmin
filenameOBSchunkLatlon_Tmax     =  filename_OBS_Tmax
;
; path for the MODEL DATA chunks
pathMODELchunk=pathmodelorg ;pathdat+'modelData/modelData/'
pathOBSchunk=pathOBSorg 
;
; Specify the run number if there are different versions of the model
; run, i.e. different initial conditions.
runToCorrect=''
;
run=runToCorrect
;
; construction filename for the chunk in compressed format (a file of ~3GB for 40 yrs)
filenameMODELCONSTRUCTIONchunkCompressed      = filename_model_const_Ptot
filenameMODELCONSTRUCTIONchunkCompressed_PRSN = filename_model_const_Psnow
filenameMODELCONSTRUCTIONchunkCompressed_T    = filename_model_const_T
filenameMODELCONSTRUCTIONchunkCompressed_Tmin = filename_model_const_Tmin
filenameMODELCONSTRUCTIONchunkCompressed_Tmax = filename_model_const_Tmax
;
; construction filename for the chunk in latlon format (this would likely be a huge
; file of more than 10GB for 40 yrs!)
filenameMODELCONSTRUCTIONchunkLatlon          = filename_model_const_Ptot
filenameMODELCONSTRUCTIONchunkLatlon_PRSN     = filename_model_const_Psnow
filenameMODELCONSTRUCTIONchunkLatlon_T        = filename_model_const_T
filenameMODELCONSTRUCTIONchunkLatlon_Tmin     = filename_model_const_Tmin
filenameMODELCONSTRUCTIONchunkLatlon_Tmax     = filename_model_const_Tmax
;
; application filename for the chunk in compressed format (a file of ~3GB for 40 yrs)
filenameMODELAPPLICATIONchunkCompressed      = filename_model_appl_Ptot
filenameMODELAPPLICATIONchunkCompressed_PRSN = filename_model_appl_Psnow
filenameMODELAPPLICATIONchunkCompressed_T    = filename_model_appl_T
filenameMODELAPPLICATIONchunkCompressed_Tmin = filename_model_appl_Tmin
filenameMODELAPPLICATIONchunkCompressed_Tmax = filename_model_appl_Tmax
;
; application filename for the chunk in latlon format (this would likely be a huge
; file of more than 10GB for 40 yrs!)
filenameMODELAPPLICATIONchunkLatlon          = filename_model_appl_Ptot
filenameMODELAPPLICATIONchunkLatlon_PRSN     = filename_model_appl_Psnow
filenameMODELAPPLICATIONchunkLatlon_T        = filename_model_appl_T
filenameMODELAPPLICATIONchunkLatlon_Tmin     = filename_model_appl_Tmin
filenameMODELAPPLICATIONchunkLatlon_Tmax     = filename_model_appl_Tmax
;
; the root of the file names used for the OBS.
OBSroot        =  'obs_Ptot_'
OBSroot_T      =  'obs_T_'
OBSroot_Tmin   =  'obs_Tmin_'
OBSroot_Tmax   =  'obs_Tmax_'
;
; the root of the file names used for the MODEL data.
MODELroot      =  'mod_Ptot_'
MODELroot_PRSN =  'mod_Psno_'
MODELroot_T    =  'mod_T_'
MODELroot_Tmin =  'mod_Tmin_'
MODELroot_Tmax =  'mod_Tmax_'
;
; should the original model data be converted to an idl format first?
; (0 -> don't convert, 1 -> convert)
IF (FORMAT_CORRECTION NE 2) THEN CONVERTMODEL2IDL_SWITCH_1=1 ELSE CONVERTMODEL2IDL_SWITCH_1=0
IF (FORMAT_APPLICATION NE 2) THEN CONVERTMODEL2IDL_SWITCH_2=1 ELSE CONVERTMODEL2IDL_SWITCH_2=0
;
; the program assumes that an output directory exists at the location
; outputdir. This directory is used to store the bias correction factors.
outputdir=pathdat+BC_runnumber+'/'+'factors/'
spawn,'mkdir -p '+outputdir
spawn,'mkdir -p '+pathOBS
spawn,'mkdir -p '+pathmodel
spawn,'mkdir -p '+pathFinalData
;
; construct a land mask file from the given data file
    file = lm_file
    theVariable = lm_var
    fileID = NCDF_Open(file)
    varID = NCDF_VarID(fileID, theVariable)
    
    varInfo = NCDF_VarInq(fileID, varID)
    dimIDs = varInfo.dim
    nDims = N_Elements(dimIDs)
    dims = IntArr(nDims)
    FOR j=0,nDims-1 DO BEGIN
        NCDF_DimInq, fileID, dimIDs[j], dname, dsize
        dims[j] = dsize
    ENDFOR
    landmask=dblarr(dims(0),dims(1))
    NCDF_VARGET,fileID,lm_var,landmask,count=[dims(0),dims(1),1],offset=[0,0,0]
    we=where(landmask ge mv_cutoff)
    land=reform(we(sort(we)))+1
    lat=land*0.0
    lon=land*0.0
    land2d=intarr(dims(0),dims(1))*0.0
    land2d(we)=1.0   
    
    count=0L
    count1=0L
    FOR ix=0L,dims(0)-1 DO BEGIN
        FOR iy=0L,dims(1)-1 DO BEGIN
            IF (landmask(ix,iy) gt mv_cutoff) THEN BEGIN
                lat(count)=iy 
                lon(count)=ix
                land(count)=iy*dims(0)+ix+1
                count=count+1
            ENDIF
            count1=count1+1
        ENDFOR
    ENDFOR
    contour,land2d
;
NUMLANDPOINTS=n_elements(land)
;
; Get the original OBS information on the lat-lon coordinates
; The following is only relevant if the user is working with the WATCH
; forcing data compressed format. 
print,'Opening: ',pathdat+'WFDcoords.nc'
id1=NCDF_OPEN(pathdat+'WFDcoords.nc')
NCDF_VARGET,id1,'nav_lon',navlon
NCDF_VARGET,id1,'nav_lat',navlat
nxx=n_elements(navlat(*,1))
nyy=n_elements(navlat(1,*))
NCDF_CLOSE,id1
print,'nxx= ',nxx
print,'nyy= ',nyy
;
;
; Some constants that the user may want to modify:
; (1) In the precip correction an upper limit for the value of tau is
; defined. Tau is the decay coefficient in the precip fit, in case the
; exponential is used. Basically, this value defines the range of
; daily precip in mm where the transition from an exponential fit
; (lower values of P) to a linear fit (higher values) is made.
; This value is used in calc_P_coeff.pro and apply_P_cor.pro.
  Tau_cutoff = 50.0  
;
;______________________________________________________________________________
printf,2,' STATISTICAL BIAS CORRECTION PROCEDURE'
printf,2,'***************************************'
printf,2,''
printf,2,' EU WATCH PROJECT'
printf,2,''
printf,2,' WORK BLOCK LEADER: S. HAGEMANN'
printf,2,''
printf,2,' FILENAME: ',LOGFILENAME
printf,2,' AUTHORS:  C. PIANI AND J. O. HAERTER'
printf,2,' DATE:     DECEMBER 08, 2009'
printf,2,''
printf,2,'***************************************'
;;
;; ___________________________________________________________
;;
;
;
printf,2,''
printf,2,'This file contains information on the user specified data for the bias statistical correction.'
printf,2,''
printf,2,'(*) GENERAL INFORMATION:'
printf,2,''
printf,2,'Tasks to complete:'
IF (T_CORRECT EQ 1) THEN printf,2,'The user is performing a temperature bias correction.'
IF (P_CORRECT EQ 1) THEN printf,2,'The user is performing a precipitation bias correction.'
IF (T_prepare EQ 1) THEN printf,2,'- T input data should be prepared.' 
IF (P_prepare EQ 1) THEN printf,2,'- P input data should be prepared.' 
IF (T_computeCor EQ 1) THEN printf,2,'- T correction factors should be computed.'
IF (P_computeCor EQ 1) THEN printf,2,'- P correction factors should be computed.'
IF (T_apply EQ 1) THEN printf,2,'- T cor. factors should be applied.'
IF (P_apply EQ 1) THEN printf,2,'- P cor. factors should be applied.'
IF (writeoutput EQ 1) THEN printf,2,'- Output should be written as NETCDF.'
;
printf,2,''
printf,2,'General file and data path: '+pathdat
printf,2,''
printf,2,'(*) OBSERVATIONAL DATA FILES:'
IF (FORMAT_OBS EQ 0) THEN BEGIN 
    printf,2,'TOTAL PRECIP:     '+pathOBSchunk+filenameOBSchunkCompressed
    printf,2,'MEAN TEMPERATURE: '+pathOBSchunk+filenameOBSchunkCompressed_T
    printf,2,'MIN TEMPERATURE:  '+pathOBSchunk+filenameOBSchunkCompressed_Tmin
    printf,2,'MAX TEMPERATURE:  '+pathOBSchunk+filenameOBSchunkCompressed_Tmax
ENDIF
IF (FORMAT_OBS EQ 1) THEN BEGIN
    printf,2,'TOTAL PRECIP:     '+pathOBSchunk+filenameOBSchunkLatlon
    printf,2,'MEAN TEMPERATURE: '+pathOBSchunk+filenameOBSchunkLatlon_T
    printf,2,'MIN TEMPERATURE:  '+pathOBSchunk+filenameOBSchunkLatlon_Tmin
    printf,2,'MAX TEMPERATURE:  '+pathOBSchunk+filenameOBSchunkLatlon_Tmax
ENDIF
IF (FORMAT_OBS EQ 2) THEN printf,2,'PATH: '+pathOBS
printf,2,'FORMAT OBS: ',FORMAT_OBS,' (0=compressed, 1=latlon, 2=idl saved)'
printf,2,''
IF (FORMAT_CORRECTION EQ 0) THEN BEGIN
    printf,2,'(*) MODEL DATA FILES (CONSTRUCTION):'
    printf,2,'TOTAL PRECIP:     '+pathmodelorg+filenameMODELCONSTRUCTIONchunkCompressed
    printf,2,'MEAN TEMPERATURE: '+pathmodelorg+filenameMODELCONSTRUCTIONchunkCompressed_T
    printf,2,'MIN TEMPERATURE:  '+pathmodelorg+filenameMODELCONSTRUCTIONchunkCompressed_Tmin
    printf,2,'MAX TEMPERATURE:  '+pathmodelorg+filenameMODELCONSTRUCTIONchunkCompressed_Tmax
ENDIF
IF (FORMAT_CORRECTION EQ 1) THEN BEGIN
    printf,2,'(*) MODEL DATA FILES (CONSTRUCTION):'
    printf,2,'TOTAL PRECIP:     '+pathmodelorg+filenameMODELCONSTRUCTIONchunkLatlon
    printf,2,'MEAN TEMPERUTURE: '+pathmodelorg+filenameMODELCONSTRUCTIONchunkLatlon_T
    printf,2,'MIN TEMPERATURE:  '+pathmodelorg+filenameMODELCONSTRUCTIONchunkLatlon_Tmin
    printf,2,'MAX TEMPERATURE:  '+pathmodelorg+filenameMODELCONSTRUCTIONchunkLatlon_Tmax
ENDIF
IF (FORMAT_CORRECTION EQ 2) THEN printf,2,'PATH: '+pathmodel
;
printf,2,''
IF (FORMAT_APPLICATION EQ 0) THEN BEGIN
    printf,2,'(*) MODEL DATA FILES (APPLICATION):'
    printf,2,'TOTAL PRECIP:     '+pathmodelorg+filenameMODELAPPLICATIONchunkCompressed
    printf,2,'PRECIP SNOW:      '+pathmodelorg+filenameMODELAPPLICATIONchunkCompressed_PRSN
    printf,2,'MEAN TEMPERATURe: '+pathmodelorg+filenameMODELAPPLICATIONchunkCompressed_T
    printf,2,'MIN TEMPERATURE:  '+pathmodelorg+filenameMODELAPPLICATIONchunkCompressed_Tmin
    printf,2,'MAX TEMPERATURE:  '+pathmodelorg+filenameMODELAPPLICATIONchunkCompressed_Tmax
ENDIF
IF (FORMAT_APPLICATION EQ 1) THEN BEGIN 
    printf,2,'(*) MODEL DATA FILES (APPLICATION):'
    printf,2,'TOTAL PRECIP:     '+pathmodelorg+filenameMODELAPPLICATIONchunkLatlon
    printf,2,'PRECIP SNOW:      '+pathmodelorg+filenameMODELAPPLICATIONchunkLatlon_PRSN
    printf,2,'MEAN TEMPERATURE: '+pathmodelorg+filenameMODELAPPLICATIONchunkLatlon_T
    printf,2,'MIN TEMPERATURE:  '+pathmodelorg+filenameMODELAPPLICATIONchunkLatlon_Tmin
    printf,2,'MAX TEMPERATURE:  '+pathmodelorg+filenameMODELAPPLICATIONchunkLatlon_Tmax
ENDIF
IF (FORMAT_APPLICATION EQ 2) THEN printf,2,'PATH: '+pathmodel
printf,2,''
printf,2,'FORMAT MODEL CORRECTION:  ',FORMAT_CORRECTION,' (0=compressed, 1=latlon, 2=idl saved)'
printf,2,'FORMAT MODEL APPLICATION: ',FORMAT_APPLICATION,' (0=compressed, 1=latlon, 2=idl saved)'
;
;
printf,2,''
printf,2,'(*) CORRECTION MASK'
printf,2,'Correction Mask File:               '+lm_file
printf,2,'Dimensions of correction mask file: ',dims
printf,2,'Number of Points to Correct:        ',NUMLANDPOINTS
printf,2,''
;
printf,2,'(*) TIME PERIODS'
printf,2,'CONSTRUCTION PERIOD: '+CONSTRUCTION_PERIOD_START+' - ' + CONSTRUCTION_PERIOD_STOP
printf,2,'DERIVATION PERIOD:   '+DERIVATION_PERIOD_START+  ' - ' + DERIVATION_PERIOD_STOP
printf,2,'APPLICATION PERIOD:  '+APPLICATION_PERIOD_START+ ' - ' + APPLICATION_PERIOD_STOP
printf,2,''
printf,2,'(*) OUTPUT'
printf,2,'Output directory for correction coefficients: '+outputdir
IF (OUTPUT_DATAFORMAT_2D EQ 1) THEN printf,2,'Output to lat lon NETCDF files in directory: '+ pathmodel
IF (OUTPUT_DATAFORMAT_1D EQ 1) THEN printf,2,'Output to compressed NETCDF files in directory: '+ pathmodel
IF (OUTPUT_DATAFORMAT_1D EQ 0 AND OUTPUT_DATAFORMAT_2D EQ 0) THEN printf,2,'No netcdf output to be written. Only idl saved files will be available.'
IF (OUTPUT_DATAFORMAT_1D EQ 0 OR OUTPUT_DATAFORMAT_2D EQ 0) THEN printf,2,'Final output for bias corrected time series written to: '+pathFinalData+' .'
printf,2,''
printf,2,'-----------------------------------------------------------------'
printf,2,''
close,2
;______________________________________________________________
;
END
