;;THIS IDL FILE CONTAINS SEVERAL DEFINITIONS REQUIRED FOR THE
;;CONSTRUCTION OF THE MONTHLY BIAS CORRECTION COEFFICIENTS.
;;
;; FILENAME: definitions.pro
;; AUTHORS:  C. PIANI AND J. O. HAERTER
;; DATE:     SEPTEMBER 28, 2009
;; PROJECT:  EU WATCH PROJECT
;; requires: definitions.pro, ncdf2idl.pro, prepareData.pro
;;
;; This files prepares original OBS and model data. The user can
;; specify in definitions.pro which format the original data is
;; in. Possible formats are (FORMAT=0) compressed WFD format as a 1d NETCDF
;; file, (FORMAT=1) latlon NETCDF files on a 1/2 degree grid and (FORMAT=2) idl
;; saved files (.dat) in the correct format for exactly the years of
;; interest. In the case of (2) the user must make sure that the
;; correct time-period is stored and that the variable for
;; precipitation is named 'idldata'. E.g. for 5 January months from 1961
;; to 1965 the user must then supply a file with 5x31=155 timesteps
;; of 67420 space points each, hence idldata(67420,155).
;; ___________________________________________________________
;;
;
;
;
; COMMON INFORMATION FOR CONSTRUCTION OF BC AND ITS APPLICATION
;
IF (CONVERTOBS2IDL_SWITCH EQ 1) THEN BEGIN
; prepare files for the case of compressed WFD format
    IF (P_computeCor EQ 1) THEN BEGIN
        IF (FORMAT_OBS EQ 0) THEN BEGIN
            YEAR_START=CONSTRUCTION_PERIOD_START
            YEAR_STOP=CONSTRUCTION_PERIOD_STOP
            
            inName=pathOBSchunk+filenameOBSchunkCompressed
            outNameBase=pathOBS+OBSroot+construction_period ; +month.dat
            prepareData,0,inName,outNameBase,'',vname_obs,YEAR_START,YEAR_STOP,month,NUMLANDPOINTS,land,multf_obs,yearl_o
        ENDIF
;
; compare files for the case of latlon format
        IF (FORMAT_OBS EQ 1) THEN BEGIN
            YEAR_START=CONSTRUCTION_PERIOD_START
            YEAR_STOP=CONSTRUCTION_PERIOD_STOP
            inName=pathOBSchunk+filenameOBSchunkLatlon
            outNameBase=pathOBS+OBSroot+construction_period ; +month.dat
            prepareData,1,inName,outNameBase,'',vname_obs,YEAR_START,YEAR_STOP,month,NUMLANDPOINTS,land,multf_obs,yearl_o
        ENDIF
;
; compare files for the case of finished idl files (do nothing whatsoever)
        IF (FORMAT_OBS EQ 2) THEN BEGIN
            print,'data is already prepared.'
        ENDIF
    ENDIF
ENDIF
;
IF (CONVERTMODEL2IDL_SWITCH_1 EQ 1) THEN BEGIN
; prepare files for the case of compressed OBS format
    IF (P_computeCor EQ 1) THEN BEGIN
        IF (FORMAT_CORRECTION EQ 0) THEN BEGIN
            YEAR_START=CONSTRUCTION_PERIOD_START
            YEAR_STOP=CONSTRUCTION_PERIOD_STOP
            
            inName=pathMODELchunk+filenameMODELCONSTRUCTIONchunkCompressed
            outNameBase=pathmodel+MODELroot+construction_period ; +month.dat
            prepareData,0,inName,outNameBase,runToCorrect,vname_model,YEAR_START,YEAR_STOP,month,NUMLANDPOINTS,land,multf_model,yearl_m
        ENDIF
;
; compare files for the case of latlon format
        IF (FORMAT_CORRECTION EQ 1) THEN BEGIN
            YEAR_START=CONSTRUCTION_PERIOD_START
            YEAR_STOP=CONSTRUCTION_PERIOD_STOP
            
            inName=pathMODELchunk+filenameMODELCONSTRUCTIONchunkLatlon
            outNameBase=pathmodel+MODELroot+construction_period ; +month.dat
            prepareData,1,inName,outNameBase,runToCorrect,vname_model,YEAR_START,YEAR_STOP,month,NUMLANDPOINTS,land,multf_model,yearl_m
        ENDIF
;
; compare files for the case of finished idl files (do nothing whatsoever)
        IF (FORMAT_CORRECTION EQ 2) THEN BEGIN
            print,'data is already prepared.'
        ENDIF
    ENDIF
ENDIF
;
;
IF (CONVERTMODEL2IDL_SWITCH_2 EQ 1) THEN BEGIN
; prepare files for the case of compressed OBS format
    IF (P_apply EQ 1) THEN BEGIN
        IF (FORMAT_APPLICATION EQ 0) THEN BEGIN
            YEAR_START=APPLICATION_PERIOD_START
            YEAR_STOP=APPLICATION_PERIOD_STOP
            
            inName=pathMODELchunk+filenameMODELAPPLICATIONchunkCompressed_PRSN
            outNameBase=pathmodel+MODELroot_PRSN+application_period ; +month.dat
            prepareData,0,inName,outNameBase,runToCorrect,vname_model_prsn,YEAR_START,YEAR_STOP,month,NUMLANDPOINTS,land,multf_model,yearl_m
            
            inName=pathMODELchunk+filenameMODELAPPLICATIONchunkCompressed
            outNameBase=pathmodel+MODELroot+application_period ; +month.dat
            prepareData,0,inName,outNameBase,runToCorrect,vname_model,YEAR_START,YEAR_STOP,month,NUMLANDPOINTS,land,multf_model,yearl_m
        ENDIF
;
; compare files for the case of latlon format
        IF (FORMAT_APPLICATION EQ 1) THEN BEGIN
            YEAR_START=APPLICATION_PERIOD_START
            YEAR_STOP=APPLICATION_PERIOD_STOP
            
            inName=pathMODELchunk+filenameMODELAPPLICATIONchunkLatlon_PRSN
            outNameBase=pathmodel+MODELroot_PRSN+application_period ; +month.dat
            prepareData,1,inName,outNameBase,runToCorrect,vname_model_prsn,YEAR_START,YEAR_STOP,month,NUMLANDPOINTS,land,multf_model,yearl_m
            
            inName=pathMODELchunk+filenameMODELAPPLICATIONchunkLatlon
            outNameBase=pathmodel+MODELroot+application_period ; +month.dat
            prepareData,1,inName,outNameBase,runToCorrect,vname_model,YEAR_START,YEAR_STOP,month,NUMLANDPOINTS,land,multf_model,yearl_m
        ENDIF
;
; compare files for the case of finished idl files (do nothing whatsoever)
        IF (FORMAT_APPLICATION EQ 2) THEN BEGIN
            print,'data is already prepared.'
        ENDIF
    ENDIF
ENDIF
END
