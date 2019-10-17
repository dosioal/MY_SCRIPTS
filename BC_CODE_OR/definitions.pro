;; THIS IDL FILE CONTAINS SEVERAL DEFINITIONS REQUIRED FOR THE
;; CONSTRUCTION OF THE MONTHLY BIAS CORRECTION COEFFICIENTS AND THEIR APPLICATION.
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
;
; In this file 'AI' means 'additional information' that may help enter
; the right specifics.
;
print,'DEFINITIONS.pro'
;
;______________________________________________________________
; INFORMATION FOR THE PERIODS OF CONSTRUCTION, DERIVATION, AND
; APPLICATION OF THE BIAS CORRECTION.
;
; Please enter year in the format 'YYYY'. The STOP date is 12-31 of
; the STOP year. Example: START=1960 and STOP=1969 means 01-01-1960
; until 12-31-1969.
;
; Specify the period over which BC-parameters are to be calculated.
;                                          ************************************
CONSTRUCTION_PERIOD_START  =  '1961'
CONSTRUCTION_PERIOD_STOP   =  '1990'
;
; If already calculated, pecify the period where correction 
; factors were calculated.
;                                             *********************************
DERIVATION_PERIOD_START    =  '1961'
DERIVATION_PERIOD_STOP     =  '1990'
;
; Specify the period that the correction should be applied to
;                                             *******************************
APPLICATION_PERIOD_START   =  '2091'
APPLICATION_PERIOD_STOP    =  '2100'
;
;_______________________________________________________________
;
; COMMON INFORMATION FOR CONSTRUCTION OF BC AND ITS APPLICATION
;
; Specify which corrections are planned to be performed.
;
; Which type of correction is the user interested in? (1=Yes, 0=No)
; AI: This switch should always be set to '1' if you are planning to run
; any of the subsequent algorithms for 'T' or 'P'.
;
T_correct          =  1    ; temperature
P_correct          =  0    ; precipitation
;
; Does the data need to be converted to IDL format. (1=Yes, 0=No)   *************************
; AI: If you have done this once you don't have to do it again even if the code
; stops after completing it once.
 
T_prepare          =  0     ; prepare files for temperature correction
P_prepare          =  0     ; prepare files for precipitation correction

; Does the user want bias correction coefficients to be computed?
; (1=Yes, 0=No)
; AI: This step has to be performed before the correction coefficients
;are not already calculated and saved in a file.                    **************************        
; to the same of another period.
T_computeCor       =  0     ; temperature
P_computeCor       =  0     ; precipitation
;
; Should the coefficients be applied?  (1=Yes, 0=No)
; AI: This step can be carried out if the previous one has been completed
; for the construction period, either in the same run of in an earlier
; successful run.
T_apply            =  1     ; apply temperature correction
P_apply            =  0    ; apply precipitation correction
;
; Should output data be converted to a NETCDF format?  (1=Yes, 0=No)
; AI: This converts th corrected data to a NETCDF format, this should be
; done after the previous step is complete.
writeoutput        =  1     ; write output files
;
; Define a switch to specify the format of the observational data (obs), the model data for
; the correction period and model data for the application period
; (0->COMPRESSED WFD format, 1->LATLON NETCDF, 2->IDL .dat files by
; month for the correct correction period) 
; AI: If you have been able to prepare the data for one or all of the data
; sets once, you can set the switch to '2' and it will not be done
; again. This can save some time. In case you are not working with
; WATCH forcing data, the option '0' will not apply to you.
FORMAT_OBS         =  1
FORMAT_CORRECTION  =  1
FORMAT_APPLICATION =  1
;
; If the format is 0 or 1 a name is required for the obs variable in the
; netcdf file (the variable name as it appears when using 'cdo infov
; <filename>')
; AI: You can get these variable names by typing 'cdo infov [filename]'.W
vname_obs          =  'rr'    ; total daily precip
vname_obs_T        =  'tg'    ; daily mean temperature
vname_obs_Tmin     =  'tn'    ; daily min temperature
vname_obs_Tmax     =  'tx'    ; daily max temperature
;
; If the format is 0 or 1 a name is required for the model variable in the
; netcdf file of model data.
; AI: You can get these variable names by typing 'cdo infov [filename]'.
vname_model        =  'pr'     ; total daily precip
vname_model_prsn   =  'prsn'    ; total daily snowfall
vname_model_T      =  'tas'     ; daily mean temperature
vname_model_Tmin   =  'tasmin'  ; daily min temperature
vname_model_Tmax   =  'tasmax'  ; daily max temperature
;
; A multiplication factor for the obs and model data to convert to
; units of mm/s (for precip) and Kelvin (for temperature). 
; Note this will not convert from C to K. Since some observational  *************************8
; datasets are in C you will have to convert them prior to use. you can
; use 'cdo addc,273.15 input_file output_file'
multf_obs          =  0.1/86400D ; 24h*60min*60sec
multf_obs_T        =  0.01
multf_model        =  1.0
multf_model_T      =  1.0
;
;______________________________________________________________
; Directory paths and file names  
;______________________________________________________________
;
; pathdat is the general working directory (include final '/')
pathdat               =  '/media/disk/POSTPROC/BC_Regional/'   ; this directory
pathdata              =  '/media/disk/DATA/'
;
; Path for the original model data (include final '/')

;model                 =  'DMI-HIRHAM5'
;scen_con	       = 'A1B_ECHAM5'       ;construction      
;scen_app	       = 'A1B_ECHAM5'       ;application      
;scen_con	       = 'A1B_ARPEGE'       ;construction      
;scen_app	       = 'A1B_ARPEGE'       ;application      
;scen_con	       = 'A1B_BCM'       ;construction      
;scen_app	       = 'A1B_BCM'       ;application      

;model                 =  'C4IRCA3'
;scen_con	       = 'A1B_HadCM3Q16'       ;construction      
;scen_app	       = 'A1B_HadCM3Q16'       ;application      

;model                 =  'ETHZ-CLM'
;scen_con	       = 'SCN_HadCM3Q0'       ;construction      
;scen_app	       = 'SCN_HadCM3Q0'       ;application      

;model                 = 'KNMI-RACMO2'
;scen_con              = 'A1B_ECHAM5-r3'
;scen_app              = 'A1B_ECHAM5-r3'

;model                 = 'MPI-M-REMO'
;scen_con              = 'SCN_ECHAM5'
;scen_app              = 'SCN_ECHAM5'

model                 = 'SMHIRCA'
;scen_con              = 'A1B_BCM'
;scen_app              = 'A1B_BCM'
;scen_con              = 'A1B_ECHAM5-r3'
;scen_app              = 'A1B_ECHAM5-r3'
scen_con              = 'A1B_HadCM3Q3'
scen_app              = 'A1B_HadCM3Q3'


pathmodelorg          =  pathdata+'ENSEMBLES_FLOODS/'+model+'/'+scen_con+'/'
;
; Year lenght (some models have 260 days/yeear)
yearl_o=365 ;OBS
yearl_m=360 ;MOD
;
; Directory of outputs = pathdat/BC_runnumber
;BC_runnumber=model+'_'+scen_app+'_EOBS_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP
BC_runnumber=model+'_'+scen_app+'_EOBS_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_1961-2100'
; If you use this on ebelow, you have to copy the /factor dir from the CONSTRUCTION case to the new APPLICATION one
;BC_runnumber=model+'_'+scen_app+'_EOBS_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP
;
; Path for the original OBS data (include final '/')
pathOBSorg            =  pathdata+'ENSOBS/ENS_MODELS/'
;
; Filename for the OBS in NETCDF format (a file of ~3-10GB for 40 yrs)
filename_OBS_Ptot     =  'rr_0.22deg_rot_v3.0_'+model+'_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'.nc'
filename_OBS_T        =  'tg_0.22deg_rot_v3.0_'+model+'_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_K.nc'
filename_OBS_Tmin     =  'tn_0.22deg_rot_v3.0_'+model+'_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_K.nc'
filename_OBS_Tmax     =  'tx_0.22deg_rot_v3.0_'+model+'_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_K.nc'
;filename_OBS_T        =  'tg_0.22deg_rot_v3.0_mpibox_K.nc'
;filename_OBS_Tmin     =  'tn_0.22deg_rot_v3.0_mpibox_K.nc'
;filename_OBS_Tmax     =  'tx_0.22deg_rot_v3.0_mpibox_K.nc'
;
; Construction filename for the model data in NETCDF format (a file of ~3GB for 40 yrs)
filename_model_const_Ptot   =  model+'_'+scen_con+'_DM_25km_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_pr.nc' 
filename_model_const_Psnow  =  model+'_'+scen_con+'_DM_25km_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_prsn.nc'
filename_model_const_T      =  model+'_'+scen_con+'_DM_25km_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_tas.nc'
filename_model_const_Tmin   =  model+'_'+scen_con+'_DM_25km_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_tasmin.nc'
filename_model_const_Tmax   =  model+'_'+scen_con+'_DM_25km_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_tasmax.nc'
;
; Application filename for the model data in NETCDF format (a file of ~3GB for 40 yrs)
filename_model_appl_Ptot    =  model+'_'+scen_app+'_DM_25km_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'_pr.nc' 
filename_model_appl_Psnow   =  model+'_'+scen_app+'_DM_25km_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'_prsn.nc'
filename_model_appl_T       =  model+'_'+scen_app+'_DM_25km_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'_tas.nc'
filename_model_appl_Tmin    =  model+'_'+scen_app+'_DM_25km_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'_tasmin.nc'
filename_model_appl_Tmax    =  model+'_'+scen_app+'_DM_25km_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'_tasmax.nc'
;
; File to construct the mask with valid points.
; AI: We recommend to use the cdo's to determine gridboxes in model
; and observed data where both data sets have a sufficient number of
; non-missing values. By sufficient we mean at least 90% non-missing
; values, however, of course in the ideal case there would be no
; missing values or the missing values are set to some plausible
; constant, such as precip set to zero. The code is able to work with
; a small degree of imperfection but the correction will be worse
; since assumptions have to be made about the missing values. Mostly,
; they will be dropped from the statistics. To construct the file cdo
; commands such as 'cdo setmisstoc,0 file1 file2', 'cdo gtc,1 file2
; file3', 'cdo sum file3 file4', 'cdo gtc,0.9*totalTimeSteps file4
; file5' may be useful. These commands will produce a file that has a
; '1' wherever there are more than 90% non-missing values.
lm_file    = pathOBSorg+'read_in_'+model+'_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'.nc'
lm_var     = 'tg'
mv_cutoff  = 1.0 ; values below which entries are considered missing values 
;
;_______________________________________________________________
;
; Specify a data format for the final output: 0=NO, 1=YES, Both on or
; both off are also possible.
OUTPUT_DATAFORMAT_1D = 0  ; you get a compressed WFD format 
OUTPUT_DATAFORMAT_2D = 1  ; you get a regular lon x lat grid 
END
;
; INFORMATION COMPLETE?
; Thank you for entering all information! Please save this file. Then
; check your information by running './makeLogFile' and opening the
; logfile "logfile_for_USER_SPECIFIED_DATA_'+<BC_runnumber>+'.txt".
; 
; You should then be ready to run the bias correction by executing
; './run_bias_correction' in your main working directory.
;_______________________________________________________________
;_______________________________________________________________
