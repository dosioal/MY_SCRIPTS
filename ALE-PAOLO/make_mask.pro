;
PRO make_mask
;
DEVICE,decompose=0
;
;
;-----------------------------------------------------------------------------------------------------------

home='/media/disk/POSTPROC/BC_Regional/'
MODEL_ID='DMI-HIRHAM5'
CASE_ID='A1B_ECHAM5'

;VARIABLE (T,Ptot,Tmax,Tmin)
VAR='Ptot'
VAR='Tmax'
VAR='T'

CONSTRUCTION_PERIOD_START  =  '1961'
CONSTRUCTION_PERIOD_STOP   =  '1990'

APPLICATION_PERIOD_START   =  '1991'
APPLICATION_PERIOD_STOP    =  '2000'

OBS_DIR=home+MODEL_ID+'_'+CASE_ID+'_EOBS_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'/OBS_idl/'
MOD_DIR=home+MODEL_ID+'_'+CASE_ID+'_EOBS_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'/BC_data/'
;MOD_BC_DIR=home+MODEL_ID+'_'+CASE_ID+'_EOBS_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'/BC_data/'
MOD_BC_DIR=home+MODEL_ID+'_'+CASE_ID+'_EOBS_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_1961-2100/BC_data/'

;FAC_DIR=home+MODEL_ID+'_'+CASE_ID+'_EOBS_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'/factors/'

;-----------------------------------------------------------------------------------------------------------

; specify the months to be bias corrected
month=['01','02','03','04','05','06','07','08','09','10','11','12']
;
; construct a land mask file from the given data file
LM_FILE_C='/media/disk/DATA/ENSOBS/ENS_MODELS/read_in_'+MODEL_ID+'_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'.nc'
LM_FILE_A='/media/disk/DATA/ENSOBS/ENS_MODELS/read_in_'+MODEL_ID+'_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'.nc'
lm_var     = 'tg'
mv_cutoff  = 1.0

    file = LM_FILE_C
    theVariable = lm_var
    fileID = NCDF_Open(file)
    varID = NCDF_VarID(fileID, theVariable)
    varInfo = NCDF_VarInq(fileID, varID)
    dimIDs = varInfo.dim
    nDims = N_Elements(dimIDs)
    dims_c = IntArr(nDims)
    FOR j=0,nDims-1 DO BEGIN
        NCDF_DimInq, fileID, dimIDs[j], dname, dsize
        dims_c[j] = dsize
    ENDFOR

    landmask_c=dblarr(dims_c(0),dims_c(1))
    NCDF_VARGET,fileID,lm_var,landmask_c,count=[dims_c(0),dims_c(1),1],offset=[0,0,0]
    we_c=where(landmask_c ge mv_cutoff)
    land_c=reform(we_c(sort(we_c)))+1
    ;land_c=reform(we_c(sort(we_c)))
    lat_c=land_c*0.0
    lon_c=land_c*0.0
    land2d_c=intarr(dims_c(0),dims_c(1))*0.0
    land2d_c(we_c)=1.0   
    lat2d_c=dblarr(dims_c(0),dims_c(1))
    lon2d_c=dblarr(dims_c(0),dims_c(1))
    NCDF_VARGET,fileID,'Actual_latitude',lat2d_c
    NCDF_VARGET,fileID,'Actual_longitude',lon2d_c
    lat2d_c=lat2d_c-273.15
    lon2d_c=lon2d_c-273.15
    
    count=0L
    count1=0L
    FOR ix=0L,dims_c(0)-1 DO BEGIN
        FOR iy=0L,dims_c(1)-1 DO BEGIN
            IF (landmask_c(ix,iy) gt mv_cutoff) THEN BEGIN
                lon_c(count)=ix
                lat_c(count)=iy 
                land_c(count)=iy*dims_c(0)+ix+1
                count=count+1
            ENDIF
            count1=count1+1
        ENDFOR
    ENDFOR

BI=[-10,2,50,59]
IP=[-10,3,36,44]
FR=[-5,5,44,50]
ME=[2,16,48,55]
SC=[5,30,55,75]
AL=[5,15,44,48]
MD=[3,25,36,44]
EA=[16,30,44,55]
EU=[-15,60,35,75]

window,0,retain=2
contour,land2d_c,lon2d_c,lat2d_c
;BI
plots,BI(0),BI(2)
plots,BI(0),BI(3),/continue
plots,BI(1),BI(3),/continue
plots,BI(1),BI(2),/continue
plots,BI(0),BI(2),/continue
;IP
plots,IP(0),IP(2)
plots,IP(0),IP(3),/continue
plots,IP(1),IP(3),/continue
plots,IP(1),IP(2),/continue
plots,IP(0),IP(2),/continue
;FR
plots,FR(0),FR(2)
plots,FR(0),FR(3),/continue
plots,FR(1),FR(3),/continue
plots,FR(1),FR(2),/continue
plots,FR(0),FR(2),/continue
;ME
plots,ME(0),ME(2)
plots,ME(0),ME(3),/continue
plots,ME(1),ME(3),/continue
plots,ME(1),ME(2),/continue
plots,ME(0),ME(2),/continue
;SC
plots,SC(0),SC(2)
plots,SC(0),SC(3),/continue
plots,SC(1),SC(3),/continue
plots,SC(1),SC(2),/continue
plots,SC(0),SC(2),/continue
;AL
plots,AL(0),AL(2)
plots,AL(0),AL(3),/continue
plots,AL(1),AL(3),/continue
plots,AL(1),AL(2),/continue
plots,AL(0),AL(2),/continue
;MD
plots,MD(0),MD(2)
plots,MD(0),MD(3),/continue
plots,MD(1),MD(3),/continue
plots,MD(1),MD(2),/continue
plots,MD(0),MD(2),/continue
;EA
plots,EA(0),EA(2)
plots,EA(0),EA(3),/continue
plots,EA(1),EA(3),/continue
plots,EA(1),EA(2),/continue
plots,EA(0),EA(2),/continue


BI=[20,60,100,150]
IP=[0,45,40,80]
FR=[30,60,60,100]
ME=[60,100,85,115]
SC=[65,145,115,195]
AL=[60,100,65,85]
MD=[60,130,25,65]
EA=[100,145,65,115]
EU=[-15,60,35,75]
window,1,retain=2
contour,land2d_c
;BI
plots,BI(0),BI(2)
plots,BI(0),BI(3),/continue
plots,BI(1),BI(3),/continue
plots,BI(1),BI(2),/continue
plots,BI(0),BI(2),/continue
;IP
plots,IP(0),IP(2)
plots,IP(0),IP(3),/continue
plots,IP(1),IP(3),/continue
plots,IP(1),IP(2),/continue
plots,IP(0),IP(2),/continue
;FR
plots,FR(0),FR(2)
plots,FR(0),FR(3),/continue
plots,FR(1),FR(3),/continue
plots,FR(1),FR(2),/continue
plots,FR(0),FR(2),/continue
;ME
plots,ME(0),ME(2)
plots,ME(0),ME(3),/continue
plots,ME(1),ME(3),/continue
plots,ME(1),ME(2),/continue
plots,ME(0),ME(2),/continue
;SC
plots,SC(0),SC(2)
plots,SC(0),SC(3),/continue
plots,SC(1),SC(3),/continue
plots,SC(1),SC(2),/continue
plots,SC(0),SC(2),/continue
;AL
plots,AL(0),AL(2)
plots,AL(0),AL(3),/continue
plots,AL(1),AL(3),/continue
plots,AL(1),AL(2),/continue
plots,AL(0),AL(2),/continue
;MD
plots,MD(0),MD(2)
plots,MD(0),MD(3),/continue
plots,MD(1),MD(3),/continue
plots,MD(1),MD(2),/continue
plots,MD(0),MD(2),/continue
;EA
plots,EA(0),EA(2)
plots,EA(0),EA(3),/continue
plots,EA(1),EA(3),/continue
plots,EA(1),EA(2),/continue
plots,EA(0),EA(2),/continue


STOP
END
