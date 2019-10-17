;
PRO make_com_grid
;
DEVICE,decompose=0
;
;
;-----------------------------------------------------------------------------------------------------------

home='/media/disk/POSTPROC/BC_Regional/'
;MODEL_ID='DMI-HIRHAM5'
;CASE_ID='A1B_ECHAM5'
;CASE_ID='A1B_ARPEGE'

;MODEL_ID='ETHZ-CLM'
;CASE_ID='SCN_HadCM3Q0'

MODEL_ID='KNMI-RACMO2'
CASE_ID='A1B_ECHAM5-r3'


;VARIABLE (T,Ptot,Tmax,Tmin)
;VAR='Ptot'
VAR='T'
;VAR='Tmin'
;VAR='Tmax'

CONSTRUCTION_PERIOD_START  =  '1961'
CONSTRUCTION_PERIOD_STOP   =  '1990'

APPLICATION_PERIOD_START   =  '1991'
APPLICATION_PERIOD_STOP    =  '2000'

NYEARS=fix(APPLICATION_PERIOD_STOP)-fix(APPLICATION_PERIOD_START)+1

OBS_DIR=home+MODEL_ID+'_'+CASE_ID+'_EOBS_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'/OBS_idl/'
MOD_DIR=home+MODEL_ID+'_'+CASE_ID+'_EOBS_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'/BC_data/'
IF CONSTRUCTION_PERIOD_START EQ '1961' AND CONSTRUCTION_PERIOD_STOP EQ '1990' THEN BEGIN 
MOD_BC_DIR=home+MODEL_ID+'_'+CASE_ID+'_EOBS_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'/BC_data/'
MOD_BC_DIR=home+MODEL_ID+'_'+CASE_ID+'_EOBS_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_1961-2100/BC_data/'
ENDIF ELSE BEGIN
MOD_BC_DIR=home+MODEL_ID+'_'+CASE_ID+'_EOBS_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'/BC_data/'
ENDELSE

;-----------------------------------------------------------------------------------------------------------

; specify the months to be bias corrected
month=['01','02','03','04','05','06','07','08','09','10','11','12']
NDAYS=fltarr(12)
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
    land2d_c=intarr(dims_c(0),dims_c(1))*0.0
    land2d_c(we_c)=1.0   
    
    count=0L
    count1=0L
    FOR ix=0L,dims_c(0)-1 DO BEGIN
        FOR iy=0L,dims_c(1)-1 DO BEGIN
            IF (landmask_c(ix,iy) gt mv_cutoff) THEN BEGIN
                land_c(count)=iy*dims_c(0)+ix+1
                count=count+1
            ENDIF
            count1=count1+1
        ENDFOR
    ENDFOR
						;
    file = LM_FILE_A
    theVariable = lm_var
    fileID = NCDF_Open(file)
    varID = NCDF_VarID(fileID, theVariable)
    varInfo = NCDF_VarInq(fileID, varID)
    dimIDs = varInfo.dim
    nDims = N_Elements(dimIDs)
    dims_a = IntArr(nDims)
    FOR j=0,nDims-1 DO BEGIN
        NCDF_DimInq, fileID, dimIDs[j], dname, dsize
        dims_a[j] = dsize
    ENDFOR

    landmask_a=dblarr(dims_a(0),dims_a(1))
    NCDF_VARGET,fileID,lm_var,landmask_a,count=[dims_a(0),dims_a(1),1],offset=[0,0,0]
    we_a=where(landmask_a ge mv_cutoff)
    land_a=reform(we_a(sort(we_a)))+1
    ;land_a=reform(we_a(sort(we_a)))
    land2d_a=intarr(dims_a(0),dims_a(1))*0.0
    land2d_a(we_a)=1.0   
    
    count=0L
    count1=0L
    FOR ix=0L,dims_a(0)-1 DO BEGIN
        FOR iy=0L,dims_a(1)-1 DO BEGIN
            IF (landmask_a(ix,iy) gt mv_cutoff) THEN BEGIN
                land_a(count)=iy*dims_a(0)+ix+1
                count=count+1
            ENDIF
            count1=count1+1
        ENDFOR
    ENDFOR

NUMLANDPOINTS_c=n_elements(land_c)
NUMLANDPOINTS_a=n_elements(land_a)

setdifference,land_a,land_c,land_diff
IF N_ELEMENTS(land_diff) GT 1 THEN BEGIN
setdifference,land_a,land_diff,land_com
ENDIF ELSE BEGIN
land_com=land_a
ENDELSE
help,land_com

help,NUMLANDPOINTS_c,NUMLANDPOINTS_a

    ;we_m=where(land_com gt 0)
    land_m=reform(land_com(sort(land_com)))-1
    lat_m=land_com*0.0
    lon_m=land_com*0.0
    land2d_m=intarr(dims_a(0),dims_a(1))*0.0
    land2d_m(land_m)=1.0   
    count=0L
    count1=0L
    lat_m=land_m*0.0
    lon_m=land_m*0.0
    FOR ix=0L,dims_a(0)-1 DO BEGIN
        FOR iy=0L,dims_a(1)-1 DO BEGIN
            IF (land2d_m(ix,iy) gt 0) THEN BEGIN
                lon_m(count)=ix
                lat_m(count)=iy 
                ;land_m(count)=iy*dims_a(0)+ix+1
                count=count+1
            ENDIF
            count1=count1+1
        ENDFOR
    ENDFOR

NUMLANDPOINTS=N_ELEMENTS(land_m)

NPOINTS=1.*dims_a(0)*dims_a(1)

MM_I=0  ;STARTING MONTH
MM_E=11 ;ENDING MONTH
NMONTHS=MM_E-MM_I+1

tas_f_y=fltarr(NPOINTS,31*NYEARS,NMONTHS)
tasc_f_y=fltarr(NPOINTS,31*NYEARS,NMONTHS)
obs_f_y=fltarr(NPOINTS,31*NYEARS,NMONTHS)
tas_f_y(*,*,*)=!VALUES.F_NAN
tasc_f_y(*,*,*)=!VALUES.F_NAN
obs_f_y(*,*,*)=!VALUES.F_NAN

;LOOP OVER MONTHS
FOR im=0,NMONTHS-1 DO BEGIN
;im=0
PRINT,'MONTH= ',month(im)

FILE_OBS='obs_'+VAR+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
FILE_MOD='mod_'+VAR+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
FILE_BC=VAR+'_'+'BCed_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;FILE_COR=VAR+'_cor_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_'+month(im)+'.dat'

print,'OBS ',OBS_DIR+FILE_OBS
print,'MOD ORIG ',MOD_DIR+FILE_MOD
print,'MOD CORR ',MOD_BC_DIR+FILE_BC
;print,'FACTORS ',FAC_DIR+FILE_COR
idldata=0
restore,MOD_DIR+FILE_MOD
CASE VAR OF
    'T': tas=idldata
    'Ptot': tas=idldata
    'Tmin': tas=idldata
    'Tmax': tas=idldata
ENDCASE
idldata=0
restore,MOD_BC_DIR+FILE_BC  ;TAS_C
CASE VAR OF
    'T': tas_c=tas_c
    'Ptot': tas_c=PR_C
    'Tmin': tas_c=Tmin_c
    'Tmax': tas_c=Tmax_c
ENDCASE
idldata=0
restore,OBS_DIR+FILE_OBS
CASE VAR OF
    'T': obs=idldata
    'Ptot': obs=idldata
    'Tmin': obs=idldata
    'Tmax': obs=idldata
ENDCASE
help,tas,tas_c,obs

IF VAR EQ 'Ptot' THEN BEGIN
	tas=tas*3600.*24.
	tas_c=tas_c*3600.*24.
	obs=obs*3600.*24.
ENDIF

nx=1.*dims_a(0)
ny=1.*dims_a(1)
NPOINTS=nx*ny
NDAYS(im)=min([n_elements(REFORM(tas(0,*))),n_elements(REFORM(obs(0,*)))])
print,'MONTH= ',month(im),' NDAYS(im)= ',NDAYS(im)

tas_f_y(land_a,0:NDAYS(im)-1,im)=tas
tasc_f_y(land_c,0:NDAYS(im)-1,im)=tas_c
obs_f_y(land_a,0:NDAYS(im)-1,im)=obs

FOR i=0,NDAYS(im)-1 DO BEGIN
;CORRECTION FOR PREP <0 (e.g KNMI)
IF var EQ 'Ptot' THEN BEGIN
p0=where(tas_f_y(*,i,im) LT 0) 
IF N_ELEMENTS(p0) GT 1 THEN tas_f_y(p0,i,im)=0.
ENDIF

land_t=where(tas_f_y(*,i,im) GE 0)
land_tc=where(tasc_f_y(*,i,im) GE 0)
land_o=where(obs_f_y(*,i,im) GE 0)

SetIntersection,land_t,land_o,land_t_o
SetIntersection,land_tc,land_o,land_tc_o
SetIntersection,land_t_o,land_tc_o,land_tco
land_dd=land_com
IF im EQ 0 THEN land_com=land_tco ELSE setintersection,land_tco,land_com,land_com
IF N_ELEMENTS(land_com) LT 10000 THEN STOP
ENDFOR

help,land_com
help,land_w

ENDFOR ; END MONTH LOOP
;STOP

FOR im=0,NMONTHS-1 DO BEGIN ;NEW LOOP OVER MONTHS
print,'MONTH= ',month(im),' NDAYS(im)= ',NDAYS(im)

NPOINTS=N_ELEMENTS(land_com)
tas_com=fltarr(NPOINTS,NDAYS(im))
tasc_com=fltarr(NPOINTS,NDAYS(im))
obs_com=fltarr(NPOINTS,NDAYS(im))

;REGRIDDING ON COMMON MASK
FOR i=0,NDAYS(im)-1 do begin
tas_com(*,i)=reform(tas_f_y(land_com,i,im))
tasc_com(*,i)=reform(tasc_f_y(land_com,i,im))
obs_com(*,i)=reform(obs_f_y(land_com,i,im))
ENDFOR
lon_m=intarr(NPOINTS)
lat_m=intarr(NPOINTS)

FOR i=0,NPOINTS-1 do begin
	lat_m(i)=(land_com(i)-1)/FIX(nx)
	lon_m(i)=FIX(land_com(i)-lat_m(i)*nx-1)
ENDFOR

loadct,39
!p.multi=0
window,3,retain=2
contour,land2d_a
plots,lon_m,lat_m,psym=4,color=30

tas2d_f = FLTARR(nx,ny,ndays(im))
tasc2d_f = FLTARR(nx,ny,ndays(im))
obs2d_f = FLTARR(nx,ny,ndays(im))
tas2d_f(*,*,*)=-999.
tasc2d_f(*,*,*)=-999.
obs2d_f(*,*,*)=-999.

FOR i=0L,NPOINTS-1 DO BEGIN &$
         tas2d_f(lon_m(i),lat_m(i),*)=tas_com(i,*) &$
         obs2d_f(lon_m(i),lat_m(i),*)=obs_com(i,*) &$
         tasc2d_f(lon_m(i),lat_m(i),*)=tasc_com(i,*)
ENDFOR
print,'SAVING ',month(im),nx,ny,ndays(im)
help,tas_f_y,tas2d_f


;SAVING DATA
ofile='DATA/MOD_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
tas_f_y_m=REFORM(tas_f_y(*,*,im))
save,filename=ofile,tas_f_y_m,tas2d_f,lon_m,lat_m,land2d_m,land_com
help,land_com
ofile='DATA/MOD_BC_'+VAR+'_CORR_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
tasc_f_y_m=REFORM(tasc_f_y(*,*,im))
save,filename=ofile,tasc_f_y_m,tasc2d_f,lon_m,lat_m,land2d_m,land_com
help,land_com
ofile='DATA/OBS_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
obs_f_y_m=REFORM(obs_f_y(*,*,im))
save,filename=ofile,obs_f_y_m,obs2d_f,lon_m,lat_m,land2d_m,land_com
help,land_com

ENDFOR ;END MONTH LOOP
STOP

;PLOTS FOR CHECKS
DEVICE,decompose=0

IF VAR EQ 'Ptot' THEN BEGIN
MinCol=20
MaxCol=235
minvalue=0
Maxvalue=6
rInterval=Maxvalue-Minvalue
ContourDist=.5
iNumOfConts=FIX(rInterval/ContourDist)+1
rgrContValues = INDGEN(iNumOfConts)*ContourDist + MinValue
rgrCenterValues = FLTARR(N_ELEMENTS(rgrContValues)-1)
FOR ii = 0, N_ELEMENTS(rgrContValues)-2 DO $
	       rgrCenterValues[ii] = (rgrContValues[ii] + rgrContValues[ii+1]) / 2.
rgrColValues = INDGEN(iNumOfConts-1) * (MaxCol - MinCol) / (iNumOfConts-2) + MinCol
rgrColValues=REVERSE(rgrColValues)
ColorTBL=62
SetColorTable,ColorTBL
!p.background=0
!p.color=255
ENDIF ELSE BEGIN
MinCol=20
MaxCol=245
Minvalue=250
Maxvalue=300
;Minvalue=270
;Maxvalue=320
ContourDist=5
rInterval=Maxvalue-Minvalue
iNumOfConts=FIX(rInterval/ContourDist)+1
rgrContValues = INDGEN(iNumOfConts)*ContourDist + MinValue
rgrCenterValues = FLTARR(N_ELEMENTS(rgrContValues)-1)
FOR ii = 0, N_ELEMENTS(rgrContValues)-2 DO $
                        rgrCenterValues[ii] = (rgrContValues[ii] + rgrContValues[ii+1]) / 2.
rgrColValues = INDGEN(iNumOfConts-1) * (MaxCol - MinCol) / (iNumOfConts-2) + MinCol
ColorTBL=62
SetColorTable,ColorTBL
!p.background=0
!p.color=255
ENDELSE

window,1,retain=2
xsize=750.
ysize=550.
!p.multi=[0,1,3]
;TAS'
contour,land2d_m,xstyle=1,ystyle=1,c_colors=255,title=VAR+' '+month(im),position=[0.05,0.2,0.3,0.8],color=250
contour,tasm2d_f(*,*),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=250
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,/vertical,position=[0.95,0.2,0.97,0.8],charsize=1.5,color=255,/pscolor

;TAS'
contour,land2d_m,xstyle=1,ystyle=1,title=VAR+' CORECTED '+month(im),position=[0.35,0.2,0.6,0.8],color=250
;contour,tascm2d(*,*),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,tascm2d_f(*,*),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=250

;OBS'
contour,land2d_m,xstyle=1,ystyle=1,title='OBS '+month(im),position=[0.65,0.2,0.9,0.8],color=250
contour,obsm2d_f(*,*),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=250


;##########################
;KOLMOGOROV STATISTICS
;##########################

stat_tas=fltarr(NPOINTS)
prob_tas=fltarr(NPOINTS)
fn_tas=fltarr(NPOINTS,NDAYS(im))
tas_sor=fltarr(NPOINTS,NDAYS(im))

stat_tasc=fltarr(NPOINTS)
prob_tasc=fltarr(NPOINTS)
fn_tasc=fltarr(NPOINTS,NDAYS(im))
tasc_sor=fltarr(NPOINTS,NDAYS(im))

fn_obs=fltarr(NPOINTS,NDAYS(im))
obs_sor=fltarr(NPOINTS,NDAYS(im))

print,'CALCULATING KOLMOGOROV STATISTCIS FOR MONTH ',im+1
FOR i=0L,NPOINTS-1 DO BEGIN
	ww=reform(tas_com(i,*))
	wwc=reform(tasc_com(i,*))
	wwo=reform(obs_com(i,*))
;IF ww(0) NE 0 THEN BEGIN
;stat =  IMSL_KOLMOGOROV2(ww,wwo,DIFFERENCES = d_tas)
;stat_tas(i)=stat(0)
;prob_tas(i)=stat(2)
;stat =  IMSL_KOLMOGOROV2(wwc,wwo,DIFFERENCES = d_tas)
;stat_tasc(i)=stat(0)
;prob_tasc(i)=stat(2)
; FOR IDL WITH NO ANALYSIT LICENSE USE BELOW
ks2,ww,NDAYS(im),wwo,NDAYS(im),stat,prob,fn1,fn2,sor1,sor2
stat_tas(i)=stat
prob_tas(i)=prob
fn_tas(i,*)=fn1
tas_sor(i,*)=sor1
fn_obs(i,*)=fn2
obs_sor(i,*)=sor2
ks2,wwc,NDAYS(im),wwo,NDAYS(im),stat,prob,fn1,fn2,sor1,sor2
stat_tasc(i)=stat
prob_tasc(i)=prob
fn_tasc(i,*)=fn1
tasc_sor(i,*)=sor1
;ENDIF
ENDFOR

;2D MAPS
stat_tas2d = FLTARR(nx,ny)
stat_tasc2d = FLTARR(nx,ny)
prob_tas2d = FLTARR(nx,ny)
prob_tasc2d = FLTARR(nx,ny)
;stat_tas2d(*,*) = !VALUES.F_NAN
;stat_tasc2d(*,*) = !VALUES.F_NAN
;prob_tas2d(*,*) = !VALUES.F_NAN
;prob_tasc2d(*,*) = !VALUES.F_NAN
stat_tas2d(*,*) =-999.
stat_tasc2d(*,*) =-999.
prob_tas2d(*,*) = -999.
prob_tasc2d(*,*) =-999.

fnm_tas=FLTARR(NPOINTS)
fnm_tasc=FLTARR(NPOINTS)
fnm_obs=FLTARR(NPOINTS)

fnm_tas2d=FLTARR(nx,ny)
fnm_tasc2d=FLTARR(nx,ny)
fnm_obs2d=FLTARR(nx,ny)
fnm_tas2d(*,*) = -999.;VALUES.F_NAN
fnm_tasc2d(*,*) = -999.;VALUES.F_NAN
fnm_obs2d(*,*) = -999.;VALUES.F_NAN

FOR i=0L,NPOINTS-1 DO BEGIN
	IF VAR EQ 'Ptot' THEN BEGIN
	ww_tas=TOTAL(fn_tas(i,*))
	ww_tasc=TOTAL(fn_tasc(i,*))
	ww_obs=TOTAL(fn_obs(i,*))
        ENDIF ELSE BEGIN
	ww_tas=MEAN(fn_tas(i,*),/NaN)
	ww_tasc=MEAN(fn_tasc(i,*),/NaN)
	ww_obs=MEAN(fn_obs(i,*),/NaN)
        ENDELSE
	 stat_tas2d(lon_m(i),lat_m(i))=stat_tas(i)
	 stat_tasc2d(lon_m(i),lat_m(i))=stat_tasc(i)
	 prob_tas2d(lon_m(i),lat_m(i))=prob_tas(i)
	 prob_tasc2d(lon_m(i),lat_m(i))=prob_tasc(i)
	 fnm_tas2d(lon_m(i),lat_m(i))=ww_tas
	 fnm_tasc2d(lon_m(i),lat_m(i))=ww_tasc
	 fnm_obs2d(lon_m(i),lat_m(i))=ww_obs
ENDFOR

print,lon_m(npoints-1),lat_m(npoints-1)

;SAVING DATA
ofile='DATA/ks_stat_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
save,filename=ofile,stat_tas,prob_tas,fn_tas,tas_sor,stat_tas2d,prob_tas2d,fnm_tas2d
;save,filename=ofile,stat_tas,prob_tas,stat_tas2d,prob_tas2d
ofile='DATA/ks_stat_'+VAR+'_CORR_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
save,filename=ofile,stat_tasc,prob_tasc,fn_tasc,tasc_sor,stat_tasc2d,prob_tasc2d,fnm_tasc2d
;save,filename=ofile,stat_tasc,prob_tasc,stat_tasc2d,prob_tasc2d
ofile='DATA/ks_stat_OBS_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
save,filename=ofile,fn_obs,obs_sor,fnm_obs2d

j=9000
PRINT,'KS STAT TAS= ',stat_tas(j),' PROB TAS= ',prob_tas(j)
PRINT,'KS STAT TAS COR= ',stat_tasc(j),' PROB TAS COR= ',prob_tasc(j)

;ENDFOR ;END MONTH LOOP

;PLOTS FOR CHECK
window,4,retain=2
;STAT AND PROB
loadct,39
!p.background=255
!p.color=0
!p.multi=[0,2,2]

levels=findgen(11)/10.
colors=reverse(findgen(10)*19+15)
contour,land2d_c,xstyle=1,ystyle=1,c_colors=0,title='STAT '+VAR,position=[0.05,0.55,0.45,0.95]
contour,stat_tas2d(*,*),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,land2d_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5
colorbar,min=0,max=1,divisions=N_ELEMENTS(levels)-1,c_colors=colors,charsize=1.5,color=0,position=[0.98,0.55,0.99,0.95],/vertical

contour,land2d_c,xstyle=1,ystyle=1,c_colors=0,title='STAT '+VAR+' CORRECTED',position=[0.5,0.55,0.9,0.95]
contour,stat_tasc2d(*,*),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,land2d_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5

levels=[0,1e-6,5e-5,1e-4,5e-4,1e-3,5e-3,1e-2,5e-2,1e-1,5e-1,1]
lev_c=['0','1e-5','5e-5','1e-4','5e-4','1e-3','5e-3','1e-2','5e-2','1e-1','5e-1','1']
colors=findgen(11)*20+30
contour,land2d_c,xstyle=1,ystyle=1,c_colors=0,title='PROB '+vAR,position=[0.05,0.05,0.45,0.45]
contour,prob_tas2d(*,*),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,land2d_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5
colorbar,c_colors=colors,levels=lev_c,divisions=N_ELEMENTS(levels)-1,charsize=1.5,color=0,position=[0.98,0.05,0.99,0.45],/vertical

contour,land2d_c,xstyle=1,ystyle=1,c_colors=0,title='PROB '+VAR+' CORRECTED',position=[0.5,0.05,0.9,0.45]
contour,prob_tasc2d(*,*),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,land2d_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5



STOP
END
