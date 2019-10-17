;
PRO make_stat
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
VAR='T'

CONSTRUCTION_PERIOD_START  =  '1971'
CONSTRUCTION_PERIOD_STOP   =  '1980'

APPLICATION_PERIOD_START   =  '1991'
APPLICATION_PERIOD_STOP    =  '2000'

OBS_DIR=home+MODEL_ID+'_'+CASE_ID+'_EOBS_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'/OBS_idl/'
MOD_DIR=home+MODEL_ID+'_'+CASE_ID+'_EOBS_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'/BC_data/'

MOD_BC_DIR=home+MODEL_ID+'_'+CASE_ID+'_EOBS_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'/BC_data/'

FAC_DIR=home+MODEL_ID+'_'+CASE_ID+'_EOBS_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'/factors/'

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
    ;land_c=reform(we_c(sort(we_c)))+1
    land_c=reform(we_c(sort(we_c)))
    lat_c=land_c*0.0
    lon_c=land_c*0.0
    land2d_c=intarr(dims_c(0),dims_c(1))*0.0
    land2d_c(we_c)=1.0   
    lat2d_c=dblarr(dims_c(0),dims_c(1))
    lon2d_c=dblarr(dims_c(0),dims_c(1))
    NCDF_VARGET,fileID,'Actual_latitude',lat2d_c
    NCDF_VARGET,fileID,'Actual_longitude',lon2d_c
    
    count=0L
    count1=0L
    FOR ix=0L,dims_c(0)-1 DO BEGIN
        FOR iy=0L,dims_c(1)-1 DO BEGIN
            IF (landmask_c(ix,iy) gt mv_cutoff) THEN BEGIN
                lon_c(count)=ix
                lat_c(count)=iy 
                ;land_c(count)=iy*dims_c(0)+ix+1
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
    ;land_a=reform(we_a(sort(we_a)))+1
    land_a=reform(we_a(sort(we_a)))
    lat_a=land_a*0.0
    lon_a=land_a*0.0
    land2d_a=intarr(dims_a(0),dims_a(1))*0.0
    land2d_a(we_a)=1.0   
    lat2d_a=dblarr(dims_c(0),dims_c(1))
    lon2d_a=dblarr(dims_c(0),dims_c(1))
    NCDF_VARGET,fileID,'Actual_latitude',lat2d_a
    NCDF_VARGET,fileID,'Actual_longitude',lon2d_a
    
    count=0L
    count1=0L
    FOR ix=0L,dims_a(0)-1 DO BEGIN
        FOR iy=0L,dims_a(1)-1 DO BEGIN
            IF (landmask_a(ix,iy) gt mv_cutoff) THEN BEGIN
                lon_a(count)=ix
                lat_a(count)=iy 
                ;land_a(count)=iy*dims_a(0)+ix+1
                count=count+1
            ENDIF
            count1=count1+1
        ENDFOR
    ENDFOR
NUMLANDPOINTS_c=n_elements(land_c)
NUMLANDPOINTS_a=n_elements(land_a)

;print,land_c(0:10)
;print,land_a(0:10)
setdifference,land_a,land_c,land_diff
;IF land_diff NE -1 THEN BEGIN
IF N_ELEMENTS(land_diff) GT 1 THEN BEGIN
setdifference,land_a,land_diff,land_com
ENDIF ELSE BEGIN
land_com=land_a
ENDELSE
help,land_com
;print,land_com(0:10)


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

loadct,39
window,0,retain=2
contour,land2d_a
plots,lon_m,lat_m,psym=4,color=30

NUMLANDPOINTS=N_ELEMENTS(land_m)

;LOOP OVER MONTHS

;FOR im=0,11 DO BEGIN
;FOR im=0,1 DO BEGIN
im=0
PRINT,'MONTH= ',im

FILE_OBS='obs_'+VAR+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
FILE_MOD='mod_'+VAR+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
FILE_BC=VAR+'_'+'BCed_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
FILE_COR=VAR+'_cor_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_'+month(im)+'.dat'

print,'OBS ',OBS_DIR+FILE_OBS
print,'MOD ORIG ',MOD_DIR+FILE_MOD
print,'MOD CORR ',MOD_BC_DIR+FILE_BC
print,'FACTORS ',FAC_DIR+FILE_COR

restore,MOD_DIR+FILE_MOD
tas=idldata
restore,MOD_BC_DIR+FILE_BC  ;TAS_C
CASE VAR OF
    'T': tas_c=tas_c
    'Ptot': tas_c=PR_C
    'Tmin': tas_c=Tn_c
    'Tmax': tas_c=Tm_c
    ENDCASE
restore,OBS_DIR+FILE_OBS
obs=idldata
IF VAR EQ 'Ptot' THEN BEGIN
	tas=tas*3600.*24.
	tas_c=tas_c*3600.*24.
	obs=obs*3600.*24.
ENDIF

ndays=1.*n_elements(REFORM(tas(0,*)))
nx=1.*dims_a(0)
ny=1.*dims_a(1)
NPOINTS=nx*ny
tas_f=findgen(nx*ny,NDAYS)*0.
obs_f=findgen(nx*ny,NDAYS)*0.
tasc_f=findgen(nx*ny,NDAYS)*0.

FOR ix=0,NUMLANDPOINTS_a-1 do begin
tas_f(land_a(ix),*)=tas(ix,*)
obs_f(land_a(ix),*)=obs(ix,*)
ENDFOR
FOR ix=0,NUMLANDPOINTS_c-1 do begin
tasc_f(land_c(ix),*)=tas_c(ix,*)
ENDFOR

print,land_a(0:10),tas(0:10,0)
print,tas_f(land_a(0:10),0)

lon_f=intarr(NPOINTS)
lat_f=intarr(NPOINTS)

tasm2d_f = FLTARR(nx,ny)
tascm2d_f = FLTARR(nx,ny)
obsm2d_f = FLTARR(nx,ny)

FOR i=0L,NUMLANDPOINTS_a-1 DO BEGIN
	 ww=mean(tas_f(land_a(i),*))
         tasm2d_f(lon_a(i),lat_a(i))=ww
	 ww=mean(obs_f(land_a(i),*))
         obsm2d_f(lon_a(i),lat_a(i))=ww
ENDFOR

FOR i=0L,NUMLANDPOINTS_c-1 DO BEGIN
	 ww=mean(tasc_f(land_c(i),*))
         tascm2d_f(lon_c(i),lat_c(i))=ww
ENDFOR

tasm=fltarr(NUMLANDPOINTS_a)
obsm=fltarr(NUMLANDPOINTS_a)

tascm=fltarr(NUMLANDPOINTS_c)

tasm2d = FLTARR(nx,ny)
tascm2d = FLTARR(nx,ny)
obsm2d = FLTARR(nx,ny)
;tasm2d(*,*)=!VALUES.F_NAN
;tascm2d(*,*)=!VALUES.F_NAN
;obsm2d(*,*)=!VALUES.F_NAN
tasm2d(*,*)=-999
tascm2d(*,*)=-999
obsm2d(*,*)=-999
;tas=reform(tas(land_com,*))
;tas_c=reform(tas_c(land_com,*))
;obs=reform(obs(land_com,*))

for i=0L,NUMLANDPOINTS_a-1 do begin
	tasm(i)=mean(tas(i,*))
	obsm(i)=mean(obs(i,*))
        nlon=lon_a(i)
        nlat=lat_a(i)
        tasm2d(nlon,nlat)=tasm(i)
        obsm2d(nlon,nlat)=obsm(i)
ENDFOR
for i=0L,NUMLANDPOINTS_c-1 do begin
	tascm(i)=mean(tas_c(i,*))
        nlon=lon_c(i)
        nlat=lat_c(i)
        tascm2d(nlon,nlat)=tascm(i)
ENDFOR


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


;KOLMOGOROV STATISTICS

stat_tas=fltarr(NPOINTS)
prob_tas=fltarr(NPOINTS)
fn_tas=fltarr(NPOINTS,ndays)
tas_sor=fltarr(NPOINTS,ndays)

stat_tasc=fltarr(NPOINTS)
prob_tasc=fltarr(NPOINTS)
fn_tasc=fltarr(NPOINTS,ndays)
tasc_sor=fltarr(NPOINTS,ndays)

fn_obs=fltarr(NPOINTS,ndays)
obs_sor=fltarr(NPOINTS,ndays)

print,'CALCULATING KOLMOGOROV STATISTCIS'
;FOR i=0L,NPOINTS-1 DO BEGIN
FOR i=0L,NUMLANDPOINTS_C-1 DO BEGIN
	ww=reform(tas_f(land_c(i),*))
	wwc=reform(tasc_f(land_c(i),*))
	wwo=reform(obs_f(land_c(i),*))
;IF ww(0) NE 0 THEN BEGIN
;stat =  IMSL_KOLMOGOROV2(reform(tas(i,*)),reform(obs(i,*)),DIFFERENCES = d_tas)
;stat_tas(i)=stat(0)
;prob_tas(i)=stat(1)
;fn_tas(i,*)=fn1
;tas_sor(i,*)=sor1
;fn_obs(i,*)=fn2
;obs_sor(i,*)=sor2
; FOR IDL WITH NO ANALYSIT LICENSE USE BELOW
ks2,ww,ndays,wwo,ndays,stat,prob,fn1,fn2,sor1,sor2
stat_tas(land_c(i))=stat
prob_tas(land_c(i))=prob
fn_tas(land_c(i),*)=fn1
tas_sor(land_c(i),*)=sor1
fn_obs(land_c(i),*)=fn2
obs_sor(land_c(i),*)=sor2
;tats  =  IMSL_KOLMOGOROV2(reform(tasc(i,*)),reform(obs(i,*)),DIFFERENCES = d_tasc)
; FOR IDL WITH NO ANALYSIT LICENSE USE BELOW
ks2,wwc,ndays,wwo,ndays,stat,prob,fn1,fn2,sor1,sor2
stat_tasc(land_c(i))=stat
prob_tasc(land_c(i))=prob
fn_tasc(land_c(i),*)=fn1
tasc_sor(land_c(i),*)=sor1
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

;FOR i=0L,NPOINTS-1 DO BEGIN
;	IF VAR EQ 'Ptot' THEN BEGIN
;	fnm_tas(i)=TOTAL(fn_tas(i,*))
;	fnm_tasc(i)=TOTAL(fn_tasc(i,*))
;	fnm_obs(i)=TOTAL(fn_obs(i,*))
;        ENDIF ELSE BEGIN
;	fnm_tas(i)=MEAN(fn_tas(i,*))
;	fnm_tasc(i)=MEAN(fn_tasc(i,*))
;	fnm_obs(i)=MEAN(fn_obs(i,*))
;        ENDELSE
;	 nlon=lon_m(i)
;	 nlat=lat_m(i)
;	 stat_tas2d(nlon,nlat)=stat_tas(i)
;	 stat_tasc2d(nlon,nlat)=stat_tasc(i)
;	 prob_tas2d(nlon,nlat)=prob_tas(i)
;	 prob_tasc2d(nlon,nlat)=prob_tasc(i)
;	 fnm_tas2d(nlon,nlat)=fnm_tas(i)
;	 fnm_tasc2d(nlon,nlat)=fnm_tasc(i)
;	 fnm_obs2d(nlon,nlat)=fnm_obs(i)
;ENDFOR
FOR i=0L,NUMLANDPOINTS_c-1 DO BEGIN
	IF VAR EQ 'Ptot' THEN BEGIN
	ww_tas=TOTAL(fn_tas(land_c(i),*))
	ww_tasc=TOTAL(fn_tasc(land_c(i),*))
	ww_obs=TOTAL(fn_obs(land_c(i),*))
        ENDIF ELSE BEGIN
	ww_tas=MEAN(fn_tas(land_c(i),*))
	ww_tasc=MEAN(fn_tasc(land_c(i),*))
	ww_obs=MEAN(fn_obs(land_c(i),*))
        ENDELSE
	 stat_tas2d(lon_c(i),lat_c(i))=stat_tas(land_c(i))
	 stat_tasc2d(lon_c(i),lat_c(i))=stat_tasc(land_c(i))
	 prob_tas2d(lon_c(i),lat_c(i))=prob_tas(land_c(i))
	 prob_tasc2d(lon_c(i),lat_c(i))=prob_tasc(land_c(i))
	 fnm_tas2d(lon_c(i),lat_c(i))=ww_tas
	 fnm_tasc2d(lon_c(i),lat_c(i))=ww_tasc
	 fnm_obs2d(lon_c(i),lat_c(i))=ww_obs
ENDFOR

;SAVING DATA
ofile='DATA/ks_stat_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
save,filename=ofile,stat_tas,prob_tas,fn_tas,tas_sor,stat_tas2d,prob_tas2d,fnm_tas2d
ofile='DATA/ks_stat_'+VAR+'_CORR_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
save,filename=ofile,stat_tasc,prob_tasc,fn_tasc,tasc_sor,stat_tasc2d,prob_tasc2d,fnm_tasc2d
ofile='DATA/ks_stat_OBS_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
save,filename=ofile,fn_obs,obs_sor,fnm_obs2d

ofile='DATA/MOD_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
save,filename=ofile,tasm2d_f
ofile='DATA/MOD_BC_'+VAR+'_CORR_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
save,filename=ofile,tascm2d_f
ofile='DATA/OBS_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
save,filename=ofile,obsm2d_f

j=1900
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

datarr=tas
ndays=n_elements(datarr(1,*))
;data_a_tas = FLTARR(ndays,dims(1),dims(0))
;tas2d = FLTARR(ndays,dims(0),dims(1))
FOR i=0L,NUMLANDPOINTS-1 DO BEGIN
        nlon=lon(i)
        nlat=lat(i)
        FOR t=0,ndays-1 DO BEGIN
            ;data_a_tas(t,nlat,nlon)=datarr(i,t)
            tas2d(t,nlon,nlat)=datarr(i,t)
        ENDFOR
ENDFOR

l=n_elements(tas_c(0,*))
datarr=tas_c
ndays=n_elements(datarr(1,*))
;data_a_tas = FLTARR(ndays,dims(1),dims(0))
tas_c2d = FLTARR(ndays,dims(0),dims(1))
FOR i=0L,NUMLANDPOINTS-1 DO BEGIN
        nlon=lon(i)
        nlat=lat(i)
        FOR t=0,ndays-1 DO BEGIN
            ;data_a_tas(t,nlat,nlon)=datarr(i,t)
            tas_c2d(t,nlon,nlat)=datarr(i,t)
        ENDFOR
ENDFOR

l=n_elements(obs(0,*))
datarr=obs
ndays=n_elements(datarr(1,*))
;data_a_tas = FLTARR(ndays,dims(1),dims(0))
obs2d = FLTARR(ndays,dims(0),dims(1))
FOR i=0L,NUMLANDPOINTS-1 DO BEGIN
        nlon=lon(i)
        nlat=lat(i)
        FOR t=0,ndays-1 DO BEGIN
            ;data_a_tas(t,nlat,nlon)=datarr(i,t)
            obs2d(t,nlon,nlat)=datarr(i,t)
        ENDFOR
ENDFOR


restore,FILE_COr
A=a_tas
af=a_tf
ad=a_td
l=n_elements(a_tas(0,*))
STOP

datarr=a_tas
ndays=n_elements(datarr(1,*))
;data_a_tas = FLTARR(ndays,dims(1),dims(0))
data_a_tas = FLTARR(ndays,dims(0),dims(1))
FOR i=0L,NUMLANDPOINTS-1 DO BEGIN
        nlon=lon(i)
        nlat=lat(i)
        FOR t=0,ndays-1 DO BEGIN
            ;data_a_tas(t,nlat,nlon)=datarr(i,t)
            data_a_tas(t,nlon,nlat)=datarr(i,t)
        ENDFOR
ENDFOR

l=n_elements(a_tf(0,*))
datarr=a_tf
ndays=n_elements(datarr(1,*))
data_a_tf = FLTARR(ndays,dims(0),dims(1))
FOR i=0L,NUMLANDPOINTS-1 DO BEGIN
        nlon=lon(i)
        nlat=lat(i)
        FOR t=0,ndays-1 DO BEGIN
            data_a_tf(t,nlon,nlat)=datarr(i,t)
        ENDFOR
ENDFOR

l=n_elements(a_td(0,*))
datarr=a_td
ndays=n_elements(datarr(1,*))
data_a_td = FLTARR(ndays,dims(0),dims(1))
FOR i=0L,NUMLANDPOINTS-1 DO BEGIN
        nlon=lon(i)
        nlat=lat(i)
        FOR t=0,ndays-1 DO BEGIN
            data_a_td(t,nlon,nlat)=datarr(i,t)
        ENDFOR
ENDFOR

loadct,39
window,0,retain=2
levels=findgen(25)*20.-200
print,'levels A=', levels
colors=findgen(25)*10
contour,land2d,xstyle=1,ystyle=1,c_colors=255
contour,data_a_tas(0,*,*),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,land2d,xstyle=1,ystyle=1,c_colors=255,/overplot

window,1,retain=2
levels=findgen(25)*0.1
print,'levels B=', levels
colors=findgen(25)*10
contour,land2d,xstyle=1,ystyle=1,c_colors=0
contour,data_a_tas(1,*,*),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,land2d,xstyle=1,ystyle=1,c_colors=255,/overplot

window,2,retain=2
levels=findgen(25)/24.
colors=findgen(25)*10
contour,land2d,xstyle=1,ystyle=1,c_colors=255
contour,data_a_td(0,*,*),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,land2d,xstyle=1,ystyle=1,c_colors=255,/overplot

window,3,retain=2
levels=findgen(25)/24.
colors=findgen(25)*10
contour,land2d,xstyle=1,ystyle=1,c_colors=0
contour,data_a_td(1,*,*),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,land2d,xstyle=1,ystyle=1,c_colors=255,/overplot

window,4,retain=2
levels=findgen(25)/24.
colors=findgen(25)*10
contour,land2d,xstyle=1,ystyle=1,c_colors=255
contour,data_a_tf(0,*,*),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,land2d,xstyle=1,ystyle=1,c_colors=255,/overplot

window,5,retain=2
levels=findgen(25)/24.
colors=findgen(25)*10
contour,land2d,xstyle=1,ystyle=1,c_colors=0
contour,data_a_tf(1,*,*),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,land2d,xstyle=1,ystyle=1,c_colors=255,/overplot

STOP
	
;______________________________________________________________
;
end
