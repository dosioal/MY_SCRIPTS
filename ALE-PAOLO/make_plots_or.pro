;
PRO make_plots
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
;LM_FILE='/media/disk/DATA/ENSOBS/ENS_MODELS/read_in_'+MODEL_ID+'_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'.nc'
LM_FILE='/media/disk/DATA/ENSOBS/ENS_MODELS/read_in_'+MODEL_ID+'_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'.nc'
lm_var     = 'tg'
mv_cutoff  = 1.0

    file = LM_FILE
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
                lon(count)=ix
                lat(count)=iy 
                land(count)=iy*dims(0)+ix+1
                count=count+1
            ENDIF
            count1=count1+1
        ENDFOR
    ENDFOR
						;
NUMLANDPOINTS=n_elements(land)

tasm=fltarr(NUMLANDPOINTS)
tascm=fltarr(NUMLANDPOINTS)
obsm=fltarr(NUMLANDPOINTS)

tasm2d = FLTARR(dims(0),dims(1))
tascm2d = FLTARR(dims(0),dims(1))
obsm2d = FLTARR(dims(0),dims(1))
;tasm2d(*,*)=!VALUES.F_NAN
;tascm2d(*,*)=!VALUES.F_NAN
;obsm2d(*,*)=!VALUES.F_NAN
tasm2d(*,*)=-999
tascm2d(*,*)=-999
obsm2d(*,*)=-999

;LOOP OVER MONTHS

;FOR im=0,11 DO BEGIN
FOR im=0,1 DO BEGIN
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

l=n_elements(tas(0,*))
for i=0L,NUMLANDPOINTS-1 do begin
	tasm(i)=mean(tas(i,*))
	tascm(i)=mean(tas_c(i,*))
	obsm(i)=mean(obs(i,*))
endfor

FOR i=0L,NUMLANDPOINTS-1 DO BEGIN
        nlon=lon(i)
        nlat=lat(i)
        tasm2d(nlon,nlat)=tasm(i)
        tascm2d(nlon,nlat)=tascm(i)
        obsm2d(nlon,nlat)=obsm(i)
ENDFOR

;KOLMOGOROV STATISTICS
ndays=n_elements(tas(0,*))

stat_tas=fltarr(NUMLANDPOINTS)
prob_tas=fltarr(NUMLANDPOINTS)
fn_tas=fltarr(NUMLANDPOINTS,ndays)
tas_sor=fltarr(NUMLANDPOINTS,ndays)

stat_tasc=fltarr(NUMLANDPOINTS)
prob_tasc=fltarr(NUMLANDPOINTS)
fn_tasc=fltarr(NUMLANDPOINTS,ndays)
tasc_sor=fltarr(NUMLANDPOINTS,ndays)

fn_obs=fltarr(NUMLANDPOINTS,ndays)
obs_sor=fltarr(NUMLANDPOINTS,ndays)


;RESTORING DATA
ofile='DATA/ks_stat_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
restore,filename=ofile
print,'RESTORING '+ofile
ofile='DATA/ks_stat_'+VAR+'_CORR_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
restore,filename=ofile
print,'RESTORING '+ofile
ofile='DATA/ks_stat_OBS_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
restore,filename=ofile
print,'RESTORING '+ofile

;2D MAPS
stat_tas2d = FLTARR(dims(0),dims(1))
stat_tasc2d = FLTARR(dims(0),dims(1))
prob_tas2d = FLTARR(dims(0),dims(1))
prob_tasc2d = FLTARR(dims(0),dims(1))
;stat_tas2d(*,*) = !VALUES.F_NAN
;stat_tasc2d(*,*) = !VALUES.F_NAN
;prob_tas2d(*,*) = !VALUES.F_NAN
;prob_tasc2d(*,*) = !VALUES.F_NAN
stat_tas2d(*,*) =-999.
stat_tasc2d(*,*) =-999.
prob_tas2d(*,*) = -999.
prob_tasc2d(*,*) =-999.

fnm_tas=FLTARR(NUMLANDPOINTS)
fnm_tasc=FLTARR(NUMLANDPOINTS)
fnm_obs=FLTARR(NUMLANDPOINTS)

fnm_tas2d=FLTARR(dims(0),dims(1))
fnm_tasc2d=FLTARR(dims(0),dims(1))
fnm_obs2d=FLTARR(dims(0),dims(1))
fnm_tas2d(*,*) = -999.;VALUES.F_NAN
fnm_tasc2d(*,*) = -999.;VALUES.F_NAN
fnm_obs2d(*,*) = -999.;VALUES.F_NAN

FOR i=0L,NUMLANDPOINTS-1 DO BEGIN
	IF VAR EQ 'Ptot' THEN BEGIN
	fnm_tas(i)=TOTAL(fn_tas(i,*))
	fnm_tasc(i)=TOTAL(fn_tasc(i,*))
	fnm_obs(i)=TOTAL(fn_obs(i,*))
        ENDIF ELSE BEGIN
	fnm_tas(i)=MEAN(fn_tas(i,*))
	fnm_tasc(i)=MEAN(fn_tasc(i,*))
	fnm_obs(i)=MEAN(fn_obs(i,*))
        ENDELSE
	 nlon=lon(i)
	 nlat=lat(i)
	 stat_tas2d(nlon,nlat)=stat_tas(i)
	 stat_tasc2d(nlon,nlat)=stat_tasc(i)
	 prob_tas2d(nlon,nlat)=prob_tas(i)
	 prob_tasc2d(nlon,nlat)=prob_tasc(i)
	 fnm_tas2d(nlon,nlat)=fnm_tas(i)
	 fnm_tasc2d(nlon,nlat)=fnm_tasc(i)
	 fnm_obs2d(nlon,nlat)=fnm_obs(i)
ENDFOR

;PLOTS
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

xsize=750.
ysize=550.

;choice=''
;PRINT,'PRINT PLOT? (y/n)'
;read,choice
;IF choice eq 'n' THEN GOTO,GOON

set_plot,'ps'
PRINT,'Creating Postscriptfile: '
device,file='./PLOTS/PLOT_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.ps',/color,/landscape

!p.multi=[0,1,3]
;TAS'
contour,land2d,xstyle=1,ystyle=1,c_colors=255,title=VAR+' '+month(im),position=[0.05,0.2,0.3,0.8],color=250
contour,tasm2d(*,*),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d,xstyle=1,ystyle=1,/overplot,thick=0.5,color=250
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,/vertical,position=[0.95,0.2,0.97,0.8],charsize=1.5,color=255,/pscolor

;TAS'
contour,land2d,xstyle=1,ystyle=1,title=VAR+' CORECTED '+month(im),position=[0.35,0.2,0.6,0.8],color=250
contour,tascm2d(*,*),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d,xstyle=1,ystyle=1,/overplot,thick=0.5,color=250

;OBS'
contour,land2d,xstyle=1,ystyle=1,title='OBS '+month(im),position=[0.65,0.2,0.9,0.8],color=250
contour,obsm2d(*,*),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d,xstyle=1,ystyle=1,/overplot,thick=0.5,color=250


;STAT AND PROB
loadct,39
!p.background=255
!p.color=0
!p.multi=[0,2,2]

levels=findgen(11)/10.
colors=reverse(findgen(10)*19+15)
contour,land2d,xstyle=1,ystyle=1,c_colors=0,title='STAT '+VAR,position=[0.05,0.55,0.45,0.95]
contour,stat_tas2d(*,*),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,land2d,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5
colorbar,min=0,max=1,divisions=N_ELEMENTS(levels)-1,c_colors=colors,charsize=1.5,color=0,position=[0.98,0.55,0.99,0.95],/vertical

contour,land2d,xstyle=1,ystyle=1,c_colors=0,title='STAT '+VAR+' CORRECTED',position=[0.5,0.55,0.9,0.95]
contour,stat_tasc2d(*,*),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,land2d,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5

levels=[0,1e-6,5e-5,1e-4,5e-4,1e-3,5e-3,1e-2,5e-2,1e-1,5e-1,1]
lev_c=['0','1e-5','5e-5','1e-4','5e-4','1e-3','5e-3','1e-2','5e-2','1e-1','5e-1','1']
colors=findgen(11)*20+30
contour,land2d,xstyle=1,ystyle=1,c_colors=0,title='PROB '+vAR,position=[0.05,0.05,0.45,0.45]
contour,prob_tas2d(*,*),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,land2d,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5
colorbar,c_colors=colors,levels=lev_c,divisions=N_ELEMENTS(levels)-1,charsize=1.5,color=0,position=[0.98,0.05,0.99,0.45],/vertical

contour,land2d,xstyle=1,ystyle=1,c_colors=0,title='PROB '+VAR+' CORRECTED',position=[0.5,0.05,0.9,0.45]
contour,prob_tasc2d(*,*),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,land2d,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5


device,/close
set_plot,'x'

GOON:
;SOME BAISC STATISTICS FOR ONE POINT
j=9000
IF VAR EQ 'Ptot' THEN BEGIN
atas=reform(tas(j,*))
aobs=reform(obs(j,*))
atasc=reform(tas_c(j,*))
ENDIF ELSE BEGIN
atas=reform(tas(j,*))-273
aobs=reform(obs(j,*))-273
atasc=reform(tas_c(j,*))-273
ENDELSE

vartas=moment(atas) 
vartasc=moment(atasc)
varobs=moment(aobs)
print,'MOMENTS TAS',vartas
print,'MOMENTS TASC',vartasc
print,'MOMENTS OBS',varobs

PRINT,'KS STAT TAS= ',stat_tas(j),' PROB TAS= ',prob_tas(j)
PRINT,'KS STAT TAS COR= ',stat_tasc(j),' PROB TAS COR= ',prob_tasc(j)

ENDFOR   ;END MONTH LOOP


;PLOTS
i=9000 
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

window,0,retain=2
!p.multi=[0,1,3]
;TAS'
contour,land2d,xstyle=1,ystyle=1,c_colors=255,title=VAR+' '+month(im),position=[0.05,0.2,0.3,0.8]
contour,tasm2d(*,*),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
xyouts,lat(i),lon(i),'QUI',charthick=2,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,/vertical,position=[0.95,0.2,0.97,0.8],charsize=1.5,color=255

;TAS'
contour,land2d,xstyle=1,ystyle=1,title=VAR+' CORECTED '+month(im),position=[0.35,0.2,0.6,0.8]
contour,tascm2d(*,*),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

;OBS'
contour,land2d,xstyle=1,ystyle=1,title='OBS '+month(im),position=[0.65,0.2,0.9,0.8]
contour,obsm2d(*,*),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

!p.multi=[0,2,2]
loadct,39
!p.background=255
!p.color=0
window,4,retain=2
plot,atas,thick=1,color=0,/nodata,title='DATA'
oplot,atas,thick=1.5,color=250
oplot,aobs,thick=1.5,color=0
oplot,atasc,thick=1.5,color=130

plot,atas-aobs,thick=1.5,title='BIAS'
oplot,atasc-aobs,color=130,thick=1.5

IF VAR EQ 'Ptot' THEN BEGIN
plot,tas_sor(i,*),color=0,title='DATA SORTED',/nodata
oplot,tas_sor(i,*),color=250,thick=2
oplot,obs_sor(i,*),color=0,thick=2
oplot,tasc_sor(i,*),color=130,thick=2
ENDIF ELSE BEGIN
plot,tas_sor(i,*)-273,color=0,title='DATA SORTED',/nodata
oplot,tas_sor(i,*)-273,color=250,thick=2
oplot,obs_sor(i,*)-273,color=0,thick=2
oplot,tasc_sor(i,*)-273,color=130,thick=2
ENDELSE

plot,tas_sor(i,*),fn_tas(i,*),color=0,title='FN',/nodata
oplot,tas_sor(i,*),fn_tas(i,*),color=250,thick=2
oplot,obs_sor(i,*),fn_obs(i,*),color=0,thick=2
oplot,tasc_sor(i,*),fn_tasc(i,*),color=130,thick=2

;STAT
window,5,retain=2
loadct,39
!p.background=255
!p.color=0
!p.multi=[0,2,2]

levels=findgen(11)/10.
colors=reverse(findgen(10)*19+15)
;colors(0)=255
contour,land2d,xstyle=1,ystyle=1,c_colors=0,title='STAT '+VAR,position=[0.05,0.55,0.45,0.95]
contour,stat_tas2d(*,*),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,land2d,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5
colorbar,min=0,max=1,divisions=N_ELEMENTS(levels)-1,c_colors=colors,charsize=1.5,color=0,position=[0.96,0.55,0.99,0.95],/vertical

contour,land2d,xstyle=1,ystyle=1,c_colors=0,title='STAT '+VAR+' CORRECTED',position=[0.5,0.55,0.9,0.95]
contour,stat_tasc2d(*,*),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,land2d,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5

levels=[0,1e-6,5e-5,1e-4,5e-4,1e-3,5e-3,1e-2,5e-2,1e-1,5e-1,1]
lev_c=['0','1e-5','5e-5','1e-4','5e-4','1e-3','5e-3','1e-2','5e-2','1e-1','5e-1','1']
colors=findgen(11)*20+30
;colors(0)=255
contour,land2d,xstyle=1,ystyle=1,c_colors=0,title='PROB '+vAR,position=[0.05,0.05,0.45,0.45]
contour,prob_tas2d(*,*),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,land2d,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5
colorbar,c_colors=colors,levels=lev_c,divisions=N_ELEMENTS(levels)-1,charsize=1.5,color=0,position=[0.96,0.05,0.99,0.45],/vertical

contour,land2d,xstyle=1,ystyle=1,c_colors=0,title='PROB '+VAR+' CORRECTED',position=[0.5,0.05,0.9,0.45]
contour,prob_tasc2d(*,*),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,land2d,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5


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
