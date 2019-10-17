;
PRO make_averages
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

CONSTRUCTION_PERIOD_START  =  '1981'
CONSTRUCTION_PERIOD_STOP   =  '1990'

APPLICATION_PERIOD_START   =  '1991'
APPLICATION_PERIOD_STOP    =  '2000'

OBS_DIR=home+MODEL_ID+'_'+CASE_ID+'_EOBS_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'/OBS_idl/'
MOD_DIR=home+MODEL_ID+'_'+CASE_ID+'_EOBS_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'/BC_data/'

MOD_BC_DIR=home+MODEL_ID+'_'+CASE_ID+'_EOBS_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'/BC_data/'

;-----------------------------------------------------------------------------------------------------------

; specify the months to be bias corrected
month=['01','02','03','04','05','06','07','08','09','10','11','12']
;

ofile='DATA/MOD_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_01.dat'
restore,filename=ofile

;DATA
nx=N_ELEMENTS(tasm2d_f(*,0))
ny=N_ELEMENTS(tasm2d_f(0,*))
NPOINTS=N_ELEMENTS(tas_f(*,0))

tas=fltarr(NPOINTS,12,310)
tasc=fltarr(NPOINTS,12,310)
obs=fltarr(NPOINTS,12,310)
tas(*,*,*)=!VALUES.F_NAN
tasc(*,*,*)=!VALUES.F_NAN
obs(*,*,*)=!VALUES.F_NAN


;LOOP OVER MONTHS

;FOR im=0,11 DO BEGIN
FOR im=0,11 DO BEGIN
PRINT,'MONTH= ',im

;RESTORING DATA
;DATA FILES
ofile='DATA/MOD_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
restore,filename=ofile
print,'RESTORING '+ofile
ofile='DATA/MOD_BC_'+VAR+'_CORR_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
restore,filename=ofile
print,'RESTORING '+ofile
ofile='DATA/OBS_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
restore,filename=ofile
print,'RESTORING '+ofile

NDAYS=N_ELEMENTS(tas_f(0,*))
tas(*,im,0:NDAYS-1)=tas_f
tasc(*,im,0:NDAYS-1)=tasc_f
obs(*,im,0:NDAYS-1)=obs_f

ENDFOR ;END LOOP MONTHS

;SEASONAL AVERAGES


tas_sm=fltarr(NPOINTS,4)
tasc_sm=fltarr(NPOINTS,4)
obs_sm=fltarr(NPOINTS,4)
tas_w=fltarr(310*3)

FOR i=0,NPOINTS/2-1 DO BEGIN
IF tas(i,0,0) GE 0. THEN BEGIN
;DJF
tas_w(0:309)=tas(i,0,*)
tas_w(310:619)=tas(i,1,*)
tas_w(620:929)=tas(i,11,*)
tas_sm(i,0)=MEAN(tas_w,/NAN)
;MAM
tas_w(0:309)=tas(i,2,*)
tas_w(310:619)=tas(i,3,*)
tas_w(620:929)=tas(i,4,*)
tas_sm(i,1)=MEAN(tas_w,/NAN)
;JJA
tas_w(0:309)=tas(i,5,*)
tas_w(310:619)=tas(i,6,*)
tas_w(620:929)=tas(i,7,*)
tas_sm(i,2)=MEAN(tas_w,/NAN)
;SON
tas_w(0:309)=tas(i,8,*)
tas_w(310:619)=tas(i,9,*)
tas_w(620:929)=tas(i,10,*)
tas_sm(i,3)=MEAN(tas_w,/NAN)

;DJF
tas_w(0:309)=tasc(i,0,*)
tas_w(310:619)=tasc(i,1,*)
tas_w(620:929)=tasc(i,11,*)
tasc_sm(i,0)=MEAN(tas_w,/NAN)
;MAM
tas_w(0:309)=tasc(i,2,*)
tas_w(310:619)=tasc(i,3,*)
tas_w(620:929)=tasc(i,4,*)
tasc_sm(i,1)=MEAN(tas_w,/NAN)
;JJA
tas_w(0:309)=tasc(i,5,*)
tas_w(310:619)=tasc(i,6,*)
tas_w(620:929)=tasc(i,7,*)
tasc_sm(i,2)=MEAN(tas_w,/NAN)
;SON
tas_w(0:309)=tasc(i,8,*)
tas_w(310:619)=tasc(i,9,*)
tas_w(620:929)=tasc(i,10,*)
tasc_sm(i,3)=MEAN(tas_w,/NAN)

;DJF
tas_w(0:309)=obs(i,0,*)
tas_w(310:619)=obs(i,1,*)
tas_w(620:929)=obs(i,11,*)
obs_sm(i,0)=MEAN(tas_w,/NAN)
;MAM
tas_w(0:309)=tas(i,2,*)
tas_w(310:619)=tas(i,3,*)
tas_w(620:929)=tas(i,4,*)
obs_sm(i,1)=MEAN(tas_w,/NAN)
;JJA
tas_w(0:309)=tas(i,5,*)
tas_w(310:619)=tas(i,6,*)
tas_w(620:929)=tas(i,7,*)
obs_sm(i,2)=MEAN(tas_w,/NAN)
;SON
tas_w(0:309)=obs(i,8,*)
tas_w(310:619)=obs(i,9,*)
tas_w(620:929)=obs(i,10,*)
obs_sm(i,3)=MEAN(tas_w,/NAN)

ENDIF
ENDFOR

FOR i=NPOINTS/2,NPOINTS-1 DO BEGIN
IF tas(i,0,0) GE 0. THEN BEGIN
;DJF
tas_w(0:309)=tas(i,0,*)
tas_w(310:619)=tas(i,1,*)
tas_w(620:929)=tas(i,11,*)
tas_sm(i,0)=MEAN(tas_w,/NAN)
;MAM
tas_w(0:309)=tas(i,2,*)
tas_w(310:619)=tas(i,3,*)
tas_w(620:929)=tas(i,4,*)
tas_sm(i,1)=MEAN(tas_w,/NAN)
;JJA
tas_w(0:309)=tas(i,5,*)
tas_w(310:619)=tas(i,6,*)
tas_w(620:929)=tas(i,7,*)
tas_sm(i,2)=MEAN(tas_w,/NAN)
;SON
tas_w(0:309)=tas(i,8,*)
tas_w(310:619)=tas(i,9,*)
tas_w(620:929)=tas(i,10,*)
tas_sm(i,3)=MEAN(tas_w,/NAN)

;DJF
tas_w(0:309)=tasc(i,0,*)
tas_w(310:619)=tasc(i,1,*)
tas_w(620:929)=tasc(i,11,*)
tasc_sm(i,0)=MEAN(tas_w,/NAN)
;MAM
tas_w(0:309)=tasc(i,2,*)
tas_w(310:619)=tasc(i,3,*)
tas_w(620:929)=tasc(i,4,*)
tasc_sm(i,1)=MEAN(tas_w,/NAN)
;JJA
tas_w(0:309)=tasc(i,5,*)
tas_w(310:619)=tasc(i,6,*)
tas_w(620:929)=tasc(i,7,*)
tasc_sm(i,2)=MEAN(tas_w,/NAN)
;SON
tas_w(0:309)=tasc(i,8,*)
tas_w(310:619)=tasc(i,9,*)
tas_w(620:929)=tasc(i,10,*)
tasc_sm(i,3)=MEAN(tas_w,/NAN)

;DJF
tas_w(0:309)=obs(i,0,*)
tas_w(310:619)=obs(i,1,*)
tas_w(620:929)=obs(i,11,*)
obs_sm(i,0)=MEAN(tas_w,/NAN)
;MAM
tas_w(0:309)=tas(i,2,*)
tas_w(310:619)=tas(i,3,*)
tas_w(620:929)=tas(i,4,*)
obs_sm(i,1)=MEAN(tas_w,/NAN)
;JJA
tas_w(0:309)=tas(i,5,*)
tas_w(310:619)=tas(i,6,*)
tas_w(620:929)=tas(i,7,*)
obs_sm(i,2)=MEAN(tas_w,/NAN)
;SON
tas_w(0:309)=obs(i,8,*)
tas_w(310:619)=obs(i,9,*)
tas_w(620:929)=obs(i,10,*)
obs_sm(i,3)=MEAN(tas_w,/NAN)

ENDIF
ENDFOR

NPOINTS=N_ELEMENTS(lon_m)
tas2d_sm = FLTARR(nx,ny,4)
tasc2d_sm = FLTARR(nx,ny,4)
obs2d_sm = FLTARR(nx,ny,4)
FOR j=0,3 DO BEGIN &$
FOR i=0L,NPOINTS-1 DO BEGIN &$
         tas2d_sm(lon_m(i),lat_m(i),j)=tas_sm(i,j) &$
         obs2d_sm(lon_m(i),lat_m(i),j)=obs_sm(i,j) &$
         tasc2d_sm(lon_m(i),lat_m(i),j)=tasc_sm(i,j) &$
ENDFOR &$
ENDFOR




;STAT FILES
;ofile='DATA/ks_stat_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;restore,filename=ofile
;print,'RESTORING '+ofile
;ofile='DATA/ks_stat_'+VAR+'_CORR_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;restore,filename=ofile
;print,'RESTORING '+ofile
;ofile='DATA/ks_stat_OBS_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;restore,filename=ofile
;print,'RESTORING '+ofile


;PLOTS
DEVICE,decompose=0

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
!p.multi=[0,2,2]
;TAS'
contour,land2d_m,xstyle=1,ystyle=1,c_colors=255,title=VAR+' DJF',position=[0.05,0.55,0.45,0.95]
contour,tas2d_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1.5,color=25,position=[0.96,0.55,0.99,0.95],/vertical

contour,land2d_m,xstyle=1,ystyle=1,c_colors=255,title=VAR+' MAM',position=[0.5,0.55,0.9,0.95]
contour,tas2d_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,land2d_m,xstyle=1,ystyle=1,c_colors=255,title=VAR+' JJA',position=[0.05,0.05,0.45,0.45]
contour,tas2d_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,land2d_m,xstyle=1,ystyle=1,c_colors=255,title=VAR+' SON',position=[0.5,0.05,0.9,0.45]
contour,tas2d_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

window,1,retain=2
!p.multi=[0,2,2]
;TAS CORR'
contour,land2d_m,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORRECTED DJF',position=[0.05,0.55,0.45,0.95]
contour,tasc2d_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1.5,color=25,position=[0.96,0.55,0.99,0.95],/vertical

contour,land2d_m,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORRECTED MAM',position=[0.5,0.55,0.9,0.95]
contour,tasc2d_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,land2d_m,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORRECTED JJA',position=[0.05,0.05,0.45,0.45]
contour,tasc2d_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,land2d_m,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORRECTED SON',position=[0.5,0.05,0.9,0.45]
contour,tasc2d_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

window,2,retain=2
!p.multi=[0,2,2]
;OBS'
contour,land2d_m,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS DJF',position=[0.05,0.55,0.45,0.95]
contour,obs2d_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1.5,color=25,position=[0.96,0.55,0.99,0.95],/vertical

contour,land2d_m,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS MAM',position=[0.5,0.55,0.9,0.95]
contour,obs2d_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,land2d_m,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS JJA',position=[0.05,0.05,0.45,0.45]
contour,obs2d_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,land2d_m,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS SON',position=[0.5,0.05,0.9,0.45]
contour,obs2d_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255



STOP
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
END
