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

;LOOP OVER MONTHS

;FOR im=0,11 DO BEGIN
FOR im=0,1 DO BEGIN
PRINT,'MONTH= ',im


;KOLMOGOROV STATISTICS


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

ofile='DATA/MOD_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
restore,filename=ofile
print,'RESTORING '+ofile
ofile='DATA/MOD_BC_'+VAR+'_CORR_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
restore,filename=ofile
print,'RESTORING '+ofile
ofile='DATA/OBS_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
restore,filename=ofile
print,'RESTORING '+ofile

;2D MAPS
NUMLANDPOINTS=N_ELEMENTS(lon_m)

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
contour,land2d_m,xstyle=1,ystyle=1,c_colors=255,title=VAR+' '+month(im),position=[0.05,0.2,0.3,0.8],color=250
contour,tasm2d_f(*,*),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=250
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,/vertical,position=[0.95,0.2,0.97,0.8],charsize=1.5,color=255,/pscolor

;TAS'
contour,land2d_m,xstyle=1,ystyle=1,title=VAR+' CORECTED '+month(im),position=[0.35,0.2,0.6,0.8],color=250
contour,tascm2d_f(*,*),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=250

;OBS'
contour,land2d_m,xstyle=1,ystyle=1,title='OBS '+month(im),position=[0.65,0.2,0.9,0.8],color=250
contour,obsm2d_f(*,*),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=250


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
contour,tasm2d_f(*,*),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
xyouts,lat(i),lon(i),'QUI',charthick=2,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,/vertical,position=[0.95,0.2,0.97,0.8],charsize=1.5,color=255

;TAS'
contour,land2d,xstyle=1,ystyle=1,title=VAR+' CORECTED '+month(im),position=[0.35,0.2,0.6,0.8]
contour,tascm2d_f(*,*),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

;OBS'
contour,land2d,xstyle=1,ystyle=1,title='OBS '+month(im),position=[0.65,0.2,0.9,0.8]
contour,obsm2d_f(*,*),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
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
END
