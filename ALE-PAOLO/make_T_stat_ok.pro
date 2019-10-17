;
PRO make_T_stat
;
DEVICE,decompose=0
;
;
;-----------------------------------------------------------------------------------------------------------

home='/media/disk/POSTPROC/BC_Regional/'

MODEL_ID='DMI-HIRHAM5'
CASE_ID='A1B_ECHAM5'

MODEL_ID='ETHZ-CLM'
CASE_ID='SCN_HadCM3Q0'

MODEL_ID='KNMI-RACMO2'
CASE_ID='A1B_ECHAM5-r3'

;VARIABLE (T,Ptot,Tmax,Tmin)
VAR='Ptot'
VAR='T'

CONSTRUCTION_PERIOD_START  =  '1961'
CONSTRUCTION_PERIOD_STOP   =  '1990'

APPLICATION_PERIOD_START   =  '1991'
APPLICATION_PERIOD_STOP    =  '2000'

NYEARS=fix(APPLICATION_PERIOD_STOP)-fix(APPLICATION_PERIOD_START)+1

month=['12','01','02','03','04','05','06','07','08','09','10','11']
NMONTHS=12
NDAYS=fltarr(12)
NDAYS_M=[31,31,28,31,30,31,30,31,31,30,31,30]
NDAYS_S=[31+31+28,31+30+31,30+31+31,30+31+30]

print,'OPENING MONTH ',month(0),' MODEL ',MODEL_ID+'_'+CASE_ID
IF MODEL_ID EQ 'ENS_AVE' THEN BEGIN
ofile='DATA/ENS_AVE_MOD_'+VAR+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(0)+'.dat'
;tas2d_ave,nxm,nym,landmask_c,mndays
print,'OPENING',ofile
restore,filename=ofile
ENDIF ELSE BEGIN
ofile='DATA/MOD_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(0)+'.dat'
;tas2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'OPENING',ofile
restore,filename=ofile
nxm=mnx
nym=mny
ENDELSE

land2d_m=landmask_c

;-----------------------------------------------------------------------------------------------------------
tas2d_m = FLTARR(nxm,nym,3,31.*NYEARS)
tas2d_m(*,*,*,*,*) = !VALUES.F_NAN
tasc2d_m = FLTARR(nxm,nym,3,31.*NYEARS)
tasc2d_m(*,*,*,*,*) = !VALUES.F_NAN
obs2d_m = FLTARR(nxm,nym,3,31.*NYEARS)
obs2d_m(*,*,*,*,*) = !VALUES.F_NAN

ave_tas2d_sm=fltarr(nxm,nym,4)
ave_tasc2d_sm=fltarr(nxm,nym,4)
ave_obs2d_sm=fltarr(nxm,nym,4)
ave_tas2d_sm(*,*,*)=!VALUES.F_NAN
ave_tasc2d_sm(*,*,*)=!VALUES.F_NAN
ave_obs2d_sm(*,*,*)=!VALUES.F_NAN

sd_tas2d_sm=fltarr(nxm,nym,4)
sd_tasc2d_sm=fltarr(nxm,nym,4)
sd_obs2d_sm=fltarr(nxm,nym,4)
sd_tas2d_sm(*,*,*)=!VALUES.F_NAN
sd_tasc2d_sm(*,*,*)=!VALUES.F_NAN
sd_obs2d_sm(*,*,*)=!VALUES.F_NAN

binsz=1
mint=230.
maxt=320.
nbins=(maxt-mint)/binsz+1
bins = FINDGEN(nbins) + mint

pdf_tas2d_sm=fltarr(nxm,nym,4,nbins)
pdf_tasc2d_sm=fltarr(nxm,nym,4,nbins)
pdf_obs2d_sm=fltarr(nxm,nym,4,nbins)
pdf_tas2d_sm(*,*,*,*)=!VALUES.F_NAN
pdf_tasc2d_sm(*,*,*,*)=!VALUES.F_NAN
pdf_obs2d_sm(*,*,*,*)=!VALUES.F_NAN

;LOOP OVER MONTHS
ism=0
FOR jm=0,11,3 DO BEGIN ;STARTING MONTH IN SEASON
imm=0
FOR im=jm,jm+2 DO BEGIN ; MONTH IN YEAR

PRINT,'SEASON = ',ism
PRINT,'MONTH IN YEAR im = ',im
PRINT,'MONTH IN SEASON imm = ',imm

;RESTORING DATA
;DATA FILES

IF MODEL_ID EQ 'ENS_AVE' THEN $
ofilie='DATA/ENS_AVE_MOD_'+VAR+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'  $
;tas2d_ave,nxm,nym,landmask_c,mndays
ELSE $
ofile='DATA/MOD_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;tas2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile

IF MODEL_ID EQ 'ENS_AVE' THEN $
ofilie='DATA/ENS_AVE_MOD_'+VAR+'_CORR_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'  $
;tas2d_ave,nxm,nym,landmask_c,mndays
ELSE $
ofile='DATA/MOD_BC_'+VAR+'_CORR_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;tasc2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile

IF MODEL_ID EQ 'ENS_AVE' THEN  BEGIN
ofilie='DATA/ENS_AVE_OBS_'+VAR+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;obs2d_ave,nxm,nym,landmask_c,ndays
restore,filename=ofile
print,'RESTORING '+ofile
ENDIF ELSE BEGIN
ofile='DATA/OBS_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;obs2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
restore,filename=ofile
print,'RESTORING '+ofile
nxm=mnx
nym=mny
ENDELSE

IF MODEL_ID EQ 'ENS_AVE' THEN BEGIN
tas2d_m(*,*,imm,0:MNDAYS-1)=tas2d_ave
tasc2d_m(*,*,imm,0:MNDAYS-1)=tasc2d_ave
obs2d_m(*,*,imm,0:MNDAYS-1)=obs2d_ave
ENDIF ELSE BEGIN
tas2d_m(*,*,imm,0:MNDAYS-1)=tas2d
tasc2d_m(*,*,imm,0:MNDAYS-1)=tasc2d
obs2d_m(*,*,imm,0:MNDAYS-1)=obs2d
ENDELSE

imm=imm+1

IF imm GT 2 THEN imm=0
ENDFOR  ;END im

tas_w=fltarr(nxm,nym,3*NYEARS*31)
;SEASONAL AVERAGES
print,'CALCULATING STANDARD DEV FOR SEASON ',ism,' MONTH ',im,jm
;TAS
tas_w(*,*,0:309)=REFORM(tas2d_m(*,*,0,*))
tas_w(*,*,310:619)=REFORM(tas2d_m(*,*,1,*))
tas_w(*,*,620:929)=REFORM(tas2d_m(*,*,2,*))
FOR ix=0,nxm-1 DO BEGIN
FOR iy=0,nym-1 DO BEGIN
IF land2d_m(ix,iy) EQ 1 THEN BEGIN
;tm=MEAN(tas_w(ix,iy,*),/NaN)
;ct=WHERE(tas_w(ix,iy,*) GE 0,count)
;sd_tas2d_sm(ix,iy,ism)=SQRT((1./count)*TOTAL((tas_w(ix,iy,*)-tm)^2.))
ave_tas2d_sm(ix,iy,ism)=MEAN(tas_w(ix,iy,*) , /NAN ) 
sd_tas2d_sm(ix,iy,ism)=STDDEV(tas_w(ix,iy,*) , /NAN ) 
pdf_tas2d_sm(ix,iy,ism,*)=HISTOGRAM(REFORM(tas_w(ix,iy,*)),min=mint,max=maxt,binsize=binsz,/NaN)
ENDIF
ENDFOR
ENDFOR

;TASC
tas_w=fltarr(nxm,nym,3*NYEARS*31)
tas_w(*,*,0:309)=REFORM(tasc2d_m(*,*,0,*))
tas_w(*,*,310:619)=REFORM(tasc2d_m(*,*,1,*))
tas_w(*,*,620:929)=REFORM(tasc2d_m(*,*,2,*))
FOR ix=0,nxm-1 DO BEGIN
FOR iy=0,nym-1 DO BEGIN
IF land2d_m(ix,iy) EQ 1 THEN BEGIN
;tm=MEAN(tas_w(ix,iy,*),/NaN)
;ct=WHERE(tas_w(ix,iy,*) GE 0,count)
;sd_tasc2d_sm(ix,iy,ism)=SQRT((1./count)*TOTAL((tas_w(ix,iy,*)-tm)^2.))
ave_tasc2d_sm(ix,iy,ism)=MEAN(tas_w(ix,iy,*) , /NAN ) 
sd_tasc2d_sm(ix,iy,ism)=STDDEV(tas_w(ix,iy,*) , /NAN ) 
pdf_tasc2d_sm(ix,iy,ism,*)=HISTOGRAM(REFORM(tas_w(ix,iy,*)),min=mint,max=maxt,binsize=binsz,/NaN)
ENDIF
ENDFOR
ENDFOR

;OBS
tas_w=fltarr(nxm,nym,3*NYEARS*31)
tas_w(*,*,0:309)=REFORM(obs2d_m(*,*,0,*))
tas_w(*,*,310:619)=REFORM(obs2d_m(*,*,1,*))
tas_w(*,*,620:929)=REFORM(obs2d_m(*,*,2,*))
FOR ix=0,nxm-1 DO BEGIN
FOR iy=0,nym-1 DO BEGIN
IF land2d_m(ix,iy) EQ 1 THEN BEGIN
;tm=MEAN(tas_w(ix,iy,*),/NaN)
;ct=WHERE(tas_w(ix,iy,*) GE 0,count)
;sd_obs2d_sm(ix,iy,ism)=SQRT((1./count)*TOTAL((tas_w(ix,iy,*)-tm)^2.))
ave_obs2d_sm(ix,iy,ism)=MEAN(tas_w(ix,iy,*) , /NAN ) 
sd_obs2d_sm(ix,iy,ism)=STDDEV(tas_w(ix,iy,*) , /NAN ) 
pdf_obs2d_sm(ix,iy,ism,*)=HISTOGRAM(REFORM(tas_w(ix,iy,*)),min=mint,max=maxt,binsize=binsz,/NaN)
ENDIF
ENDFOR
ENDFOR

ism=ism+1

ENDFOR ; END LOOP MONTHS


p_ave_tas2d_sm=fltarr(nxm,nym,4)
p_ave_tasc2d_sm=fltarr(nxm,nym,4)
p_ave_obs2d_sm=fltarr(nxm,nym,4)
p_ave_tas2d_sm(*,*,*)=-999.
p_ave_tasc2d_sm(*,*,*)=-999.
p_ave_obs2d_sm(*,*,*)=-999.

p_sd_tas2d_sm=fltarr(nxm,nym,4)
p_sd_tasc2d_sm=fltarr(nxm,nym,4)
p_sd_obs2d_sm=fltarr(nxm,nym,4)
p_sd_tas2d_sm(*,*,*)=-999.
p_sd_tasc2d_sm(*,*,*)=-999.
p_sd_obs2d_sm(*,*,*)=-999.

FOR id=0,3 do begin &$
FOR ix=0,nxm-1 do begin &$
FOR iy=0,nym-1 do begin &$
IF ave_tas2d_sm(ix,iy,id) GE 0 THEN p_ave_tas2d_sm(ix,iy,id)=ave_tas2d_sm(ix,iy,id) &$
IF ave_tasc2d_sm(ix,iy,id) GE 0 THEN p_ave_tasc2d_sm(ix,iy,id)=ave_tasc2d_sm(ix,iy,id) &$
IF ave_obs2d_sm(ix,iy,id) GE 0 THEN p_ave_obs2d_sm(ix,iy,id)=ave_obs2d_sm(ix,iy,id) &$
IF sd_tas2d_sm(ix,iy,id) GE 0 THEN p_sd_tas2d_sm(ix,iy,id)=sd_tas2d_sm(ix,iy,id) &$
IF sd_tasc2d_sm(ix,iy,id) GE 0 THEN p_sd_tasc2d_sm(ix,iy,id)=sd_tasc2d_sm(ix,iy,id) &$
IF sd_obs2d_sm(ix,iy,id) GE 0 THEN p_sd_obs2d_sm(ix,iy,id)=sd_obs2d_sm(ix,iy,id) &$
ENDFOR &$
ENDFOR &$
ENDFOR

;MEANS
IF VAR EQ 'Ptot' THEN BEGIN
MinCol=20
MaxCol=155
minvalue=0
Maxvalue=6
rInterval=Maxvalue-Minvalue
ContourDist=1
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
Maxvalue=310
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
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' DJF',position=[0.05,0.55,0.45,0.95]
contour,p_ave_tas2d_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' MAM',position=[0.5,0.55,0.9,0.95]
contour,p_ave_tas2d_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' JJA',position=[0.05,0.05,0.45,0.45]
contour,p_ave_tas2d_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' SON',position=[0.5,0.05,0.9,0.45]
contour,p_ave_tas2d_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1,color=25,position=[0.96,0.55,0.99,0.95],/vertical

window,1,retain=2
!p.multi=[0,2,2]
;TAS CORR'
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORRECTED DJF',position=[0.05,0.55,0.45,0.95]
contour,p_ave_tasc2d_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORRECTED MAM',position=[0.5,0.55,0.9,0.95]
contour,p_ave_tasc2d_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORRECTED JJA',position=[0.05,0.05,0.45,0.45]
contour,p_ave_tasc2d_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORRECTED SON',position=[0.5,0.05,0.9,0.45]
contour,p_ave_tasc2d_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1,color=25,position=[0.96,0.55,0.99,0.95],/vertical

window,2,retain=2
!p.multi=[0,2,2]
;OBS'
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS DJF',position=[0.05,0.55,0.45,0.95]
contour,p_ave_obs2d_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS MAM',position=[0.5,0.55,0.9,0.95]
contour,p_ave_obs2d_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS JJA',position=[0.05,0.05,0.45,0.45]
contour,p_ave_obs2d_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS SON',position=[0.5,0.05,0.9,0.45]
contour,p_ave_obs2d_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1,color=25,position=[0.96,0.55,0.99,0.95],/vertical




;ST DEV
MinCol=135
MaxCol=245
Minvalue=0
Maxvalue=10
ContourDist=1
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

window,3,retain=2
!p.multi=[0,2,2]
;TAS'
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' DJF',position=[0.05,0.55,0.45,0.95]
contour,p_sd_tas2d_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' MAM',position=[0.5,0.55,0.9,0.95]
contour,p_sd_tas2d_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' JJA',position=[0.05,0.05,0.45,0.45]
contour,p_sd_tas2d_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' SON',position=[0.5,0.05,0.9,0.45]
contour,p_sd_tas2d_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1,color=25,position=[0.96,0.55,0.99,0.95],/vertical

window,4,retain=2
!p.multi=[0,2,2]
;TAS CORR'
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORRECTED DJF',position=[0.05,0.55,0.45,0.95]
contour,p_sd_tasc2d_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORRECTED MAM',position=[0.5,0.55,0.9,0.95]
contour,p_sd_tasc2d_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORRECTED JJA',position=[0.05,0.05,0.45,0.45]
contour,p_sd_tasc2d_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORRECTED SON',position=[0.5,0.05,0.9,0.45]
contour,p_sd_tasc2d_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1,color=25,position=[0.96,0.55,0.99,0.95],/vertical

window,5,retain=2
!p.multi=[0,2,2]
;OBS'
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS DJF',position=[0.05,0.55,0.45,0.95]
contour,p_sd_obs2d_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS MAM',position=[0.5,0.55,0.9,0.95]
contour,p_sd_obs2d_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS JJA',position=[0.05,0.05,0.45,0.45]
contour,p_sd_obs2d_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS SON',position=[0.5,0.05,0.9,0.45]
contour,p_sd_obs2d_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1,color=25,position=[0.96,0.55,0.99,0.95],/vertical


nm=8
mask=['BI','IP','FR','ME','SC','AL','MD','EA','EU']
BI=[20,60,100,150]
IP=[0,45,40,80]
FR=[30,60,60,100]
ME=[60,100,85,115]
SC=[65,145,115,189]
AL=[60,100,65,85]
MD=[60,130,25,65]
EA=[100,145,65,115]
EU=[-15,60,35,75]

pdf_tas2d_sm_m=FLTARR(nm,4,nbins)
pdf_tasc2d_sm_m=FLTARR(nm,4,nbins)
pdf_obs2d_sm_m=FLTARR(nm,4,nbins)
pdf_tas2d_sm_m(*,*,*)=!VALUES.F_NAN
pdf_tasc2d_sm_m(*,*,*)=!VALUES.F_NAN
pdf_obs2d_sm_m(*,*,*)=!VALUES.F_NAN

FOR imm=0,nm-1 DO BEGIN ; LOOP OVER MASKS

print,'MASK: ',mask(imm)
ma=fltarr(4)
IF mask(imm) eq 'BI' then ma=BI
IF mask(imm) eq 'IP' then ma=IP
IF mask(imm) eq 'FR' then ma=FR
IF mask(imm) eq 'ME' then ma=ME
IF mask(imm) eq 'SC' then ma=SC
IF mask(imm) eq 'AL' then ma=AL
IF mask(imm) eq 'MD' then ma=MD
IF mask(imm) eq 'EA' then ma=EA

FOR ism=0,3 DO BEGIN 
ww=REFORM(pdf_tas2d_sm(ma(0):ma(1),ma(2):ma(3),ism,*))
wwc=REFORM(pdf_tasc2d_sm(ma(0):ma(1),ma(2):ma(3),ism,*))
wwo=REFORM(pdf_obs2d_sm(ma(0):ma(1),ma(2):ma(3),ism,*))
FOR ib=0,nbins-1 DO BEGIN 
pdf_tas2d_sm_m(imm,ism,ib)=MEAN(ww(*,*,ib),/NaN) 
pdf_tasc2d_sm_m(imm,ism,ib)=MEAN(wwc(*,*,ib),/NaN) 
pdf_obs2d_sm_m(imm,ism,ib)=MEAN(wwo(*,*,ib),/NaN) 
ENDFOR 
ENDFOR 

ENDFOR 

window,10,retain=2,xsize=1050,ysize=950
!p.multi=[0,4,8]
FOR imm=0,nm-1 DO BEGIN ; LOOP OVER MASKS

pdf_data=reform(pdf_tas2d_sm_m(imm,0,*))
pdf_datac=reform(pdf_tasc2d_sm_m(imm,0,*))
pdf_datao=reform(pdf_obs2d_sm_m(imm,0,*))
;PLOT, bins-273.15, pdf_datao/(NDAYS_S(0)*NYEARS),xrange=[-20,20],yrange=[0,0.16],xstyle=1,ystyle=1,title=mask(imm),thick=2
;OPLOT, bins-273.15, pdf_data/(NDAYS_S(0)*NYEARS),color=50,thick=2
;OPLOT, bins-273.15, pdf_datac/(NDAYS_S(0)*NYEARS),color=200,thick=2
PLOT, bins-273.15, pdf_datao/(NDAYS_S(0)*NYEARS),xrange=[-20,20],yrange=[0,0.16],xstyle=1,ystyle=1,title=mask(imm),thick=2
OPLOT, bins-273.15, pdf_data/(NDAYS_S(0)*NYEARS),color=50,thick=2
OPLOT, bins-273.15, pdf_datac/(NDAYS_S(0)*NYEARS),color=200,thick=2

pdf_data=reform(pdf_tas2d_sm_m(imm,1,*))
pdf_datac=reform(pdf_tasc2d_sm_m(imm,1,*))
pdf_datao=reform(pdf_obs2d_sm_m(imm,1,*))
PLOT, bins-273.15, pdf_datao/(NDAYS_S(1)*NYEARS),xrange=[-10,30],yrange=[0,0.16],xstyle=1,ystyle=1,title=mask(imm),thick=2
OPLOT, bins-273.15, pdf_data/(NDAYS_S(1)*NYEARS),color=50,thick=2
OPLOT, bins-273.15, pdf_datac/(NDAYS_S(1)*NYEARS),color=200,thick=2

pdf_data=reform(pdf_tas2d_sm_m(imm,2,*))
pdf_datac=reform(pdf_tasc2d_sm_m(imm,2,*))
pdf_datao=reform(pdf_obs2d_sm_m(imm,2,*))
PLOT, bins-273.15, pdf_datao/(NDAYS_S(2)*NYEARS),xrange=[0,40],yrange=[0,0.16],xstyle=1,ystyle=1,title=mask(imm),thick=2
OPLOT, bins-273.15, pdf_data/(NDAYS_S(2)*NYEARS),color=50,thick=2
OPLOT, bins-273.15, pdf_datac/(NDAYS_S(2)*NYEARS),color=200,thick=2

pdf_data=reform(pdf_tas2d_sm_m(imm,3,*))
pdf_datac=reform(pdf_tasc2d_sm_m(imm,3,*))
pdf_datao=reform(pdf_obs2d_sm_m(imm,3,*))
PLOT, bins-273.15, pdf_datao/(NDAYS_S(3)*NYEARS),xrange=[-10,30],yrange=[0,0.16],xstyle=1,ystyle=1,title=mask(imm),thick=2
OPLOT, bins-273.15, pdf_data/(NDAYS_S(3)*NYEARS),color=50,thick=2
OPLOT, bins-273.15, pdf_datac/(NDAYS_S(3)*NYEARS),color=200,thick=2

ENDFOR


;=============================
;DIURNAL RANGEE
;RESTORING DATA
;DATA FILES
tas2d_m = FLTARR(nxm,nym,3,31.*NYEARS)
tas2d_m(*,*,*,*,*) = !VALUES.F_NAN
tasc2d_m = FLTARR(nxm,nym,3,31.*NYEARS)
tasc2d_m(*,*,*,*,*) = !VALUES.F_NAN
obs2d_m = FLTARR(nxm,nym,3,31.*NYEARS)
obs2d_m(*,*,*,*,*) = !VALUES.F_NAN

ave_tas2d_sm=fltarr(nxm,nym,4)
ave_tasc2d_sm=fltarr(nxm,nym,4)
ave_obs2d_sm=fltarr(nxm,nym,4)
ave_tas2d_sm(*,*,*)=!VALUES.F_NAN
ave_tasc2d_sm(*,*,*)=!VALUES.F_NAN
ave_obs2d_sm(*,*,*)=!VALUES.F_NAN

;LOOP OVER MONTHS
ism=0
;FOR jm=0,11,3 DO BEGIN ;STARTING MONTH IN SEASON
imm=0
;FOR im=jm,jm+2 DO BEGIN ; MONTH IN YEAR

PRINT,'SEASON = ',ism
PRINT,'MONTH IN YEAR im = ',im
PRINT,'MONTH IN SEASON imm = ',imm


IF MODEL_ID EQ 'ENS_AVE' THEN BEGIN
ofilie='DATA/ENS_AVE_MOD_Tmax_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'  
;tas2d_ave,nxm,nym,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
tas2d_m(*,*,imm,0:MNDAYS-1)=tas2d_ave
ofilie='DATA/ENS_AVE_MOD_Tmin_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'  
print,'RESTORING '+ofile
restore,filename=ofile
tas2d_m(*,*,imm,0:MNDAYS-1)=tas2d_ave
ENDIF ELSE BEGIN 
ofile='DATA/MOD_Tmax_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;tas2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
ofile='DATA/MOD_Tmin_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
print,'RESTORING '+ofile
restore,filename=ofile
ENDELSE

IF MODE_ID EQ 'ENS_AVE' THEN  BEGIN
ofilie='DATA/ENS_AVE_MOD_Tmax_CORR_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'  
print,'RESTORING '+ofile
restore,filename=ofile
ofilie='DATA/ENS_AVE_MOD_Tmin_CORR_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat' 
print,'RESTORING '+ofile
restore,filename=ofile
;tas2d_ave,nxm,nym,landmask_c,mndays
ENDIF ELSE BEGIN
ofile='DATA/MOD_BC_Tmax_CORR_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
print,'RESTORING '+ofile
restore,filename=ofile
;tasc2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
ofile='DATA/MOD_BC_Tmin_CORR_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
print,'RESTORING '+ofile
restore,filename=ofile
ENDELSE

IF MODEL_ID EQ 'ENS_AVE' THEN  BEGIN
ofilie='DATA/ENS_AVE_OBS_Tmax_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;obs2d_ave,nxm,nym,landmask_c,ndays
restore,filename=ofile
print,'RESTORING '+ofile
ofilie='DATA/ENS_AVE_OBS_Tmin_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;obs2d_ave,nxm,nym,landmask_c,ndays
restore,filename=ofile
ENDIF ELSE BEGIN
ofile='DATA/OBS_Tmax_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;obs2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
restore,filename=ofile
print,'RESTORING '+ofile
ofile='DATA/OBS_Tmin_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;obs2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
restore,filename=ofile
nxm=mnx
nym=mny
ENDELSE

IF MODEL_ID EQ 'ENS_AVE' THEN BEGIN
tas2d_m(*,*,imm,0:MNDAYS-1)=tas2d_ave
tasc2d_m(*,*,imm,0:MNDAYS-1)=tasc2d_ave
obs2d_m(*,*,imm,0:MNDAYS-1)=obs2d_ave
ENDIF ELSE BEGIN
tas2d_m(*,*,imm,0:MNDAYS-1)=tas2d
tasc2d_m(*,*,imm,0:MNDAYS-1)=tasc2d
obs2d_m(*,*,imm,0:MNDAYS-1)=obs2d
ENDELSE


STOP
END
