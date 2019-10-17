;
PRO make_T_stat
;
DEVICE,decompose=0
;
;
;-----------------------------------------------------------------------------------------------------------


;DATA= 'OBS'
;DATA= 'ENS_AVE'

;DATA= 'MOD'
DATA= 'CORR'

MODEL_ID=['C4IRCA3','DMI-HIRHAM5','DMI-HIRHAM5','ETHZ-CLM','KNMI-RACMO2','MPI-M-REMO','SMHIRCA','SMHIRCA']
CASE_ID=['A1B_HadCM3Q16','A1B_ARPEGE','A1B_ECHAM5','SCN_HadCM3Q0','A1B_ECHAM5-r3','SCN_ECHAM5','A1B_BCM','A1B_ECHAM5-r3']
nmod=1.*N_ELEMENTS(MODEL_ID)

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


;====================================================
;ENSE AVERAGE and COMMON GRID
;LOOP OVER MONTHS
IF MODEL_ID(imd) EQ 'ENS_AVE' THEN BEGIN
ofile='DATA/ENS_AVE_MOD_'+VAR+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(0)+'.dat'
print,'OPENING',ofile
restore,filename=ofile
;nxm,nym,landmask_com
ENDIF ELSE BEGIN
ofile='DATA/MOD_'+VAR+'_'+MODEL_ID(imd)+'_'+CASE_ID(imd)+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(0)+'.dat'
restore,filename=ofile
print,'OPENING',ofile
;mnx,mny,landmask_c
nxm=mnx
nym=mny
ENDELSE

;-----------------------------------------------------------------------------------------------------------
tas2d_m = FLTARR(nxm,nym,3*31.*NYEARS)
tas2d_m(*,*,*) = !VALUES.F_NAN
tasc2d_m = FLTARR(nxm,nym,3*31.*NYEARS)
tasc2d_m(*,*,*) = !VALUES.F_NAN
obs2d_m = FLTARR(nxm,nym,3*31.*NYEARS)
obs2d_m(*,*,*) = !VALUES.F_NAN

ave_tas2d_sm=fltarr(nxm,nym,4) ;SEASONAL AVERAGE
ave_tasc2d_sm=fltarr(nxm,nym,4)
ave_obs2d_sm=fltarr(nxm,nym,4)
ave_tasc2d_sm(*,*,*)=!VALUES.F_NAN
ave_tasc2d_sm(*,*,*)=!VALUES.F_NAN
ave_obs2d_sm(*,*,*)=!VALUES.F_NAN

sd_tas2d_sm=fltarr(nxm,nym,4) ;STANDARD DEVIATION
sd_tasc2d_sm=fltarr(nxm,nym,4)
sd_obs2d_sm=fltarr(nxm,nym,4)
sd_tas2d_sm(*,*,*)=!VALUES.F_NAN
sd_tasc2d_sm(*,*,*)=!VALUES.F_NAN
sd_obs2d_sm(*,*,*)=!VALUES.F_NAN

s25_tas2d_sm=fltarr(nxm,nym)  ;SUMMER DAYS (=n of days when Tmean > 25 C)
s25_tasc2d_sm=fltarr(nxm,nym)
s25_obs2d_sm=fltarr(nxm,nym)
s25_tas2d_sm(*,*)=-999.
s25_tasc2d_sm(*,*)=-999.
s25_obs2d_sm(*,*)=-999.

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

IF MODEL_ID EQ 'ENS_AVE' THEN BEGIN
ofile='DATA/ENS_AVE_MOD_'+VAR+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'  
;tas2d_ave,nxm,nym,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
tas2d_m(*,*,310*imm:310*imm+mndays-1)=tas2d_ave
ENDIF ELSE BEGIN
ofile='DATA/MOD_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;tas2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
tas2d_m(*,*,310*imm:310*imm+mndays-1)=tas2d
ENDELSE

IF MODEL_ID EQ 'ENS_AVE' THEN  BEGIN
ofile='DATA/ENS_AVE_MOD_'+VAR+'_CORR_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'  
;tas2d_ave,nxm,nym,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
tasc2d_m(*,*,310*imm:310*imm+mndays-1)=tasc2d_ave
ENDIF ELSE BEGIN
ofile='DATA/MOD_BC_'+VAR+'_CORR_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;tasc2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
tasc2d_m(*,*,310*imm:310*imm+mndays-1)=tasc2d
ENDELSE

;================
;OBS
print,'OPENING MONTH ',month(im),' MODEL OBS ',MODEL_OBS
ofile='DATA/OBS_'+VAR+'_'+MODEL_OBS+'_'+CASE_OBS+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;obs2d_ave,nxm,nym,landmask_c,ndays
restore,filename=ofile
obs2d_m(*,*,310*imm:310*imm+mndays-1)=obs2d

imm=imm+1

IF imm GT 2 THEN imm=0
ENDFOR  ;END im

;SEASONAL AVERAGES
print,'CALCULATING STANDARD DEV FOR SEASON ',ism,' MONTH ',im,jm
;TAS
FOR ix=0,nxm-1 DO BEGIN
FOR iy=0,nym-1 DO BEGIN
IF land2d_m(ix,iy) EQ 1 THEN BEGIN
ave_tas2d_sm(ix,iy,ism)=MEAN(tas2d_m(ix,iy,*) , /NAN ) 
sd_tas2d_sm(ix,iy,ism)=STDDEV(tas2d_m(ix,iy,*) , /NAN ) 
pdf_tas2d_sm(ix,iy,ism,*)=HISTOGRAM(REFORM(tas2d_m(ix,iy,*)),min=mint,max=maxt,binsize=binsz,/NaN)
ENDIF
ENDFOR
ENDFOR

;TASC
FOR ix=0,nxm-1 DO BEGIN
FOR iy=0,nym-1 DO BEGIN
IF land2d_m(ix,iy) EQ 1 THEN BEGIN
ave_tasc2d_sm(ix,iy,ism)=MEAN(tasc2d_m(ix,iy,*) , /NAN ) 
sd_tasc2d_sm(ix,iy,ism)=STDDEV(tasc2d_m(ix,iy,*) , /NAN ) 
pdf_tasc2d_sm(ix,iy,ism,*)=HISTOGRAM(REFORM(tasc2d_m(ix,iy,*)),min=mint,max=maxt,binsize=binsz,/NaN)
ENDIF
ENDFOR
ENDFOR

;OBS
FOR ix=0,nxm-1 DO BEGIN
FOR iy=0,nym-1 DO BEGIN
IF land2d_m(ix,iy) EQ 1 THEN BEGIN
ave_obs2d_sm(ix,iy,ism)=MEAN(obs2d_m(ix,iy,*) , /NAN ) 
sd_obs2d_sm(ix,iy,ism)=STDDEV(obs2d_m(ix,iy,*) , /NAN ) 
pdf_obs2d_sm(ix,iy,ism,*)=HISTOGRAM(REFORM(obs2d_m(ix,iy,*)),min=mint,max=maxt,binsize=binsz,/NaN)
ENDIF
ENDFOR
ENDFOR

IF ism EQ 2 THEN BEGIN &$ ;ONLY FOR SUMMER
FOR ix=0,nxm-1 DO BEGIN &$
FOR iy=0,nym-1 DO BEGIN &$
IF land2d_m(ix,iy) EQ 1 THEN BEGIN &$
cc=WHERE(tas2d_m(ix,iy,*) GE 273.15+25	,count) &$
s25_tas2d_sm(ix,iy)=count &$
cc=WHERE(tasc2d_m(ix,iy,*) GE 273.15+25	,count) &$
s25_tasc2d_sm(ix,iy)=count &$
cc=WHERE(obs2d_m(ix,iy,*) GE 273.15+25	,count) &$
s25_obs2d_sm(ix,iy)=count &$
ENDIF &$
ENDFOR &$
ENDFOR &$
ENDIF

ism=ism+1

ENDFOR ; END LOOP MONTHS


;=======================================
;PLOTS
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

window,0,retain=2
!p.multi=[0,4,3]
;TAS'
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' DJF',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,p_ave_tas2d_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' MAM',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,p_ave_tas2d_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' JJA',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,p_ave_tas2d_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' SON',charsize=1.5;,position=[0.5,0.05,0.9,0.45]
contour,p_ave_tas2d_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1,color=25,position=[0.96,0.55,0.99,0.95],/vertical

;TASC
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORRECTED DJF',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,p_ave_tasc2d_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORRECTED MAM',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,p_ave_tasc2d_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORRECTED JJA',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,p_ave_tasc2d_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORRECTED SON',charsize=1.5;,position=[0.5,0.05,0.9,0.45]
contour,p_ave_tasc2d_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

;OBS
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS DJF',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,p_ave_obs2d_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS MAM',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,p_ave_obs2d_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS JJA',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,p_ave_obs2d_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS SON',charsize=1.5;,position=[0.5,0.05,0.9,0.45]
contour,p_ave_obs2d_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

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
!p.multi=[0,4,3]
;TAS'
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' DJF',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,p_sd_tas2d_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' MAM',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,p_sd_tas2d_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' JJA',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,p_sd_tas2d_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' SON',charsize=1.5;,position=[0.5,0.05,0.9,0.45]
contour,p_sd_tas2d_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1,color=25,position=[0.96,0.55,0.99,0.95],/vertical

;TAS CORR'
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORRECTED DJF',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,p_sd_tasc2d_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORRECTED MAM',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,p_sd_tasc2d_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORRECTED JJA',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,p_sd_tasc2d_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORRECTED SON',charsize=1.5;,position=[0.5,0.05,0.9,0.45]
contour,p_sd_tasc2d_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

;OBS'
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS DJF',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,p_sd_obs2d_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS MAM',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,p_sd_obs2d_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS JJA',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,p_sd_obs2d_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS SON',charsize=1.5;,position=[0.5,0.05,0.9,0.45]
contour,p_sd_obs2d_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

;SUMMER DAYS
loadct,39
!p.background=255
!p.color=0
levels=findgen(10)*9.
colors=findgen(10)*24+20

levels_p=findgen(11)/10.
colors_p=(findgen(11)*22+30)
levels_p=[0,0.2,0.4,0.6,0.8,1]
colors_p=(findgen(5)*42+50)
colors_p=[50.0000 ,    92.0000   ,  134.000   ,  186.000   ,  218.000]

window,6,retain=2
!p.multi=[0,3,2]
;TAS CORR'
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS MOD',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,s25_tas2d_sm(*,*)/NYEARS,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS CORR',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,s25_tasc2d_sm(*,*)/NYEARS,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS OBS',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,s25_obs2d_sm(*,*)/NYEARS,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=min(levels),max=max(levels),divisions=N_ELEMENTS(levels-1),c_colors=colors,charsize=1,color=25,position=[0.97,0.55,0.99,0.95],/vertical


contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='PROB No OF SUMMER DAYS MOD',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,s25_tas2d_sm(*,*)/(NDAYS_S(2)*NYEARS),levels=levels_p,/fill,c_colors=colors_p,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='PROB No OF SUMMER DAYS CORR',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,s25_tasc2d_sm(*,*)/(NDAYS_S(2)*NYEARS),levels=levels_p,/fill,c_colors=colors_p,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='PROB No OF SUMMER DAYS OBS',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,s25_obs2d_sm(*,*)/(NDAYS_S(2)*NYEARS),levels=levels_p,/fill,c_colors=colors_p,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=0,max=1,divisions=5,c_colors=colors_p,charsize=1,color=25,position=[0.97,0.05,0.99,0.45],/vertical


;====================
;MASKS PDF

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
Mx_tas2d_m = FLTARR(nxm,nym,3*31.*NYEARS) ;TMAX
Mx_tas2d_m(*,*,*) = !VALUES.F_NAN
Mx_tasc2d_m = FLTARR(nxm,nym,3*31.*NYEARS)
Mx_tasc2d_m(*,*,*) = !VALUES.F_NAN
Mx_obs2d_m = FLTARR(nxm,nym,3*31.*NYEARS)
Mx_obs2d_m(*,*,*) = !VALUES.F_NAN

mn_tas2d_m = FLTARR(nxm,nym,3*31.*NYEARS) ;Tmin
mn_tas2d_m(*,*,*) = !VALUES.F_NAN
mn_tasc2d_m = FLTARR(nxm,nym,3*31.*NYEARS)
mn_tasc2d_m(*,*,*) = !VALUES.F_NAN
mn_obs2d_m = FLTARR(nxm,nym,3*31.*NYEARS)
mn_obs2d_m(*,*,*) = !VALUES.F_NAN

dr_tas2d_sm=fltarr(nxm,nym,4)  ;DIURNAL RANGE
dr_tasc2d_sm=fltarr(nxm,nym,4)
dr_obs2d_sm=fltarr(nxm,nym,4)
dr_tas2d_sm(*,*,*)=-999.
dr_tasc2d_sm(*,*,*)=-999.
dr_obs2d_sm(*,*,*)=-999.

ss25_tas2d_sm=fltarr(nxm,nym)  ;SUMMER DAYS (=n of days when TMax > 25 C)
ss25_tasc2d_sm=fltarr(nxm,nym)
ss25_obs2d_sm=fltarr(nxm,nym)
ss25_tas2d_sm(*,*)=-999.
ss25_tasc2d_sm(*,*)=-999.
ss25_obs2d_sm(*,*)=-999.

ss35_tas2d_sm=fltarr(nxm,nym)  ;SUMMER DAYS (=n of days when TMax > 35 C)
ss35_tasc2d_sm=fltarr(nxm,nym)
ss35_obs2d_sm=fltarr(nxm,nym)
ss35_tas2d_sm(*,*)=-999.
ss35_tasc2d_sm(*,*)=-999.
ss35_obs2d_sm(*,*)=-999.

ssm20_tas2d_sm=fltarr(nxm,nym)  ;SUMMER DAYS (=n of days when Tmin > 20 C)
ssm20_tasc2d_sm=fltarr(nxm,nym)
ssm20_obs2d_sm=fltarr(nxm,nym)
ssm20_tas2d_sm(*,*)=-999.
ssm20_tasc2d_sm(*,*)=-999.
ssm20_obs2d_sm(*,*)=-999.

ssm25_tas2d_sm=fltarr(nxm,nym)  ;SUMMER DAYS (=n of days when Tmin > 25 C)
ssm25_tasc2d_sm=fltarr(nxm,nym)
ssm25_obs2d_sm=fltarr(nxm,nym)
ssm25_tas2d_sm(*,*)=-999.
ssm25_tasc2d_sm(*,*)=-999.
ssm25_obs2d_sm(*,*)=-999.

;LOOP OVER MONTHS
ism=0
FOR jm=0,11,3 DO BEGIN ;STARTING MONTH IN SEASON
imm=0
FOR im=jm,jm+2 DO BEGIN ; MONTH IN YEAR

PRINT,'SEASON = ',ism
PRINT,'MONTH IN YEAR im = ',im
PRINT,'MONTH IN SEASON imm = ',imm


IF MODEL_ID EQ 'ENS_AVE' THEN BEGIN
ofile='DATA/ENS_AVE_MOD_Tmax_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'  
;tas2d_ave,nxm,nym,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
Mx_tas2d_m(*,*,310*imm:310*imm+mndays-1)=tas2d_ave
ofile='DATA/ENS_AVE_MOD_Tmin_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat' 
print,'RESTORING '+ofile
restore,filename=ofile
mn_tas2d_m(*,*,310*imm:310*imm+mndays-1)=tas2d_ave
ENDIF ELSE BEGIN 
ofile='DATA/MOD_Tmax_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;tas2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
Mx_tas2d_m(*,*,310*imm:310*imm+mndays-1)=tas2d
ofile='DATA/MOD_Tmin_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
print,'RESTORING '+ofile
restore,filename=ofile
mn_tas2d_m(*,*,310*imm:310*imm+mndays-1)=tas2d
ENDELSE

IF MODEL_ID EQ 'ENS_AVE' THEN BEGIN
ofile='DATA/ENS_AVE_MOD_Tmax_CORR_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'  
print,'RESTORING '+ofile
restore,filename=ofile
Mx_tasc2d_m(*,*,310*imm:310*imm+mndays-1)=tasc2d_ave
ofile='DATA/ENS_AVE_MOD_Tmin_CORR_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat' 
print,'RESTORING '+ofile
restore,filename=ofile
mn_tasc2d_m(*,*,310*imm:310*imm+mndays-1)=tasc2d_ave
;tas2d_ave,nxm,nym,landmask_c,mndays
ENDIF ELSE BEGIN 
ofile='DATA/MOD_BC_Tmax_CORR_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
print,'RESTORING '+ofile
restore,filename=ofile
Mx_tasc2d_m(*,*,310*imm:310*imm+mndays-1)=tasc2d
;tasc2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
ofile='DATA/MOD_BC_Tmin_CORR_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
print,'RESTORING '+ofile
restore,filename=ofile
mn_tasc2d_m(*,*,310*imm:310*imm+mndays-1)=tasc2d
ENDELSE

IF MODEL_ID EQ 'ENS_AVE' THEN  BEGIN
ofile='DATA/ENS_AVE_OBS_Tmax_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;obs2d_ave,nxm,nym,landmask_c,ndays
print,'RESTORING '+ofile
restore,filename=ofile
Mx_obs2d_m(*,*,310*imm:310*imm+mndays-1)=obs2d_ave
ofile='DATA/ENS_AVE_OBS_Tmin_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;obs2d_ave,nxm,nym,landmask_c,ndays
restore,filename=ofile
print,'RESTORING '+ofile
mn_obs2d_m(*,*,310*imm:310*imm+mndays-1)=obs2d_ave
ENDIF ELSE BEGIN
ofile='DATA/OBS_Tmax_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;obs2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
restore,filename=ofile
print,'RESTORING '+ofile
Mx_obs2d_m(*,*,310*imm:310*imm+mndays-1)=obs2d
ofile='DATA/OBS_Tmin_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;obs2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
nxm=mnx
nym=mny
mn_obs2d_m(*,*,310*imm:310*imm+mndays-1)=obs2d
ENDELSE


imm=imm+1

IF imm GT 2 THEN imm=0
ENDFOR  ;END im

FOR ix=0,nxm-1 DO BEGIN &$
FOR iy=0,nym-1 DO BEGIN &$
IF land2d_m(ix,iy) EQ 1 THEN BEGIN &$
dr_tas2d_sm(ix,iy,ism)=MEAN(Mx_tas2d_m(ix,iy,*)-mn_tas2d_m(ix,iy,*),/NaN)
dr_tasc2d_sm(ix,iy,ism)=MEAN(Mx_tasc2d_m(ix,iy,*)-mn_tasc2d_m(ix,iy,*),/NaN)
dr_obs2d_sm(ix,iy,ism)=MEAN(Mx_obs2d_m(ix,iy,*)-mn_obs2d_m(ix,iy,*),/NaN)
ENDIF
ENDFOR
ENDFOR

IF ism EQ 2 THEN BEGIN &$ ;ONLY FOR SUMMER
FOR ix=0,nxm-1 DO BEGIN &$
FOR iy=0,nym-1 DO BEGIN &$
IF land2d_m(ix,iy) EQ 1 THEN BEGIN &$
cc=WHERE(Mx_tas2d_m(ix,iy,*) GE 273.15+25	,count) &$
ss25_tas2d_sm(ix,iy)=count
cc=WHERE(Mx_tas2d_m(ix,iy,*) GE 273.15+35	,count) &$
ss35_tas2d_sm(ix,iy)=count
cc=WHERE(mn_tas2d_m(ix,iy,*) GE 273.15+20	,count) &$
ssm20_tas2d_sm(ix,iy)=count
cc=WHERE(mn_tas2d_m(ix,iy,*) GE 273.15+25	,count) &$
ssm25_tas2d_sm(ix,iy)=count
cc=WHERE(Mx_tasc2d_m(ix,iy,*) GE 273.15+25	,count) &$
ss25_tasc2d_sm(ix,iy)=count
cc=WHERE(Mx_tasc2d_m(ix,iy,*) GE 273.15+35	,count) &$
ss35_tasc2d_sm(ix,iy)=count
cc=WHERE(mn_tasc2d_m(ix,iy,*) GE 273.15+20	,count) &$
ssm20_tasc2d_sm(ix,iy)=count
cc=WHERE(mn_tasc2d_m(ix,iy,*) GE 273.15+25	,count) &$
ssm25_tasc2d_sm(ix,iy)=count
cc=WHERE(Mx_obs2d_m(ix,iy,*) GE 273.15+25	,count) &$
ss25_obs2d_sm(ix,iy)=count
cc=WHERE(Mx_obs2d_m(ix,iy,*) GE 273.15+35	,count) &$
ss35_obs2d_sm(ix,iy)=count
cc=WHERE(mn_obs2d_m(ix,iy,*) GE 273.15+20	,count) &$
ssm20_obs2d_sm(ix,iy)=count
cc=WHERE(mn_obs2d_m(ix,iy,*) GE 273.15+25	,count) &$
ssm25_obs2d_sm(ix,iy)=count
ENDIF &$
ENDFOR &$
ENDFOR &$
ENDIF

ism=ism+1
ENDFOR ; END LOOP MONTHS


;PLOTS

MinCol=20
MaxCol=240
minvalue=0
Maxvalue=20
ContourDist=2.
rInterval=Maxvalue-Minvalue
iNumOfConts=FIX(rInterval/ContourDist)+1
rgrContValues = INDGEN(iNumOfConts)*ContourDist + MinValue
rgrColValues = INDGEN(iNumOfConts-1) * (MaxCol - MinCol) / (iNumOfConts-2) + MinCol

;DIURNAL RANGE
window,10,retain=2
!p.multi=[0,4,3]
;TAS'
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='DIURNAL RANGE '+VAR+' DJF',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,dr_tas2d_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' MAM',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,dr_tas2d_sm(*,*,1),levels=rgrContValues_b,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' JJA',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,dr_tas2d_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' SON',charsize=1.5;,position=[0.5,0.05,0.9,0.45]
contour,dr_tas2d_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1,color=25,position=[0.96,0.55,0.99,0.95],/vertical

;TASC
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='DIURNAL RANGE '+VAR+' CORRECTED DJF',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,dr_tasc2d_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORRECTED MAM',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,dr_tasc2d_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORRECTED JJA',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,dr_tasc2d_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORRECTED SON',charsize=1.5;,position=[0.5,0.05,0.9,0.45]
contour,dr_tasc2d_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

;OBS
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='DIURNAL RANGE '+VAR+' OBS DJF',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,dr_obs2d_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS MAM',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,dr_obs2d_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS JJA',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,dr_obs2d_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS SON',charsize=1.5;,position=[0.5,0.05,0.9,0.45]
contour,dr_obs2d_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

;SUMMER DAYS
loadct,39
!p.background=255
!p.color=0
levels=findgen(10)*9.
colors=findgen(10)*24+20

levels_p=findgen(11)/10.
colors_p=(findgen(11)*22+30)
levels_p=[0,0.2,0.4,0.6,0.8,1]
colors_p=(findgen(5)*42+50)
colors_p=[50.0000 ,    92.0000   ,  134.000   ,  186.000   ,  218.000]

window,11,retain=2
!p.multi=[0,3,2]
;MAX T > 25
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS TMAX > 25 MOD',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,ss25_tas2d_sm(*,*)/NYEARS,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS TMAX > 25 CORR',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,ss25_tasc2d_sm(*,*)/NYEARS,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS TMAX > 25 OBS',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,ss25_obs2d_sm(*,*)/NYEARS,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=min(levels),max=max(levels),divisions=N_ELEMENTS(levels-1),c_colors=colors,charsize=1,color=25,position=[0.97,0.55,0.99,0.95],/vertical

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='PROB No OF SUMMER DAYS TMAX > 25 MOD',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,ss25_tas2d_sm(*,*)/(NDAYS_S(2)*NYEARS),levels=levels_p,/fill,c_colors=colors_p,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='PROB No OF SUMMER DAYS TMAX > 25 CORR',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,ss25_tasc2d_sm(*,*)/(NDAYS_S(2)*NYEARS),levels=levels_p,/fill,c_colors=colors_p,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='PROB No OF SUMMER DAYS TMAX > 25 OBS',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,ss25_obs2d_sm(*,*)/(NDAYS_S(2)*NYEARS),levels=levels_p,/fill,c_colors=colors_p,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

window,12,retain=2
!p.multi=[0,3,2]
;MAX T > 35
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS TMAX > 35 MOD',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,ss35_tas2d_sm(*,*)/NYEARS,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS TMAX > 35 CORR',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,ss35_tasc2d_sm(*,*)/NYEARS,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS TMAX > 35 OBS',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,ss35_obs2d_sm(*,*)/NYEARS,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=min(levels),max=max(levels),divisions=N_ELEMENTS(levels-1),c_colors=colors,charsize=1,color=25,position=[0.97,0.55,0.99,0.95],/vertical

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='PROB No OF SUMMER DAYS TMAX > 35 MOD',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,ss35_tas2d_sm(*,*)/(NDAYS_S(2)*NYEARS),levels=levels_p,/fill,c_colors=colors_p,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='PROB No OF SUMMER DAYS TMAX > 35 CORR',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,ss35_tasc2d_sm(*,*)/(NDAYS_S(2)*NYEARS),levels=levels_p,/fill,c_colors=colors_p,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='PROB No OF SUMMER DAYS TMAX > 35 OBS',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,ss35_obs2d_sm(*,*)/(NDAYS_S(2)*NYEARS),levels=levels_p,/fill,c_colors=colors_p,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

window,13,retain=2
!p.multi=[0,3,2]
;MIN T > 20
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS Tmin > 20 MOD',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,ssm20_tas2d_sm(*,*)/NYEARS,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS Tmin > 20 CORR',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,ssm20_tasc2d_sm(*,*)/NYEARS,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS Tmin > 20 OBS',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,ssm20_obs2d_sm(*,*)/NYEARS,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=min(levels),max=max(levels),divisions=N_ELEMENTS(levels-1),c_colors=colors,charsize=1,color=25,position=[0.97,0.55,0.99,0.95],/vertical

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='PROB No OF SUMMER DAYS Tmin > 20 MOD',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,ssm20_tas2d_sm(*,*)/(NDAYS_S(2)*NYEARS),levels=levels_p,/fill,c_colors=colors_p,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='PROB No OF SUMMER DAYS Tmin > 20 CORR',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,ssm20_tasc2d_sm(*,*)/(NDAYS_S(2)*NYEARS),levels=levels_p,/fill,c_colors=colors_p,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='PROB No OF SUMMER DAYS Tmin > 20 OBS',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,ssm20_obs2d_sm(*,*)/(NDAYS_S(2)*NYEARS),levels=levels_p,/fill,c_colors=colors_p,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

window,14,retain=2
!p.multi=[0,3,2]
;MIN T > 25
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS Tmin > 25 MOD',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,ssm25_tas2d_sm(*,*)/NYEARS,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS Tmin > 25 CORR',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,ssm25_tasc2d_sm(*,*)/NYEARS,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS Tmin > 25 OBS',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,ssm25_obs2d_sm(*,*)/NYEARS,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=min(levels),max=max(levels),divisions=N_ELEMENTS(levels-1),c_colors=colors,charsize=1,color=25,position=[0.97,0.55,0.99,0.95],/vertical

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='PROB No OF SUMMER DAYS Tmin > 25 MOD',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,ssm25_tas2d_sm(*,*)/(NDAYS_S(2)*NYEARS),levels=levels_p,/fill,c_colors=colors_p,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='PROB No OF SUMMER DAYS Tmin > 25 CORR',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,ssm25_tasc2d_sm(*,*)/(NDAYS_S(2)*NYEARS),levels=levels_p,/fill,c_colors=colors_p,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='PROB No OF SUMMER DAYS Tmin > 25 OBS',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,ssm25_obs2d_sm(*,*)/(NDAYS_S(2)*NYEARS),levels=levels_p,/fill,c_colors=colors_p,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

STOP



END
STOP



END
