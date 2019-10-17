;
PRO make_T_stat_scen
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

APPLICATION_PERIOD_START_1   =  '1991'
APPLICATION_PERIOD_STOP_1    =  '2000'

APPLICATION_PERIOD_START_2   =  '2081'
APPLICATION_PERIOD_STOP_2    =  '2090'

NYEARS=fix(APPLICATION_PERIOD_STOP_1)-fix(APPLICATION_PERIOD_START_1)+1

;ns=4
;season=['DJF','MAM','JJA','SON']

month=['12','01','02','03','04','05','06','07','08','09','10','11']
NMONTHS=12
NDAYS=fltarr(12)
NDAYS_M=[31,31,28,31,30,31,30,31,31,30,31,30]
NDAYS_S=[31+31+28,31+30+31,30+31+31,30+31+30]

print,'OPENING MONTH ',month(0),' MODEL ',MODEL_ID+'_'+CASE_ID
IF MODEL_ID EQ 'ENS_AVE' THEN BEGIN
ofile='DATA/ENS_AVE_MOD_'+VAR+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START_1+'_'+APPLICATION_PERIOD_STOP_1+'_'+month(0)+'.dat'
;tas2d_ave,nxm,nym,landmask_c,mndays
print,'OPENING ',ofile
restore,filename=ofile
ENDIF ELSE BEGIN
ofile='DATA/MOD_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START_1+'_'+APPLICATION_PERIOD_STOP_1+'_'+month(0)+'.dat'
;tas2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'OPENING ',ofile
restore,filename=ofile
nxm=mnx
nym=mny
ENDELSE

land2d_m=landmask_c

;-----------------------------------------------------------------------------------------------------------
tas2d_m_1 = FLTARR(nxm,nym,3*31*NYEARS)
tas2d_m_1(*,*,*,*) = !VALUES.F_NAN
tasc2d_m_1 = FLTARR(nxm,nym,3*31*NYEARS)
tasc2d_m_1(*,*,*,*) = !VALUES.F_NAN

tas2d_m_2 = FLTARR(nxm,nym,3*31*NYEARS)
tas2d_m_2(*,*,*,*) = !VALUES.F_NAN
tasc2d_m_2 = FLTARR(nxm,nym,3*31*NYEARS)
tasc2d_m_2(*,*,*,*) = !VALUES.F_NAN

ave_tas2d_sm_1=fltarr(nxm,nym,4)
ave_tas2d_sm_1(*,*,*)=!VALUES.F_NAN
ave_tasc2d_sm_1=fltarr(nxm,nym,4)
ave_tasc2d_sm_1(*,*,*)=!VALUES.F_NAN

ave_tas2d_sm_2=fltarr(nxm,nym,4)
ave_tas2d_sm_2(*,*,*)=!VALUES.F_NAN
ave_tasc2d_sm_2=fltarr(nxm,nym,4)
ave_tasc2d_sm_2(*,*,*)=!VALUES.F_NAN

sd_tas2d_sm_1=fltarr(nxm,nym,4)
sd_tas2d_sm_1(*,*,*)=!VALUES.F_NAN
sd_tasc2d_sm_1=fltarr(nxm,nym,4)
sd_tasc2d_sm_1(*,*,*)=!VALUES.F_NAN

sd_tas2d_sm_2=fltarr(nxm,nym,4)
sd_tas2d_sm_2(*,*,*)=!VALUES.F_NAN
sd_tasc2d_sm_2=fltarr(nxm,nym,4)
sd_tasc2d_sm_2(*,*,*)=!VALUES.F_NAN

s25_tas2d_sm_1=fltarr(nxm,nym)  ;SUMMER DAYS (=n of days when Tmean > 25 C)
s25_tasc2d_sm_1=fltarr(nxm,nym)
s25_tas2d_sm_1(*,*)=-999.
s25_tasc2d_sm_1(*,*)=-999.
s25_tas2d_sm_2=fltarr(nxm,nym)  ;SUMMER DAYS (=n of days when Tmean > 25 C)
s25_tasc2d_sm_2=fltarr(nxm,nym)
s25_tas2d_sm_2(*,*)=-999.
s25_tasc2d_sm_2(*,*)=-999.


binsz=1
mint=230.
maxt=320.
nbins=(maxt-mint)/binsz+1
bins = FINDGEN(nbins) + mint

pdf_tas2d_sm_1=fltarr(nxm,nym,4,nbins)
pdf_tasc2d_sm_1=fltarr(nxm,nym,4,nbins)
pdf_tas2d_sm_1(*,*,*,*)=!VALUES.F_NAN
pdf_tasc2d_sm_1(*,*,*,*)=!VALUES.F_NAN

pdf_tas2d_sm_2=fltarr(nxm,nym,4,nbins)
pdf_tasc2d_sm_2=fltarr(nxm,nym,4,nbins)
pdf_tas2d_sm_2(*,*,*,*)=!VALUES.F_NAN
pdf_tasc2d_sm_2(*,*,*,*)=!VALUES.F_NAN

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

print,'PERIOD 1: ',APPLICATION_PERIOD_START_1,'-',+APPLICATION_PERIOD_STOP_1
IF MODEL_ID EQ 'ENS_AVE' THEN BEGIN 
ofile='DATA/ENS_AVE_MOD_'+VAR+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START_1+'_'+APPLICATION_PERIOD_STOP_1+'_'+month(im)+'.dat'  
;tas2d_ave,nxm,nym,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
tas2d_m_1(*,*,310*imm:310*imm+mndays-1)=tas2d_ave
ENDIF ELSE BEGIN 
ofile='DATA/MOD_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START_1+'_'+APPLICATION_PERIOD_STOP_1+'_'+month(im)+'.dat'
;tas2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
tas2d_m_1(*,*,310*imm:310*imm+mndays-1)=tas2d
ENDELSE

IF MODEL_ID EQ 'ENS_AVE' THEN BEGIN
ofile='DATA/ENS_AVE_MOD_'+VAR+'_CORR_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START_1+'_'+APPLICATION_PERIOD_STOP_1+'_'+month(im)+'.dat'  
;tas2d_ave,nxm,nym,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
tasc2d_m_1(*,*,310*imm:310*imm+mndays-1)=tasc2d_ave
ENDIF ELSE BEGIN
ofile='DATA/MOD_BC_'+VAR+'_CORR_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START_1+'_'+APPLICATION_PERIOD_STOP_1+'_'+month(im)+'.dat'
;tasc2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
tasc2d_m_1(*,*,310*imm:310*imm+mndays-1)=tasc2d
ENDELSE

print,'PERIOD 2: ',APPLICATION_PERIOD_START_2,'-',+APPLICATION_PERIOD_STOP_2
IF MODEL_ID EQ 'ENS_AVE' THEN BEGIN 
ofile='DATA/ENS_AVE_MOD_'+VAR+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START_2+'_'+APPLICATION_PERIOD_STOP_2+'_'+month(im)+'.dat'  
;tas2d_ave,nxm,nym,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
tas2d_m_2(*,*,310*imm:310*imm+mndays-1)=tas2d_ave
ENDIF ELSE BEGIN 
ofile='DATA/MOD_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START_2+'_'+APPLICATION_PERIOD_STOP_2+'_'+month(im)+'.dat'
;tas2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
tas2d_m_2(*,*,310*imm:310*imm+mndays-1)=tas2d
ENDELSE

IF MODEL_ID EQ 'ENS_AVE' THEN BEGIN
ofile='DATA/ENS_AVE_MOD_'+VAR+'_CORR_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START_2+'_'+APPLICATION_PERIOD_STOP_2+'_'+month(im)+'.dat'  
;tas2d_ave,nxm,nym,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
tasc2d_m_2(*,*,310*imm:310*imm+mndays-1)=tasc2d_ave
ENDIF ELSE BEGIN
ofile='DATA/MOD_BC_'+VAR+'_CORR_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START_2+'_'+APPLICATION_PERIOD_STOP_2+'_'+month(im)+'.dat'
;tasc2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
nxm=mnx
nym=mny
tasc2d_m_2(*,*,310*imm:310*imm+mndays-1)=tasc2d
ENDELSE

imm=imm+1

IF imm GT 2 THEN imm=0
ENDFOR  ;END im

;SEASONAL AVERAGES
print,'CALCULATING STANDARD DEV FOR SEASON ',ism,' MONTH ',im,jm
;TAS_1
FOR ix=0,nxm-1 DO BEGIN
FOR iy=0,nym-1 DO BEGIN
IF land2d_m(ix,iy) EQ 1 THEN BEGIN
ave_tas2d_sm_1(ix,iy,ism)=MEAN(tas2d_m_1(ix,iy,*) , /NAN ) 
sd_tas2d_sm_1(ix,iy,ism)=STDDEV(tas2d_m_1(ix,iy,*) , /NAN ) 
pdf_tas2d_sm_1(ix,iy,ism,*)=HISTOGRAM(REFORM(tas2d_m_1(ix,iy,*)),min=mint,max=maxt,binsize=binsz,/NaN)
ENDIF
ENDFOR
ENDFOR

;TAS_2
FOR ix=0,nxm-1 DO BEGIN
FOR iy=0,nym-1 DO BEGIN
IF land2d_m(ix,iy) EQ 1 THEN BEGIN
ave_tas2d_sm_2(ix,iy,ism)=MEAN(tas2d_m_2(ix,iy,*) , /NAN ) 
sd_tas2d_sm_2(ix,iy,ism)=STDDEV(tas2d_m_2(ix,iy,*) , /NAN ) 
pdf_tas2d_sm_2(ix,iy,ism,*)=HISTOGRAM(REFORM(tas2d_m_2(ix,iy,*)),min=mint,max=maxt,binsize=binsz,/NaN)
ENDIF
ENDFOR
ENDFOR

;TASC_1
FOR ix=0,nxm-1 DO BEGIN
FOR iy=0,nym-1 DO BEGIN
IF land2d_m(ix,iy) EQ 1 THEN BEGIN
ave_tasc2d_sm_1(ix,iy,ism)=MEAN(tasc2d_m_1(ix,iy,*) , /NAN ) 
sd_tasc2d_sm_1(ix,iy,ism)=STDDEV(tasc2d_m_1(ix,iy,*) , /NAN ) 
pdf_tasc2d_sm_1(ix,iy,ism,*)=HISTOGRAM(REFORM(tasc2d_m_1(ix,iy,*)),min=mint,max=maxt,binsize=binsz,/NaN)
ENDIF
ENDFOR
ENDFOR

;TASC_2
FOR ix=0,nxm-1 DO BEGIN
FOR iy=0,nym-1 DO BEGIN
IF land2d_m(ix,iy) EQ 1 THEN BEGIN
ave_tasc2d_sm_2(ix,iy,ism)=MEAN(tasc2d_m_2(ix,iy,*) , /NAN ) 
sd_tasc2d_sm_2(ix,iy,ism)=STDDEV(tasc2d_m_2(ix,iy,*) , /NAN ) 
pdf_tasc2d_sm_2(ix,iy,ism,*)=HISTOGRAM(REFORM(tasc2d_m_2(ix,iy,*)),min=mint,max=maxt,binsize=binsz,/NaN)
ENDIF
ENDFOR
ENDFOR

IF ism EQ 2 THEN BEGIN &$ ;ONLY FOR SUMMER
FOR ix=0,nxm-1 DO BEGIN &$
FOR iy=0,nym-1 DO BEGIN &$
IF land2d_m(ix,iy) EQ 1 THEN BEGIN &$
cc=WHERE(tas2d_m_1(ix,iy,*) GE 273.15+25  ,count) &$
s25_tas2d_sm_1(ix,iy)=count &$
cc=WHERE(tasc2d_m_1(ix,iy,*) GE 273.15+25 ,count) &$
s25_tasc2d_sm_1(ix,iy)=count &$
cc=WHERE(tas2d_m_2(ix,iy,*) GE 273.15+25  ,count) &$
s25_tas2d_sm_2(ix,iy)=count &$
cc=WHERE(tasc2d_m_2(ix,iy,*) GE 273.15+25 ,count) &$
s25_tasc2d_sm_2(ix,iy)=count &$
ENDIF &$
ENDFOR &$
ENDFOR &$
ENDIF


ism=ism+1

ENDFOR ; END LOOP MONTHS


;=================
;PLOTS

p_ave_tas2d_sm_1=fltarr(nxm,nym,4)
p_ave_tasc2d_sm_1=fltarr(nxm,nym,4)
p_ave_tas2d_sm_1(*,*,*)=-999.
p_ave_tasc2d_sm_1(*,*,*)=-999.

p_sd_tas2d_sm_1=fltarr(nxm,nym,4)
p_sd_tasc2d_sm_1=fltarr(nxm,nym,4)
p_sd_tas2d_sm_1(*,*,*)=-999.
p_sd_tasc2d_sm_1(*,*,*)=-999.

p_ave_tas2d_sm_2=fltarr(nxm,nym,4)
p_ave_tasc2d_sm_2=fltarr(nxm,nym,4)
p_ave_tas2d_sm_2(*,*,*)=-999.
p_ave_tasc2d_sm_2(*,*,*)=-999.

p_sd_tas2d_sm_2=fltarr(nxm,nym,4)
p_sd_tasc2d_sm_2=fltarr(nxm,nym,4)
p_sd_tas2d_sm_2(*,*,*)=-999.
p_sd_tasc2d_sm_2(*,*,*)=-999.

FOR id=0,3 do begin &$
FOR ix=0,nxm-1 do begin &$
FOR iy=0,nym-1 do begin &$
IF ave_tas2d_sm_1(ix,iy,id) GE 0 THEN p_ave_tas2d_sm_1(ix,iy,id)=ave_tas2d_sm_1(ix,iy,id) &$
IF ave_tasc2d_sm_1(ix,iy,id) GE 0 THEN p_ave_tasc2d_sm_1(ix,iy,id)=ave_tasc2d_sm_1(ix,iy,id) &$
IF sd_tas2d_sm_1(ix,iy,id) GE 0 THEN p_sd_tas2d_sm_1(ix,iy,id)=sd_tas2d_sm_1(ix,iy,id) &$
IF sd_tasc2d_sm_1(ix,iy,id) GE 0 THEN p_sd_tasc2d_sm_1(ix,iy,id)=sd_tasc2d_sm_1(ix,iy,id) &$
IF ave_tas2d_sm_2(ix,iy,id) GE 0 THEN p_ave_tas2d_sm_2(ix,iy,id)=ave_tas2d_sm_2(ix,iy,id) &$
IF ave_tasc2d_sm_2(ix,iy,id) GE 0 THEN p_ave_tasc2d_sm_2(ix,iy,id)=ave_tasc2d_sm_2(ix,iy,id) &$
IF sd_tas2d_sm_2(ix,iy,id) GE 0 THEN p_sd_tas2d_sm_2(ix,iy,id)=sd_tas2d_sm_2(ix,iy,id) &$
IF sd_tasc2d_sm_2(ix,iy,id) GE 0 THEN p_sd_tasc2d_sm_2(ix,iy,id)=sd_tasc2d_sm_2(ix,iy,id) &$
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

window,0,retain=2,xsize=1250,ysize=750
!p.multi=[0,4,3]
;TAS 1'
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CTL DJF',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,p_ave_tas2d_sm_1(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CTL MAM',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,p_ave_tas2d_sm_1(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CTL JJA',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,p_ave_tas2d_sm_1(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CTL SON',charsize=1.5;,position=[0.5,0.05,0.9,0.45]
contour,p_ave_tas2d_sm_1(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1,color=25,position=[0.96,0.55,0.99,0.95],/vertical

;TAS 2'
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' SCN DJF',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,p_ave_tas2d_sm_2(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' SCN MAM',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,p_ave_tas2d_sm_2(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' SCN JJA',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,p_ave_tas2d_sm_2(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' SCN SON',charsize=1.5;,position=[0.5,0.05,0.9,0.45]
contour,p_ave_tas2d_sm_2(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1,color=25,position=[0.96,0.55,0.99,0.95],/vertical

;BIAS
MinCol_b=20
MaxCol_b=240
minvalue_b=-4.25
Maxvalue_b=4.25
ContourDist_b=0.5
minvalue_b=-5.5
Maxvalue_b=5.5
ContourDist_b=1.
rInterval_b=Maxvalue_b-Minvalue_b
iNumOfConts_b=FIX(rInterval_b/ContourDist_b)+1
rgrContValues_b = INDGEN(iNumOfConts_b)*ContourDist_b + MinValue_b
rgrColValues_b = INDGEN(iNumOfConts_b-1) * (MaxCol_b - MinCol_b) / (iNumOfConts_b-2) + MinCol_b

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' SCN-CTL DJF',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,p_ave_tas2d_sm_2(*,*,0)-p_ave_tas2d_sm_1(*,*,0),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' SCN-CTL MAM',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,p_ave_tas2d_sm_2(*,*,1)-p_ave_tas2d_sm_1(*,*,1),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' SCN-CTL JJA',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,p_ave_tas2d_sm_2(*,*,2)-p_ave_tas2d_sm_1(*,*,2),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' SCN-CTL SON',charsize=1.5;,position=[0.5,0.05,0.9,0.45]
contour,p_ave_tas2d_sm_2(*,*,3)-p_ave_tas2d_sm_1(*,*,3),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue_b,max=maxvalue_b,c_colors=rgrColValues_b,divisions=iNumOfConts_b-1,charsize=1,color=25,position=[0.96,0.55,0.99,0.95],/vertical


;TAS CORR
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

window,1,retain=2,xsize=1250,ysize=750
!p.multi=[0,4,3]
;TASC 1'
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CTL CORR DJF',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,p_ave_tasc2d_sm_1(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CTL CORR MAM',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,p_ave_tasc2d_sm_1(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CTL CORR JJA',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,p_ave_tasc2d_sm_1(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CTL CORR SON',charsize=1.5;,position=[0.5,0.05,0.9,0.45]
contour,p_ave_tasc2d_sm_1(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1,color=25,position=[0.96,0.55,0.99,0.95],/vertical

;TASC 2'
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' SCN CORR DJF',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,p_ave_tasc2d_sm_2(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' SCN CORR MAM',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,p_ave_tasc2d_sm_2(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' SCN CORR JJA',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,p_ave_tasc2d_sm_2(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' SCN CORR SON',charsize=1.5;,position=[0.5,0.05,0.9,0.45]
contour,p_ave_tasc2d_sm_2(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1,color=25,position=[0.96,0.55,0.99,0.95],/vertical

;BIAS'
MinCol_b=20
MaxCol_b=240
minvalue_b=-4.25
Maxvalue_b=4.25
ContourDist_b=0.5
minvalue_b=-5.5
Maxvalue_b=5.5
ContourDist_b=1.
rInterval_b=Maxvalue_b-Minvalue_b
iNumOfConts_b=FIX(rInterval_b/ContourDist_b)+1
rgrContValues_b = INDGEN(iNumOfConts_b)*ContourDist_b + MinValue_b
rgrColValues_b = INDGEN(iNumOfConts_b-1) * (MaxCol_b - MinCol_b) / (iNumOfConts_b-2) + MinCol_b

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' SCN-CTL CORR DJF',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,p_ave_tasc2d_sm_2(*,*,0)-p_ave_tasc2d_sm_1(*,*,0),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' SCN-CTL CORR MAM',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,p_ave_tasc2d_sm_2(*,*,1)-p_ave_tasc2d_sm_1(*,*,1),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' SCN-CTL CORR JJA',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,p_ave_tasc2d_sm_2(*,*,2)-p_ave_tasc2d_sm_1(*,*,2),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' SCN-CTL CORR SON',charsize=1.5;,position=[0.5,0.05,0.9,0.45]
contour,p_ave_tasc2d_sm_2(*,*,3)-p_ave_tasc2d_sm_1(*,*,3),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue_b,max=maxvalue_b,c_colors=rgrColValues_b,divisions=iNumOfConts_b-1,charsize=1,color=25,position=[0.96,0.55,0.99,0.95],/vertical


;ST DEV_1
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

window,2,retain=2,xsize=1250,ysize=750
!p.multi=[0,4,3]
;STD TAS 1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='STD '+VAR+' CTL DJF',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,p_sd_tas2d_sm_1(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='STD '+VAR+' CTL MAM',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,p_sd_tas2d_sm_1(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='STD '+VAR+' CTL JJA',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,p_sd_tas2d_sm_1(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='STD '+VAR+' CTL SON',charsize=1.5;,position=[0.5,0.05,0.9,0.45]
contour,p_sd_tas2d_sm_1(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1,color=25,position=[0.96,0.55,0.99,0.95],/vertical

;STD TAS 2
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='STD '+VAR+' SCN DJF',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,p_sd_tas2d_sm_2(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='STD '+VAR+' SCN MAM',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,p_sd_tas2d_sm_2(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='STD '+VAR+' SCN JJA',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,p_sd_tas2d_sm_2(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='STD '+VAR+' SCN SON',charsize=1.5;,position=[0.5,0.05,0.9,0.45]
contour,p_sd_tas2d_sm_2(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1,color=25,position=[0.96,0.55,0.99,0.95],/vertical

;BIAS
MinCol_b=20
MaxCol_b=240
minvalue_b=-4.25
Maxvalue_b=4.25
ContourDist_b=0.5
minvalue_b=-5.5
Maxvalue_b=5.5
ContourDist_b=1.
rInterval_b=Maxvalue_b-Minvalue_b
iNumOfConts_b=FIX(rInterval_b/ContourDist_b)+1
rgrContValues_b = INDGEN(iNumOfConts_b)*ContourDist_b + MinValue_b
rgrColValues_b = INDGEN(iNumOfConts_b-1) * (MaxCol_b - MinCol_b) / (iNumOfConts_b-2) + MinCol_b

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='STD '+VAR+' SCN DJF',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,p_sd_tas2d_sm_2(*,*,0)-p_sd_tas2d_sm_1(*,*,0),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='STD '+VAR+' SCN MAM',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,p_sd_tas2d_sm_2(*,*,1)-p_sd_tas2d_sm_1(*,*,1),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='STD '+VAR+' SCN JJA',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,p_sd_tas2d_sm_2(*,*,2)-p_sd_tas2d_sm_1(*,*,2),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='STD '+VAR+' SCN SON',charsize=1.5;,position=[0.5,0.05,0.9,0.45]
contour,p_sd_tas2d_sm_2(*,*,3)-p_sd_tas2d_sm_1(*,*,3),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue_b,max=maxvalue_b,c_colors=rgrColValues_b,divisions=iNumOfConts_b-1,charsize=1,color=25,position=[0.96,0.55,0.99,0.95],/vertical

window,3,retain=2,xsize=1250,ysize=750
!p.multi=[0,4,3]
;STD TAS CORR 1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='STD '+VAR+' CTL DJF',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,p_sd_tasc2d_sm_1(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='STD '+VAR+' CTL MAM',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,p_sd_tasc2d_sm_1(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='STD '+VAR+' CTL JJA',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,p_sd_tasc2d_sm_1(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='STD '+VAR+' CTL SON',charsize=1.5;,position=[0.5,0.05,0.9,0.45]
contour,p_sd_tasc2d_sm_1(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1,color=25,position=[0.96,0.55,0.99,0.95],/vertical

;STD TAS 2
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='STD '+VAR+' SCN DJF',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,p_sd_tasc2d_sm_2(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='STD '+VAR+' SCN MAM',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,p_sd_tasc2d_sm_2(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='STD '+VAR+' SCN JJA',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,p_sd_tasc2d_sm_2(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='STD '+VAR+' SCN SON',charsize=1.5;,position=[0.5,0.05,0.9,0.45]
contour,p_sd_tasc2d_sm_2(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1,color=25,position=[0.96,0.55,0.99,0.95],/vertical

;BIAS
MinCol_b=20
MaxCol_b=240
minvalue_b=-4.25
Maxvalue_b=4.25
ContourDist_b=0.5
minvalue_b=-5.5
Maxvalue_b=5.5
ContourDist_b=1.
rInterval_b=Maxvalue_b-Minvalue_b
iNumOfConts_b=FIX(rInterval_b/ContourDist_b)+1
rgrContValues_b = INDGEN(iNumOfConts_b)*ContourDist_b + MinValue_b
rgrColValues_b = INDGEN(iNumOfConts_b-1) * (MaxCol_b - MinCol_b) / (iNumOfConts_b-2) + MinCol_b

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='STD '+VAR+' SCN-CTL CORR DJF',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,p_sd_tasc2d_sm_2(*,*,0)-p_sd_tasc2d_sm_1(*,*,0),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='STD '+VAR+' SCN-CTL CORR MAM',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,p_sd_tasc2d_sm_2(*,*,1)-p_sd_tasc2d_sm_1(*,*,1),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='STD '+VAR+' SCN-CTL CORR JJA',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,p_sd_tasc2d_sm_2(*,*,2)-p_sd_tasc2d_sm_1(*,*,2),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='STD '+VAR+' SCN-CTL CORR SON',charsize=1.5;,position=[0.5,0.05,0.9,0.45]
contour,p_sd_tasc2d_sm_2(*,*,3)-p_sd_tasc2d_sm_1(*,*,3),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue_b,max=maxvalue_b,c_colors=rgrColValues_b,divisions=iNumOfConts_b-1,charsize=1,color=25,position=[0.96,0.55,0.99,0.95],/vertical

;SUMMER DAYS
loadct,39
!p.background=255
!p.color=0
levels=findgen(10)*3.
colors=findgen(10)*24+20

levels_p=[0,2,4,6,8,10]
colors_p=[50.0000 ,    92.0000   ,  134.000   ,  186.000   ,  218.000]

window,6,retain=2
!p.multi=[0,3,2]
;TAS 
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS CTL',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,s25_tas2d_sm_1(*,*)/NYEARS/3.,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS SCN',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,s25_tas2d_sm_2(*,*)/NYEARS/3.,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=min(levels),max=max(levels),divisions=N_ELEMENTS(levels-1),c_colors=colors,charsize=1,color=25,position=[0.0,0.55,0.02,0.95],/vertical

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS SCN-CTL',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,(s25_tas2d_sm_2(*,*)-s25_tas2d_sm_1(*,*))/NYEARS/3.,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=min(levels_p),max=max(levels_p),divisions=N_ELEMENTS(levels_p-1),c_colors=colors_p,charsize=1,color=25,position=[0.97,0.55,0.99,0.95],/vertical

;TASC 
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS CTL CORR',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,s25_tasc2d_sm_1(*,*)/NYEARS/3.,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS SCN CORR',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,s25_tasc2d_sm_2(*,*)/NYEARS/3.,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS SCN-CTL CORR',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,(s25_tasc2d_sm_2(*,*)-s25_tasc2d_sm_1(*,*))/NYEARS/3.,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

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

pdf_tas2d_sm_m_1=FLTARR(nm,4,nbins)
pdf_tasc2d_sm_m_1=FLTARR(nm,4,nbins)
pdf_tas2d_sm_m_1(*,*,*)=!VALUES.F_NAN
pdf_tasc2d_sm_m_1(*,*,*)=!VALUES.F_NAN

pdf_tas2d_sm_m_2=FLTARR(nm,4,nbins)
pdf_tasc2d_sm_m_2=FLTARR(nm,4,nbins)
pdf_tas2d_sm_m_2(*,*,*)=!VALUES.F_NAN
pdf_tasc2d_sm_m_2(*,*,*)=!VALUES.F_NAN

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
ww=REFORM(pdf_tas2d_sm_1(ma(0):ma(1),ma(2):ma(3),ism,*))
wwc=REFORM(pdf_tasc2d_sm_1(ma(0):ma(1),ma(2):ma(3),ism,*))
FOR ib=0,nbins-1 DO BEGIN 
pdf_tas2d_sm_m_1(imm,ism,ib)=MEAN(ww(*,*,ib),/NaN) 
pdf_tasc2d_sm_m_1(imm,ism,ib)=MEAN(wwc(*,*,ib),/NaN) 
ENDFOR 
ww=REFORM(pdf_tas2d_sm_2(ma(0):ma(1),ma(2):ma(3),ism,*))
wwc=REFORM(pdf_tasc2d_sm_2(ma(0):ma(1),ma(2):ma(3),ism,*))
FOR ib=0,nbins-1 DO BEGIN 
pdf_tas2d_sm_m_2(imm,ism,ib)=MEAN(ww(*,*,ib),/NaN) 
pdf_tasc2d_sm_m_2(imm,ism,ib)=MEAN(wwc(*,*,ib),/NaN) 
ENDFOR 
ENDFOR 

ENDFOR 

loadct,39
window,10,retain=2,xsize=1050,ysize=950
!p.multi=[0,4,8]
!p.color=0
!p.background=255
FOR imm=0,nm-1 DO BEGIN ; LOOP OVER MASKS

pdf_data=reform(pdf_tas2d_sm_m_1(imm,0,*))
pdf_datac=reform(pdf_tasc2d_sm_m_1(imm,0,*))
pdf_data_1=reform(pdf_tas2d_sm_m_2(imm,0,*))
pdf_datac_1=reform(pdf_tasc2d_sm_m_2(imm,0,*))
PLOT, bins-273.15, pdf_data/(NDAYS_S(0)*NYEARS),xrange=[-10,20],yrange=[0,0.16],xstyle=1,ystyle=1,title=mask(imm),thick=2
OPLOT, bins-273.15, pdf_datac/(NDAYS_S(0)*NYEARS),color=200,thick=2
OPLOT, bins-273.15, pdf_data_1/(NDAYS_S(0)*NYEARS),color=0,thick=2
OPLOT, bins-273.15, pdf_datac_1/(NDAYS_S(0)*NYEARS),color=200,thick=2

pdf_data=reform(pdf_tas2d_sm_m_1(imm,1,*))
pdf_datac=reform(pdf_tasc2d_sm_m_1(imm,1,*))
pdf_data_1=reform(pdf_tas2d_sm_m_2(imm,1,*))
pdf_datac_1=reform(pdf_tasc2d_sm_m_2(imm,1,*))
PLOT, bins-273.15, pdf_data/(NDAYS_S(1)*NYEARS),xrange=[-5,25],yrange=[0,0.16],xstyle=1,ystyle=1,title=mask(imm),thick=2
OPLOT, bins-273.15, pdf_datac/(NDAYS_S(1)*NYEARS),color=200,thick=2
OPLOT, bins-273.15, pdf_data_1/(NDAYS_S(1)*NYEARS),color=0,thick=2
OPLOT, bins-273.15, pdf_datac_1/(NDAYS_S(1)*NYEARS),color=200,thick=2

pdf_data=reform(pdf_tas2d_sm_m_1(imm,2,*))
pdf_datac=reform(pdf_tasc2d_sm_m_1(imm,2,*))
pdf_data_1=reform(pdf_tas2d_sm_m_2(imm,2,*))
pdf_datac_1=reform(pdf_tasc2d_sm_m_2(imm,2,*))
PLOT, bins-273.15, pdf_data/(NDAYS_S(2)*NYEARS),xrange=[0,30],yrange=[0,0.16],xstyle=1,ystyle=1,title=mask(imm),thick=2
OPLOT, bins-273.15, pdf_datac/(NDAYS_S(2)*NYEARS),color=200,thick=2
OPLOT, bins-273.15, pdf_data_1/(NDAYS_S(2)*NYEARS),color=0,thick=2
OPLOT, bins-273.15, pdf_datac_1/(NDAYS_S(2)*NYEARS),color=200,thick=2

pdf_data=reform(pdf_tas2d_sm_m_1(imm,3,*))
pdf_datac=reform(pdf_tasc2d_sm_m_1(imm,3,*))
pdf_data_1=reform(pdf_tas2d_sm_m_2(imm,3,*))
pdf_datac_1=reform(pdf_tasc2d_sm_m_2(imm,3,*))
PLOT, bins-273.15, pdf_data/(NDAYS_S(3)*NYEARS),xrange=[-5,25],yrange=[0,0.16],xstyle=1,ystyle=1,title=mask(imm),thick=2
OPLOT, bins-273.15, pdf_datac/(NDAYS_S(3)*NYEARS),color=200,thick=2
OPLOT, bins-273.15, pdf_data_1/(NDAYS_S(3)*NYEARS),color=0,thick=2
OPLOT, bins-273.15, pdf_datac_1/(NDAYS_S(3)*NYEARS),color=200,thick=2

ENDFOR


;=============================
;DIURNAL RANGEE
;RESTORING DATA
;DATA FILES
Mx_tas2d_m_1 = FLTARR(nxm,nym,3*31.*NYEARS) ;TMAX
Mx_tas2d_m_1(*,*,*) = !VALUES.F_NAN
Mx_tasc2d_m_1 = FLTARR(nxm,nym,3*31.*NYEARS)
Mx_tasc2d_m_1(*,*,*) = !VALUES.F_NAN
Mx_tas2d_m_2 = FLTARR(nxm,nym,3*31.*NYEARS) ;TMAX
Mx_tas2d_m_2(*,*,*) = !VALUES.F_NAN
Mx_tasc2d_m_2 = FLTARR(nxm,nym,3*31.*NYEARS)
Mx_tasc2d_m_2(*,*,*) = !VALUES.F_NAN

mn_tas2d_m_1 = FLTARR(nxm,nym,3*31.*NYEARS) ;Tmin
mn_tas2d_m_1(*,*,*) = !VALUES.F_NAN
mn_tasc2d_m_1 = FLTARR(nxm,nym,3*31.*NYEARS)
mn_tasc2d_m_1(*,*,*) = !VALUES.F_NAN
mn_tas2d_m_2 = FLTARR(nxm,nym,3*31.*NYEARS) ;Tmin
mn_tas2d_m_2(*,*,*) = !VALUES.F_NAN
mn_tasc2d_m_2 = FLTARR(nxm,nym,3*31.*NYEARS)
mn_tasc2d_m_2(*,*,*) = !VALUES.F_NAN

dr_tas2d_sm_1=fltarr(nxm,nym,4)  ;DIURNAL RANGE
dr_tasc2d_sm_1=fltarr(nxm,nym,4)
dr_tas2d_sm_1(*,*,*)=-999.
dr_tasc2d_sm_1(*,*,*)=-999.
dr_tas2d_sm_2=fltarr(nxm,nym,4)  ;DIURNAL RANGE
dr_tasc2d_sm_2=fltarr(nxm,nym,4)
dr_tas2d_sm_2(*,*,*)=-999.
dr_tasc2d_sm_2(*,*,*)=-999.

ss25_tas2d_sm_1=fltarr(nxm,nym)  ;SUMMER DAYS (=n of days when TMax > 25 C)
ss25_tasc2d_sm_1=fltarr(nxm,nym)
ss25_tas2d_sm_1(*,*)=-999.
ss25_tasc2d_sm_1(*,*)=-999.
ss25_tas2d_sm_2=fltarr(nxm,nym)  ;SUMMER DAYS (=n of days when TMax > 25 C)
ss25_tasc2d_sm_2=fltarr(nxm,nym)
ss25_tas2d_sm_2(*,*)=-999.
ss25_tasc2d_sm_2(*,*)=-999.

ss35_tas2d_sm_1=fltarr(nxm,nym)  ;SUMMER DAYS (=n of days when TMax > 35 C)
ss35_tasc2d_sm_1=fltarr(nxm,nym)
ss35_tas2d_sm_1(*,*)=-999.
ss35_tasc2d_sm_1(*,*)=-999.
ss35_tas2d_sm_2=fltarr(nxm,nym)  ;SUMMER DAYS (=n of days when TMax > 35 C)
ss35_tasc2d_sm_2=fltarr(nxm,nym)
ss35_tas2d_sm_2(*,*)=-999.
ss35_tasc2d_sm_2(*,*)=-999.

ssm20_tas2d_sm_1=fltarr(nxm,nym)  ;SUMMER DAYS (=n of days when Tmin > 20 C)
ssm20_tasc2d_sm_1=fltarr(nxm,nym)
ssm20_tas2d_sm_1(*,*)=-999.
ssm20_tasc2d_sm_1(*,*)=-999.
ssm20_tas2d_sm_2=fltarr(nxm,nym)  ;SUMMER DAYS (=n of days when Tmin > 20 C)
ssm20_tasc2d_sm_2=fltarr(nxm,nym)
ssm20_tas2d_sm_2(*,*)=-999.
ssm20_tasc2d_sm_2(*,*)=-999.

ssm25_tas2d_sm_1=fltarr(nxm,nym)  ;SUMMER DAYS (=n of days when Tmin > 25 C)
ssm25_tasc2d_sm_1=fltarr(nxm,nym)
ssm25_tas2d_sm_1(*,*)=-999.
ssm25_tasc2d_sm_1(*,*)=-999.
ssm25_tas2d_sm_2=fltarr(nxm,nym)  ;SUMMER DAYS (=n of days when Tmin > 25 C)
ssm25_tasc2d_sm_2=fltarr(nxm,nym)
ssm25_tas2d_sm_2(*,*)=-999.
ssm25_tasc2d_sm_2(*,*)=-999.




;LOOP OVER MONTHS
ism=0
FOR jm=0,11,3 DO BEGIN ;STARTING MONTH IN SEASON
imm=0
FOR im=jm,jm+2 DO BEGIN ; MONTH IN YEAR

PRINT,'SEASON = ',ism
PRINT,'MONTH IN YEAR im = ',im
PRINT,'MONTH IN SEASON imm = ',imm


print,'PERIOD 1: ',APPLICATION_PERIOD_START_1,'-',+APPLICATION_PERIOD_STOP_1
IF MODEL_ID EQ 'ENS_AVE' THEN BEGIN 
ofile='DATA/ENS_AVE_MOD_Tmax_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START_1+'_'+APPLICATION_PERIOD_STOP_1+'_'+month(im)+'.dat'  
;tas2d_ave,nxm,nym,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
Mx_tas2d_m_1(*,*,310*imm:310*imm+mndays-1)=tas2d_ave
ofile='DATA/ENS_AVE_MOD_Tmin_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START_1+'_'+APPLICATION_PERIOD_STOP_1+'_'+month(im)+'.dat'  
;tas2d_ave,nxm,nym,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
mn_tas2d_m_1(*,*,310*imm:310*imm+mndays-1)=tas2d_ave
ENDIF ELSE BEGIN 
ofile='DATA/MOD_Tmax_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START_1+'_'+APPLICATION_PERIOD_STOP_1+'_'+month(im)+'.dat'
;tas2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
Mx_tas2d_m_1(*,*,310*imm:310*imm+mndays-1)=tas2d
ofile='DATA/MOD_Tmin_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START_1+'_'+APPLICATION_PERIOD_STOP_1+'_'+month(im)+'.dat'
;tas2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
mn_tas2d_m_1(*,*,310*imm:310*imm+mndays-1)=tas2d
ENDELSE

IF MODEL_ID EQ 'ENS_AVE' THEN BEGIN
ofile='DATA/ENS_AVE_MOD_Tmax_CORR_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START_1+'_'+APPLICATION_PERIOD_STOP_1+'_'+month(im)+'.dat'  
;tas2d_ave,nxm,nym,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
Mx_tasc2d_m_1(*,*,310*imm:310*imm+mndays-1)=tasc2d_ave
ofile='DATA/ENS_AVE_MOD_Tmin_CORR_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START_1+'_'+APPLICATION_PERIOD_STOP_1+'_'+month(im)+'.dat'  
;tas2d_ave,nxm,nym,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
mn_tasc2d_m_1(*,*,310*imm:310*imm+mndays-1)=tasc2d_ave
ENDIF ELSE BEGIN
ofile='DATA/MOD_BC_Tmax_CORR_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START_1+'_'+APPLICATION_PERIOD_STOP_1+'_'+month(im)+'.dat'
;tasc2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
Mx_tasc2d_m_1(*,*,310*imm:310*imm+mndays-1)=tasc2d
ofile='DATA/MOD_BC_Tmin_CORR_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START_1+'_'+APPLICATION_PERIOD_STOP_1+'_'+month(im)+'.dat'
;tasc2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
mn_tasc2d_m_1(*,*,310*imm:310*imm+mndays-1)=tasc2d
ENDELSE

print,'PERIOD 2: ',APPLICATION_PERIOD_START_2,'-',+APPLICATION_PERIOD_STOP_2
IF MODEL_ID EQ 'ENS_AVE' THEN BEGIN 
ofile='DATA/ENS_AVE_MOD_Tmax_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START_2+'_'+APPLICATION_PERIOD_STOP_2+'_'+month(im)+'.dat'  
;tas2d_ave,nxm,nym,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
_Mxtas2d_m_2(*,*,310*imm:310*imm+mndays-1)=tas2d_ave
ofile='DATA/ENS_AVE_MOD_Tmin_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START_2+'_'+APPLICATION_PERIOD_STOP_2+'_'+month(im)+'.dat'  
;tas2d_ave,nxm,nym,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
mn_tas2d_m_2(*,*,310*imm:310*imm+mndays-1)=tas2d_ave
ENDIF ELSE BEGIN 
ofile='DATA/MOD_Tmax_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START_2+'_'+APPLICATION_PERIOD_STOP_2+'_'+month(im)+'.dat'
;tas2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
Mx_tas2d_m_2(*,*,310*imm:310*imm+mndays-1)=tas2d
ofile='DATA/MOD_Tmin_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START_2+'_'+APPLICATION_PERIOD_STOP_2+'_'+month(im)+'.dat'
;tas2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
mn_tas2d_m_2(*,*,310*imm:310*imm+mndays-1)=tas2d
ENDELSE

IF MODEL_ID EQ 'ENS_AVE' THEN BEGIN
ofile='DATA/ENS_AVE_MOD_Tmax_CORR_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START_2+'_'+APPLICATION_PERIOD_STOP_2+'_'+month(im)+'.dat'  
;tas2d_ave,nxm,nym,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
Mx_tasc2d_m_2(*,*,310*imm:310*imm+mndays-1)=tasc2d_ave
ofile='DATA/ENS_AVE_MOD_Tmin_CORR_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START_2+'_'+APPLICATION_PERIOD_STOP_2+'_'+month(im)+'.dat'  
;tas2d_ave,nxm,nym,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
mn_tasc2d_m_2(*,*,310*imm:310*imm+mndays-1)=tasc2d_ave
ENDIF ELSE BEGIN
ofile='DATA/MOD_BC_Tmax_CORR_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START_2+'_'+APPLICATION_PERIOD_STOP_2+'_'+month(im)+'.dat'
;tasc2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
Mx_tasc2d_m_2(*,*,310*imm:310*imm+mndays-1)=tasc2d
ofile='DATA/MOD_BC_Tmin_CORR_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START_2+'_'+APPLICATION_PERIOD_STOP_2+'_'+month(im)+'.dat'
;tasc2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
nxm=mnx
nym=mny
mn_tasc2d_m_2(*,*,310*imm:310*imm+mndays-1)=tasc2d
ENDELSE


imm=imm+1

IF imm GT 2 THEN imm=0
ENDFOR  ;END im

FOR ix=0,nxm-1 DO BEGIN &$
FOR iy=0,nym-1 DO BEGIN &$
IF land2d_m(ix,iy) EQ 1 THEN BEGIN &$
dr_tas2d_sm_1(ix,iy,ism)=MEAN(Mx_tas2d_m_1(ix,iy,*)-mn_tas2d_m_1(ix,iy,*),/NaN)
dr_tasc2d_sm_1(ix,iy,ism)=MEAN(Mx_tasc2d_m_1(ix,iy,*)-mn_tasc2d_m_1(ix,iy,*),/NaN)
dr_tas2d_sm_2(ix,iy,ism)=MEAN(Mx_tas2d_m_2(ix,iy,*)-mn_tas2d_m_2(ix,iy,*),/NaN)
dr_tasc2d_sm_2(ix,iy,ism)=MEAN(Mx_tasc2d_m_2(ix,iy,*)-mn_tasc2d_m_2(ix,iy,*),/NaN)
ENDIF
ENDFOR
ENDFOR

IF ism EQ 2 THEN BEGIN &$ ;ONLY FOR SUMMER
FOR ix=0,nxm-1 DO BEGIN &$
FOR iy=0,nym-1 DO BEGIN &$
IF land2d_m(ix,iy) EQ 1 THEN BEGIN &$
cc=WHERE(Mx_tas2d_m_1(ix,iy,*) GE 273.15+25       ,count) &$
ss25_tas2d_sm_1(ix,iy)=count
cc=WHERE(Mx_tas2d_m_1(ix,iy,*) GE 273.15+35       ,count) &$
ss35_tas2d_sm_1(ix,iy)=count
cc=WHERE(mn_tas2d_m_1(ix,iy,*) GE 273.15+20       ,count) &$
ssm20_tas2d_sm_1(ix,iy)=count
cc=WHERE(mn_tas2d_m_1(ix,iy,*) GE 273.15+25       ,count) &$
ssm25_tas2d_sm_1(ix,iy)=count
cc=WHERE(Mx_tasc2d_m_1(ix,iy,*) GE 273.15+25      ,count) &$
ss25_tasc2d_sm_1(ix,iy)=count
cc=WHERE(Mx_tasc2d_m_1(ix,iy,*) GE 273.15+35      ,count) &$
ss35_tasc2d_sm_1(ix,iy)=count
cc=WHERE(mn_tasc2d_m_1(ix,iy,*) GE 273.15+20      ,count) &$
ssm20_tasc2d_sm_1(ix,iy)=count
cc=WHERE(mn_tasc2d_m_1(ix,iy,*) GE 273.15+25      ,count) &$
ssm25_tasc2d_sm_1(ix,iy)=count
cc=WHERE(Mx_tas2d_m_2(ix,iy,*) GE 273.15+25       ,count) &$
ss25_tas2d_sm_2(ix,iy)=count
cc=WHERE(Mx_tas2d_m_2(ix,iy,*) GE 273.15+35       ,count) &$
ss35_tas2d_sm_2(ix,iy)=count
cc=WHERE(mn_tas2d_m_2(ix,iy,*) GE 273.15+20       ,count) &$
ssm20_tas2d_sm_2(ix,iy)=count
cc=WHERE(mn_tas2d_m_2(ix,iy,*) GE 273.15+25       ,count) &$
ssm25_tas2d_sm_2(ix,iy)=count
cc=WHERE(Mx_tasc2d_m_2(ix,iy,*) GE 273.15+25      ,count) &$
ss25_tasc2d_sm_2(ix,iy)=count
cc=WHERE(Mx_tasc2d_m_2(ix,iy,*) GE 273.15+35      ,count) &$
ss35_tasc2d_sm_2(ix,iy)=count
cc=WHERE(mn_tasc2d_m_2(ix,iy,*) GE 273.15+20      ,count) &$
ssm20_tasc2d_sm_2(ix,iy)=count
cc=WHERE(mn_tasc2d_m_1(ix,iy,*) GE 273.15+25      ,count) &$
ssm25_tasc2d_sm_2(ix,iy)=count
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
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='DIURNAL RANGE '+VAR+' CTL DJF',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,dr_tas2d_sm_1(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=' MAM',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,dr_tas2d_sm_1(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=' JJA',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,dr_tas2d_sm_1(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=' SON',charsize=1.5;,position=[0.5,0.05,0.9,0.45]
contour,dr_tas2d_sm_1(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1,color=25,position=[0.96,0.55,0.99,0.95],/vertical

;TASC
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='DIURNAL RANGE '+VAR+' SCN DJF',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,dr_tas2d_sm_2(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=' MAM',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,dr_tas2d_sm_2(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=' JJA',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,dr_tas2d_sm_2(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=' SON',charsize=1.5;,position=[0.5,0.05,0.9,0.45]
contour,dr_tas2d_sm_2(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

;BIAS
MinCol_b=20
MaxCol_b=240
minvalue_b=-4.5
Maxvalue_b=4.5
ContourDist_b=1.
rInterval_b=Maxvalue_b-Minvalue_b
iNumOfConts_b=FIX(rInterval_b/ContourDist_b)+1
rgrContValues_b = INDGEN(iNumOfConts_b)*ContourDist_b + MinValue_b
rgrColValues_b = INDGEN(iNumOfConts_b-1) * (MaxCol_b - MinCol_b) / (iNumOfConts_b-2) + MinCol_b


contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='DIURNAL RANGE '+VAR+' SCN -CTL DJF',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,dr_tas2d_sm_2(*,*,0)-dr_tas2d_sm_1(*,*,0),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=' MAM',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,dr_tas2d_sm_2(*,*,1)-dr_tas2d_sm_1(*,*,1),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=' JJA',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,dr_tas2d_sm_2(*,*,2)-dr_tas2d_sm_1(*,*,2),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=' SON',charsize=1.5;,position=[0.5,0.05,0.9,0.45]
contour,dr_tas2d_sm_2(*,*,3)-dr_tas2d_sm_1(*,*,3),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

;DIURNAL RANGE CORRECTED
window,11,retain=2
!p.multi=[0,4,3]
;TASC'
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='DIURNAL RANGE '+VAR+' CTL CORRECTED DJF',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,dr_tasc2d_sm_1(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=' MAM',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,dr_tasc2d_sm_1(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=' JJA',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,dr_tasc2d_sm_1(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=' SON',charsize=1.5;,position=[0.5,0.05,0.9,0.45]
contour,dr_tasc2d_sm_1(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1,color=25,position=[0.96,0.55,0.99,0.95],/vertical

;TASC
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='DIURNAL RANGE '+VAR+' SCN CORRECTED DJF',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,dr_tasc2d_sm_2(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=' MAM',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,dr_tasc2d_sm_2(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=' JJA',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,dr_tasc2d_sm_2(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=' SON',charsize=1.5;,position=[0.5,0.05,0.9,0.45]
contour,dr_tasc2d_sm_2(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

;BIAS
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='DIURNAL RANGE '+VAR+' SCN-CTL CORRECTED DJF',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,dr_tasc2d_sm_2(*,*,0)-dr_tasc2d_sm_1(*,*,0),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=' MAM',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,dr_tasc2d_sm_2(*,*,1)-dr_tasc2d_sm_1(*,*,1),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=' JJA',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,dr_tasc2d_sm_2(*,*,2)-dr_tasc2d_sm_1(*,*,2),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=' SON',charsize=1.5;,position=[0.5,0.05,0.9,0.45]
contour,dr_tasc2d_sm_2(*,*,3)-dr_tasc2d_sm_1(*,*,3),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

;SUMMER DAYS
loadct,39
!p.background=255
!p.color=0
levels=findgen(10)*3.
colors=findgen(10)*24+20

levels_p=findgen(11)/10.
colors_p=(findgen(11)*22+30)
levels_p=[0,2,4,6,8,10]
colors_p=(findgen(5)*42+50)
colors_p=[50.0000 ,    92.0000   ,  134.000   ,  186.000   ,  218.000]

window,12,retain=2
!p.multi=[0,3,2]
;MAX T > 25
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS TMAX > 25 CTL',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,ss25_tas2d_sm_1(*,*)/NYEARS/3.,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS TMAX > 25 SCN',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,ss25_tas2d_sm_2(*,*)/NYEARS/3.,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=min(levels),max=max(levels),divisions=N_ELEMENTS(levels-1),c_colors=colors,charsize=1,color=25,position=[0.,0.55,0.02,0.95],/vertical

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS TMAX > 25 SCN-CTL',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,(ss25_tas2d_sm_2(*,*)-ss25_tas2d_sm_1(*,*))/NYEARS/3.,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=min(levels_p),max=max(levels_p),divisions=N_ELEMENTS(levels_p-1),c_colors=colors_p,charsize=1,color=25,position=[0.97,0.55,0.99,0.95],/vertical

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS TMAX > 25 CORRECTED CTL',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,ss25_tasc2d_sm_1(*,*)/NYEARS/3.,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS TMAX > 25 CORRECTED SCN',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,ss25_tasc2d_sm_2(*,*)/NYEARS/3.,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=min(levels),max=max(levels),divisions=N_ELEMENTS(levels-1),c_colors=colors,charsize=1,color=25,position=[0.,0.55,0.02,0.95],/vertical

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS TMAX > 25 CORRECTED SCN-CTL',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,(ss25_tasc2d_sm_2(*,*)-ss25_tasc2d_sm_1(*,*))/NYEARS/3.,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=min(levels_p),max=max(levels_p),divisions=N_ELEMENTS(levels_p-1),c_colors=colors_p,charsize=1,color=25,position=[0.97,0.55,0.99,0.95],/vertical

window,13,retain=2
!p.multi=[0,3,2]
;MAX T > 35
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS TMAX > 35 CTL',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,ss35_tas2d_sm_1(*,*)/NYEARS/3.,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS TMAX > 35 SCN',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,ss35_tas2d_sm_2(*,*)/NYEARS/3.,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=min(levels),max=max(levels),divisions=N_ELEMENTS(levels-1),c_colors=colors,charsize=1,color=25,position=[0.,0.55,0.02,0.95],/vertical

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS TMAX > 35 SCN-CTL',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,(ss35_tas2d_sm_2(*,*)-ss35_tas2d_sm_1(*,*))/NYEARS/3.,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=min(levels_p),max=max(levels_p),divisions=N_ELEMENTS(levels_p-1),c_colors=colors_p,charsize=1,color=25,position=[0.97,0.55,0.99,0.95],/vertical

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS TMAX > 35 CORRECTED CTL',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,ss35_tasc2d_sm_1(*,*)/NYEARS/3.,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS TMAX > 35 CORRECTED SCN',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,ss35_tasc2d_sm_2(*,*)/NYEARS/3.,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=min(levels),max=max(levels),divisions=N_ELEMENTS(levels-1),c_colors=colors,charsize=1,color=25,position=[0.,0.55,0.02,0.95],/vertical

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS TMAX > 35 CORRECTED SCN-CTL',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,(ss35_tasc2d_sm_2(*,*)-ss35_tasc2d_sm_1(*,*))/NYEARS/3.,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=min(levels_p),max=max(levels_p),divisions=N_ELEMENTS(levels_p-1),c_colors=colors_p,charsize=1,color=25,position=[0.97,0.55,0.99,0.95],/vertical

window,14,retain=2
!p.multi=[0,3,2]
;MIN T > 20
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS TMIN > 20 CTL',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,ssm20_tas2d_sm_1(*,*)/NYEARS/3.,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS TMIN > 20 SCN',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,ssm20_tas2d_sm_2(*,*)/NYEARS/3.,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=min(levels),max=max(levels),divisions=N_ELEMENTS(levels-1),c_colors=colors,charsize=1,color=25,position=[0.,0.55,0.02,0.95],/vertical

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS TMIN > 20 SCN-CTL',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,(ssm20_tas2d_sm_2(*,*)-ssm20_tas2d_sm_1(*,*))/NYEARS/3.,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=min(levels_p),max=max(levels_p),divisions=N_ELEMENTS(levels_p-1),c_colors=colors_p,charsize=1,color=25,position=[0.97,0.55,0.99,0.95],/vertical

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS TMIN > 20 CORRECTED CTL',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,ssm20_tasc2d_sm_1(*,*)/NYEARS/3.,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS TMIN > 20 CORRECTED SCN',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,ssm20_tasc2d_sm_2(*,*)/NYEARS/3.,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=min(levels),max=max(levels),divisions=N_ELEMENTS(levels-1),c_colors=colors,charsize=1,color=25,position=[0.,0.55,0.02,0.95],/vertical

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS TMIN > 20 CORRECTED SCN-CTL',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,(ssm20_tasc2d_sm_2(*,*)-ssm20_tasc2d_sm_1(*,*))/NYEARS/3.,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=min(levels_p),max=max(levels_p),divisions=N_ELEMENTS(levels_p-1),c_colors=colors_p,charsize=1,color=25,position=[0.97,0.55,0.99,0.95],/vertical

window,15,retain=2
!p.multi=[0,3,2]
;MIN T > 25
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS TMIN > 25 CTL',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,ssm25_tas2d_sm_1(*,*)/NYEARS/3.,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS TMIN > 25 SCN',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,ssm25_tas2d_sm_2(*,*)/NYEARS/3.,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=min(levels),max=max(levels),divisions=N_ELEMENTS(levels-1),c_colors=colors,charsize=1,color=25,position=[0.,0.55,0.02,0.95],/vertical

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS TMIN > 25 SCN-CTL',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,(ssm25_tas2d_sm_2(*,*)-ssm25_tas2d_sm_1(*,*))/NYEARS/3.,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=min(levels_p),max=max(levels_p),divisions=N_ELEMENTS(levels_p-1),c_colors=colors_p,charsize=1,color=25,position=[0.97,0.55,0.99,0.95],/vertical

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS TMIN > 25 CORRECTED CTL',charsize=1.5;,position=[0.05,0.55,0.45,0.95]
contour,ssm25_tasc2d_sm_1(*,*)/NYEARS/3.,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS TMIN > 25 CORRECTED SCN',charsize=1.5;,position=[0.5,0.55,0.9,0.95]
contour,ssm25_tasc2d_sm_2(*,*)/NYEARS/3.,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=min(levels),max=max(levels),divisions=N_ELEMENTS(levels-1),c_colors=colors,charsize=1,color=25,position=[0.,0.55,0.02,0.95],/vertical

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='No OF SUMMER DAYS TMIN > 25 CORRECTED SCN-CTL',charsize=1.5;,position=[0.05,0.05,0.45,0.45]
contour,(ssm25_tasc2d_sm_2(*,*)-ssm25_tasc2d_sm_1(*,*))/NYEARS/3.,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=min(levels_p),max=max(levels_p),divisions=N_ELEMENTS(levels_p-1),c_colors=colors_p,charsize=1,color=25,position=[0.97,0.55,0.99,0.95],/vertical

STOP




END
