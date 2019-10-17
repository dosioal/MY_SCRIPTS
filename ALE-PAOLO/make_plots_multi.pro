;
PRO make_plots_multi
;
DEVICE,decomposed=0
;
;
;-----------------------------------------------------------------------------------------------------------

home_data='/media/disk/POSTPROC/ALE-PAOLO/DATA/'
home_const='/media/disk/DATA/ENSOBS/ENS_MODELS/'

MODEL_ID=['ETHZ-CLM','DMI-HIRHAM5','KNMI-RACMO2']
CASE_ID=['SCN_HadCM3Q0','A1B_ECHAM5','A1B_ECHAM5-r3']
nmod=1.*N_ELEMENTS(MODEL_ID)

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

month=['01','02','03','04','05','06','07','08','09','10','11','12']
NDAYS=fltarr(12)
NDAYS_M=[31,31,28,31,30,31,30,31,31,30,31,30]
MM_I=0
MM_E=11


;===============================================
print,'OPENING MONTH ',month(0),' MODEL ',MODEL_ID(0)+'_'+CASE_ID(0)
ofile='DATA/'+VAR+'_SM_'+MODEL_ID(0)+'_'+CASE_ID(0)+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'.dat'
;tas2d_sm,stat_tas2d_sm,prob_tasc2d_sm,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c
restore,filename=ofile

landmask_m=dblarr(nxm,nym,nmod)
landmask_m(*,*,*)=0
tas2d_sm_m=FLTARR(nxm,nym,4,nmod+1) ;LAST MODEL IS ENS_AV
tasc2d_sm_m=FLTARR(nxm,nym,4,nmod+1) ;LAST MODEL IS ENS_AV
obs2d_sm_m=FLTARR(nxm,nym,4,nmod+1) ;LAST MODEL IS ENS_AV
stat_tas2d_sm_m=FLTARR(nxm,nym,4,nmod+1) ;LAST MODEL IS ENS_AV
stat_tasc2d_sm_m=FLTARR(nxm,nym,4,nmod+1) ;LAST MODEL IS ENS_AV
prob_tas2d_sm_m=FLTARR(nxm,nym,4,nmod+1) ;LAST MODEL IS ENS_AV
prob_tasc2d_sm_m=FLTARR(nxm,nym,4,nmod+1) ;LAST MODEL IS ENS_AV


;LOOP OVER MODELS
FOR imd=0,nmod-1 do begin ;START imd

print,'OPENING MODEL ',MODEL_ID(imd)+'_'+CASE_ID(imd)
ofile='DATA/'+VAR+'_SM_'+MODEL_ID(imd)+'_'+CASE_ID(imd)+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'.dat'
;tas2d_sm,stat_tas2d_sm,prob_tasc2d_sm,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c
restore,filename=ofile

ofile='DATA/'+VAR+'_SM_CORR_'+MODEL_ID(imd)+'_'+CASE_ID(imd)+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'.dat'
;tasc2d_sm,stat_tasc2d_sm,prob_tasc2d_sm,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c
restore,filename=ofile

ofile='DATA/OBS_'+VAR+'_SM_'+MODEL_ID(imd)+'_'+CASE_ID(imd)+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'.dat'
;obs2d_sm,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c
restore,filename=ofile

landmask_m(nx0(imd):nx0(imd)+nx(imd)-1,ny0(imd):ny0(imd)+ny(imd)-1,imd)=landmask_c(*,*)

tas2d_m(mnx0:mnx0+mnx-1,mny0:mny0+mny-1,*,imd)=tas2d_sm
tasc2d_m(mnx0:mnx0+mnx-1,mny0:mny0+mny-1,*,imd)=tasc2d_sm
obs2d_m(mnx0:mnx0+mnx-1,mny0:mny0+mny-1,*,imd)=obs2d_sm
stat_tas2d_m(mnx0:mnx0+mnx-1,mny0:mny0+mny-1,*,imd)=stat_tas2d_sm
stat_tasc2d_m(mnx0:mnx0+mnx-1,mny0:mny0+mny-1,*,imd)=stat_tasc2d_sm
prob_tas2d_m(mnx0:mnx0+mnx-1,mny0:mny0+mny-1,*,imd)=prob_tas2d_sm
prob_tasc2d_m(mnx0:mnx0+mnx-1,mny0:mny0+mny-1,*,imd)=prob_tasc2d_sm

bias_tas=tas2d_m-obs2d_m
bias_tasc=tasc2d_m-obs2d_m
;bias_tas_ave=tas2d_ave-REFORM(obs2d_m(*,*,*,0))
;bias_tasc_ave=tasc2d_ave-REFORM(obs2d_m(*,*,*,0)

;landmask_m(mnx0:mnx0+mnx-1,mny0:mny0+mny-1,imd)=landmask_c

ENDFOR ; END LOOP MODELS

print,'OPENING ENS AVERAGES','DATA/ENS_AVE_'+VAR+'_SM_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'.dat'

ofile='DATA/ENS_AVE_'+VAR+'_SM_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'.dat'
;tas2d_sm,stat_tas2d_sm,prob_tas2d_sm,nxm,nym,landmask_c
restore,filename=ofile
ofile='DATA/ENS_AVE_'+VAR+'_SM_CORR_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'.dat'
;tasc2d_sm,stat_tas2d_sm,prob_tas2d_sm,nxm,nym,landmask_c
restore,filename=ofile

tas2d_m(*,*,*,nmods)=tas2d_sm
tasc2d_m(*,*,*,nmods)=tasc2d_sm
obs2d_m(*,*,*,nmods)=obs2d_sm
stat_tas2d_m(*,*,*,nmods)=stat_tas2d_sm
stat_tasc2d_m(*,*,*,nmods)=stat_tasc2d_sm
prob_tas2d_m(*,*,*,nmods)=prob_tas2d_sm
prob_tasc2d_m(*,*,*,nmods)=prob_tasc2d_sm

bias_tas=tas2d_m-obs2d_m
bias_tasc=tasc2d_m-obs2d_m


landmask_com=fltarr(nxm,nym)
FOR ix=0,nxm-1 do begin
FOR iy=0,nym-1 do begin
      landmask_com(ix,iy)=MEAN(landmask_m(ix,iy,*))
      IF landmask_com(ix,iy) GT 0 THEN landmask_com(ix,iy)=1
ENDFOR
ENDFOR



;ENSEMBLE AVERAGE OF SEASONAL MEANS
;FOR j=0,3 DO BEGIN &$
;FOR ix=0,nxm-1 DO BEGIN &$
;FOR iy=0,nym-1 DO BEGIN &$
;ave_tas2d_sm(ix,iy,j)=MEAN(tas2d_sm(ix,iy,*,j))
;ave_tasc2d_sm(ix,iy,j)=MEAN(tasc2d_sm(ix,iy,*,j))
;ENDFOR
;ENDFOR
;ENDFOR
;ave_bias_tas2d_sm(*,*,*)=ave_tas2d_sm(*,*,*)-REFORM(obs2d_sm(*,*,0,*))
;ave_bias_tasc2d_sm(*,*,*)=ave_tasc2d_sm(*,*,*)-REFORM(obs2d_sm(*,*,0,*))

;PLOTS
i=9000 
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
ENDELSE
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

window,0,retain=2,xsize=1250,ysize=900
SetColorTable,ColorTBL
!p.background=0
!p.color=255
!p.multi=[0,5,nmod+1]
;OBS'
FOR im=0,nmod-1 DO BEGIN
contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS DJF';,position=[0.05,0.55,0.45,0.95]
contour,obs2d_sm(*,*,im,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=MODEL_ID(im)+' '+CASE_ID(im);,position=[0.5,0.55,0.9,0.95]
contour,tas2d_sm(*,*,im,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=MODEL_ID(im)+' '+CASE_ID(im)+' CORRECTED';,position=[0.05,0.05,0.45,0.45]
contour,tasc2d_sm(*,*,im,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title='BIAS';,position=[0.5,0.05,0.9,0.45]
contour,bias_tas2d_sm(*,*,im,0),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title='BIAS CORRECTED';,position=[0.5,0.05,0.9,0.45]
contour,bias_tasc2d_sm(*,*,im,0),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
ENDFOR

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title='OBS DJF';,position=[0.05,0.55,0.45,0.95]
contour,obs2d_sm(*,*,1,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title='ENSEMBLE MEAN';,position=[0.5,0.55,0.9,0.95]
contour,ave_tas2d_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title='ENSEMBLE MEAN CORRECTED';,position=[0.05,0.05,0.45,0.45]
contour,ave_tasc2d_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title='BIAS ENS MEAN';,position=[0.5,0.05,0.9,0.45]
contour,ave_bias_tas2d_sm(*,*,0),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title='BIAS ENS MEAN CORRECTED';,position=[0.5,0.05,0.9,0.45]
contour,ave_bias_tasc2d_sm(*,*,0),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1,color=25,position=[0.96,0.55,0.99,0.95],/vertical

window,1,retain=2,xsize=1250,ysize=900
SetColorTable,ColorTBL
!p.background=0
!p.color=255
!p.multi=[0,5,nmod+1]
;OBS'
FOR im=0,nmod-1 DO BEGIN
contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS MAM';,position=[0.05,0.55,0.45,0.95]
contour,obs2d_sm(*,*,im,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=MODEL_ID(im)+' '+CASE_ID(im);,position=[0.5,0.55,0.9,0.95]
contour,tas2d_sm(*,*,im,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=MODEL_ID(im)+' '+CASE_ID(im)+' CORRECTED';,position=[0.05,0.05,0.45,0.45]
contour,tasc2d_sm(*,*,im,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title='BIAS';,position=[0.5,0.05,0.9,0.45]
contour,bias_tas2d_sm(*,*,im,1),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title='BIAS CORRECTED';,position=[0.5,0.05,0.9,0.45]
contour,bias_tasc2d_sm(*,*,im,1),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
ENDFOR

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title='OBS MAM';,position=[0.05,0.55,0.45,0.95]
contour,obs2d_sm(*,*,1,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title='ENSEMBLE MEAN';,position=[0.5,0.55,0.9,0.95]
contour,ave_tas2d_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title='ENSEMBLE MEAN CORRECTED';,position=[0.05,0.05,0.45,0.45]
contour,ave_tasc2d_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title='BIAS ENS MEAN';,position=[0.5,0.05,0.9,0.45]
contour,ave_bias_tas2d_sm(*,*,1),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title='BIAS ENS MEAN CORRECTED';,position=[0.5,0.05,0.9,0.45]
contour,ave_bias_tasc2d_sm(*,*,1),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1,color=25,position=[0.96,0.55,0.99,0.95],/vertical

window,2,retain=2,xsize=1250,ysize=900
SetColorTable,ColorTBL
!p.background=0
!p.color=255
!p.multi=[0,5,nmod+1]
;OBS'
FOR im=0,nmod-1 DO BEGIN
contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS JJA';,position=[0.05,0.55,0.45,0.95]
contour,obs2d_sm(*,*,im,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=MODEL_ID(im)+' '+CASE_ID(im);,position=[0.5,0.55,0.9,0.95]
contour,tas2d_sm(*,*,im,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=MODEL_ID(im)+' '+CASE_ID(im)+' CORRECTED';,position=[0.05,0.05,0.45,0.45]
contour,tasc2d_sm(*,*,im,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title='BIAS';,position=[0.5,0.05,0.9,0.45]
contour,bias_tas2d_sm(*,*,im,2),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title='BIAS CORRECTED';,position=[0.5,0.05,0.9,0.45]
contour,bias_tasc2d_sm(*,*,im,2),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
ENDFOR

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title='OBS JJA';,position=[0.05,0.55,0.45,0.95]
contour,obs2d_sm(*,*,1,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title='ENSEMBLE MEAN';,position=[0.5,0.55,0.9,0.95]
contour,ave_tas2d_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title='ENSEMBLE MEAN CORRECTED';,position=[0.05,0.05,0.45,0.45]
contour,ave_tasc2d_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title='BIAS ENS MEAN';,position=[0.5,0.05,0.9,0.45]
contour,ave_bias_tas2d_sm(*,*,2),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title='BIAS ENS MEAN CORRECTED';,position=[0.5,0.05,0.9,0.45]
contour,ave_bias_tasc2d_sm(*,*,2),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1,color=25,position=[0.96,0.55,0.99,0.95],/vertical

window,3,retain=2,xsize=1250,ysize=900
SetColorTable,ColorTBL
!p.background=0
!p.color=255
!p.multi=[0,5,nmod+1]
;OBS'
FOR im=0,nmod-1 DO BEGIN
contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS SON';,position=[0.05,0.55,0.45,0.95]
contour,obs2d_sm(*,*,im,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=MODEL_ID(im)+' '+CASE_ID(im);,position=[0.5,0.55,0.9,0.95]
contour,tas2d_sm(*,*,im,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=MODEL_ID(im)+' '+CASE_ID(im)+' CORRECTED';,position=[0.05,0.05,0.45,0.45]
contour,tasc2d_sm(*,*,im,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title='BIAS';,position=[0.5,0.05,0.9,0.45]
contour,bias_tas2d_sm(*,*,im,3),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title='BIAS CORRECTED';,position=[0.5,0.05,0.9,0.45]
contour,bias_tasc2d_sm(*,*,im,3),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
ENDFOR

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title='OBS SON';,position=[0.05,0.55,0.45,0.95]
contour,obs2d_sm(*,*,1,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title='ENSEMBLE MEAN';,position=[0.5,0.55,0.9,0.95]
contour,ave_tas2d_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title='ENSEMBLE MEAN CORRECTED';,position=[0.05,0.05,0.45,0.45]
contour,ave_tasc2d_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title='BIAS ENS MEAN';,position=[0.5,0.05,0.9,0.45]
contour,ave_bias_tas2d_sm(*,*,3),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title='BIAS ENS MEAN CORRECTED';,position=[0.5,0.05,0.9,0.45]
contour,ave_bias_tasc2d_sm(*,*,3),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1,color=25,position=[0.96,0.55,0.99,0.95],/vertical
END
