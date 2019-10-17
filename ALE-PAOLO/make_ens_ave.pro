;
PRO make_ens_ave
;
DEVICE,decompose=0
;
;
;-----------------------------------------------------------------------------------------------------------
home_data='/media/disk/POSTPROC/BC_Regional/'
home_const='/media/disk/DATA/ENSOBS/ENS_MODELS/'

MODEL_ID=['C4IRCA3','DMI-HIRHAM5','DMI-HIRHAM5','ETHZ-CLM','KNMI-RACMO2','MPI-M-REMO','SMHIRCA','SMHIRCA']
CASE_ID=['A1B_HadCM3Q16','A1B_ARPEGE','A1B_ECHAM5','SCN_HadCM3Q0','A1B_ECHAM5-r3','SCN_ECHAM5','A1B_BCM','A1B_BCM']
nmod=1.*N_ELEMENTS(MODEL_ID)

MODEL_OBS='KNMI-RACMO2'
CASE_OBS='A1B_ECHAM5-r3'


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
MM_I=0
MM_E=11


;===============================================
print,'OPENING MONTH ',month(0),' MODEL ',MODEL_ID(0)+'_'+CASE_ID(0)
ofile='DATA/MOD_'+VAR+'_'+MODEL_ID(0)+'_'+CASE_ID(0)+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(0)+'.dat'
;tas2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
restore,filename=ofile

tas2d_m = FLTARR(nxm,nym,31.*NYEARS,nmod)
tas2d_m(*,*,*,*) = !VALUES.F_NAN
tasc2d_m = FLTARR(nxm,nym,31.*NYEARS,nmod)
tasc2d_m(*,*,*,*) = !VALUES.F_NAN
obs2d_m = FLTARR(nxm,nym,31.*NYEARS,nmod)
obs2d_m(*,*,*,*) = !VALUES.F_NAN

;LOOP OVER MONTHS
FOR im=MM_I,MM_E DO BEGIN
PRINT,'MONTH= ',month(im)

ofile='DATA/OBS_'+VAR+'_'+MODEL_OBS+'_'+CASE_OBS+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;obs2d,nxm,nym,nx_o,ny_o,nx0_o,ny0_o,landmask_a,ndays_o
restore,filename=ofile

obs2d_m(nx0_o:nx0_o+nx_o-1,ny0_o:ny0_o+ny_o-1,0:NDAYS_o-1)=obs2d

;LOOP OVER MODELS
FOR imd=0,nmod-1 do begin

print,'OPENING MONTH ',month(im),' MODEL ',MODEL_ID(imd)+'_'+CASE_ID(imd)
ofile='DATA/MOD_'+VAR+'_'+MODEL_ID(imd)+'_'+CASE_ID(imd)+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;tas2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
restore,filename=ofile

ofile='DATA/MOD_BC_'+VAR+'_CORR_'+MODEL_ID(imd)+'_'+CASE_ID(imd)+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;tasc2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
restore,filename=ofile

tas2d_m(mnx0:mnx0+mnx-1,mny0:mny0+mny-1,0:mNDAYS-1,imd)=tas2d
tasc2d_m(mnx0:mnx0+mnx-1,mny0:mny0+mny-1,0:mNDAYS-1,imd)=tasc2d
ENDFOR ;END MODEL LOOP

tas2d_ave=fltarr(nxm,nym,mndays)
tasc2d_ave=fltarr(nxm,nym,mndays)
obs2d_ave=fltarr(nxm,nym,mndays)
tas2d_ave(*,*,*)=!VALUES.F_NAN
tasc2d_ave(*,*,*)=!VALUES.F_NAN
obs2d_ave(*,*,*)=!VALUES.F_NAN

FOR i=0,mndays-1 do begin &$
FOR iy=0,nym-1 do begin &$
FOR ix=0,nxm-1 do begin &$
	tas2d_ave(ix,iy,i)=MEAN(tas2d_m(ix,iy,i,*),/NAN) &$
 	tasc2d_ave(ix,iy,i)=MEAN(tasc2d_m(ix,iy,i,*),/NAN) &$
ENDFOR &$
ENDFOR &$
ENDFOR

obs2d_ave(*,*,*)=REFORM(obs2d_m(*,*,0:NDAYS_o-1))

;SAVING ENSE AVERAGE DATA
print,'SAVING ENSEMBLE AVERAGE',month(im)
ofile='DATA/ENS_AVE_MOD_'+VAR+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
save,filename=ofile,tas2d_ave,nxm,nym,landmask_c,mndays
ofile='DATA/ENS_AVE_MOD_'+VAR+'_CORR_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
save,filename=ofile,tasc2d_ave,nxm,nym,landmask_c,mndays
ofile='DATA/ENS_AVE_OBS_'+VAR+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
save,filename=ofile,obs2d_ave,nxm,nym,landmask_c,mndays

ENDFOR ;END MONTH LOOP

STOP

p_tas2d_m=fltarr(nxm,nym,NDAYS(im),nmod)
p_tasc2d_m=fltarr(nxm,nym,NDAYS(im),nmod)
p_obs2d_m=fltarr(nxm,nym,NDAYS(im),nmod)
p_tas2d_ave=fltarr(nxm,nym,NDAYS(im))
p_tasc2d_ave=fltarr(nxm,nym,NDAYS(im))
p_tas2d_m(*,*,*,*)=-999.
p_tasc2d_m(*,*,*,*)=-999.
p_obs2d_m(*,*,*,*)=-999.
p_tas2d_ave(*,*,*)=-999.
p_tasc2d_ave(*,*,*)=-999.
p_tasc2d_mm_ave(*,*)=-999.

FOR id=0,NDAYS(im)-1 do begin &$
FOR ix=0,nxm-1 do begin &$
FOR iy=0,nym-1 do begin &$
FOR imd=0,nmod-1 do begin &$
	IF obs2d_m(ix,iy,id,imd) GE 0 THEN p_obs2d_m(ix,iy,id,imd)=obs2d_m(ix,iy,id,imd) &$
	IF tas2d_m(ix,iy,id,imd) GE 0 THEN p_tas2d_m(ix,iy,id,imd)=tas2d_m(ix,iy,id,imd) &$
	IF tasc2d_m(ix,iy,id,imd) GE 0 THEN p_tasc2d_m(ix,iy,id,imd)=tasc2d_m(ix,iy,id,imd) &$
ENDFOR &$
	IF tas2d_ave(ix,iy,id) GE 0 THEN p_tas2d_ave(ix,iy,id)=tas2d_ave(ix,iy,id) &$
	IF tasc2d_ave(ix,iy,id) GE 0 THEN p_tasc2d_ave(ix,iy,id)=tasc2d_ave(ix,iy,id) &$
ENDFOR &$
ENDFOR &$
ENDFOR


MinCol=20
MaxCol=245
Minvalue=230
Maxvalue=310
;Minvalue=270
;Maxvalue=320
ContourDist=5
rInterval=Maxvalue-Minvalue
iNumOfConts=FIX(rInterval/ContourDist)+1
rgrContValues = INDGEN(iNumOfConts)*ContourDist + MinValue
rgrCenterValues = FLTARR(N_ELEMENTS(rgrContValues)-1)
rgrColValues = INDGEN(iNumOfConts-1) * (MaxCol - MinCol) / (iNumOfConts-2) + MinCol
ColorTBL=62


window,0,retain=2,xsize=1250,ysize=900
SetColorTable,ColorTBL
!p.background=0
!p.color=255
;!p.multi=[0,5,nmod+1]
!p.multi=[0,3,nmod+1]
;OBS'
FOR imd=0,nmod-1 DO BEGIN &$
	contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS DJF' &$;,position=[0.05,0.55,0.45,0.95] 
	contour,p_obs2d_m(*,*,0,imd),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot &$
	contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255 &$

	contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=VAR+' MODEL DJF' &$;,position=[0.05,0.55,0.45,0.95] 
	contour,p_tas2d_m(*,*,0,imd),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot &$
	contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255 &$

	contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORR DJF' &$;,position=[0.05,0.55,0.45,0.95] 
	contour,p_tasc2d_m(*,*,0,imd),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot &$
	contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255 &$
ENDFOR
	contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS DJF' &$;,position=[0.05,0.55,0.45,0.95] 
	contour,p_obs2d_m(*,*,0,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot &$
	contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255 &$

	contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=VAR+' MODEL DJF' &$;,position=[0.05,0.55,0.45,0.95] 
	contour,p_tas2d_ave(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot &$
	contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255 &$

	contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORR DJF' &$;,position=[0.05,0.55,0.45,0.95] 
	contour,p_tasc2d_ave(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot &$
	contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255 &$

window,1,retain=2,xsize=1250,ysize=900
SetColorTable,ColorTBL
!p.background=0
!p.color=255
;!p.multi=[0,5,nmod+1]
!p.multi=[0,3,nmod+1]
;OBS'
FOR imd=0,nmod-1 DO BEGIN &$
	contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS DJF' &$;,position=[0.05,0.55,0.45,0.95] 
	contour,p_obs2d_mm(*,*,imd),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot &$
	contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255 &$

	contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=VAR+' MODEL DJF' &$;,position=[0.05,0.55,0.45,0.95] 
	contour,p_tas2d_mm(*,*,imd),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot &$
	contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255 &$

	contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORR DJF' &$;,position=[0.05,0.55,0.45,0.95] 
	contour,p_tasc2d_mm(*,*,imd),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot &$
	contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255 &$
ENDFOR
	contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS DJF' &$;,position=[0.05,0.55,0.45,0.95] 
	contour,p_obs2d_mm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot &$
	contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255 &$

	contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=VAR+' MODEL DJF' &$;,position=[0.05,0.55,0.45,0.95] 
	contour,p_tas2d_mm_ave(*,*),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot &$
	contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255 &$

	contour,landmask_com,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORR DJF' &$;,position=[0.05,0.55,0.45,0.95] 
	contour,p_tasc2d_mm_ave(*,*),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot &$
	contour,landmask_com,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255 &$


STOP


END
