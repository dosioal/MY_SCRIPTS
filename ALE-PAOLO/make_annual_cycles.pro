;
PRO make_annual_cycles
;
DEVICE,decompose=0
;
;
;-----------------------------------------------------------------------------------------------------------

MODEL_ID=['C4IRCA3','DMI-HIRHAM5','DMI-HIRHAM5','ETHZ-CLM','KNMI-RACMO2','MPI-M-REMO','SMHIRCA','SMHIRCA']
CASE_ID=['A1B_HadCM3Q16','A1B_ARPEGE','A1B_ECHAM5','SCN_HadCM3Q0','A1B_ECHAM5-r3','SCN_ECHAM5','A1B_BCM','A1B_ECHAM5-r3']
nmod=1.*N_ELEMENTS(MODEL_ID)

MODEL_OBS='KNMI-RACMO2'
CASE_OBS='A1B_ECHAM5-r3'

;ENS_AVE='ENS_AVE'


;VARIABLE (T,Ptot,Tmax,Tmin)
VAR='Ptot'
;VAR='T'

CONSTRUCTION_PERIOD_START  =  '1961'
CONSTRUCTION_PERIOD_STOP   =  '1990'

APPLICATION_PERIOD_START   =  '1991'
APPLICATION_PERIOD_STOP    =  '2000'

NYEARS=fix(APPLICATION_PERIOD_STOP)-fix(APPLICATION_PERIOD_START)+1

month=['01','02','03','04','05','06','07','08','09','10','11','12']
NMONTHS=12
NDAYS=fltarr(12)
NDAYS_M=[31,28,31,30,31,30,31,31,30,31,30,31]

nmask=8
mask=['BI','IP','FR','ME','SC','AL','MD','EA','EU']
BI=[40,80,100,150]
IP=[20,65,40,80]
FR=[50,80,60,100]
ME=[80,120,85,115]
SC=[85,165,115,189]
AL=[80,120,65,85]
MD=[80,150,25,65]
EA=[120,165,65,115]
EU=[5,80,35,75]

tas2d_ac=fltarr(12,nmask,nmod)
tas2d_ac(*,*,*) = !VALUES.F_NAN
tasc2d_ac=fltarr(12,nmask,nmod)
tasc2d_ac(*,*,*) = !VALUES.F_NAN
obs2d_ac=fltarr(12,nmask)
obs2d_ac(*,*) = !VALUES.F_NAN
tas2d_ave_ac=fltarr(12,nmask)
tas2d_ave_ac(*,*) = !VALUES.F_NAN
tasc2d_ave_ac=fltarr(12,nmask)
tasc2d_ave_ac(*,*) = !VALUES.F_NAN

;====================================================
;ENSE AVERAGE and COMMON GRID
;LOOP OVER MONTHS
FOR im=0,11 DO BEGIN ;STARTING MONTH IN SEASON
print,'OPENING MONTH ',month(im),' ENS AVERAGE '

ofile='DATA/ENS_AVE_MOD_'+VAR+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
restore,filename=ofile

ofile='DATA/ENS_AVE_MOD_'+VAR+'_CORR_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
restore,filename=ofile

FOR imm=0,nmask-1 DO BEGIN ; LOOP OVER MASKS
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
ww=REFORM(tas2d_ave(ma(0):ma(1),ma(2):ma(3),*))
tas2d_ave_ac(im,imm)=MEAN(ww,/NaN)
ww=REFORM(tasc2d_ave(ma(0):ma(1),ma(2):ma(3),*))
tasc2d_ave_ac(im,imm)=MEAN(ww,/NaN)
ENDFOR
ENDFOR

window,0,retain=2
contour,landmask_com
;BI
plots,BI(0),BI(2)
plots,BI(0),BI(3),/continue
plots,BI(1),BI(3),/continue
plots,BI(1),BI(2),/continue
plots,BI(0),BI(2),/continue
;IP
plots,IP(0),IP(2)
plots,IP(0),IP(3),/continue
plots,IP(1),IP(3),/continue
plots,IP(1),IP(2),/continue
plots,IP(0),IP(2),/continue
;FR
plots,FR(0),FR(2)
plots,FR(0),FR(3),/continue
plots,FR(1),FR(3),/continue
plots,FR(1),FR(2),/continue
plots,FR(0),FR(2),/continue
;ME
plots,ME(0),ME(2)
plots,ME(0),ME(3),/continue
plots,ME(1),ME(3),/continue
plots,ME(1),ME(2),/continue
plots,ME(0),ME(2),/continue
;SC
plots,SC(0),SC(2)
plots,SC(0),SC(3),/continue
plots,SC(1),SC(3),/continue
plots,SC(1),SC(2),/continue
plots,SC(0),SC(2),/continue
;AL
plots,AL(0),AL(2)
plots,AL(0),AL(3),/continue
plots,AL(1),AL(3),/continue
plots,AL(1),AL(2),/continue
plots,AL(0),AL(2),/continue
;MD
plots,MD(0),MD(2)
plots,MD(0),MD(3),/continue
plots,MD(1),MD(3),/continue
plots,MD(1),MD(2),/continue
plots,MD(0),MD(2),/continue
;EA
plots,EA(0),EA(2)
plots,EA(0),EA(3),/continue
plots,EA(1),EA(3),/continue
plots,EA(1),EA(2),/continue
plots,EA(0),EA(2),/continue

;STOP


;=========================
;MODEL LOOP
FOR imd=0,nmod-1 DO BEGIN

;LOOP OVER MONTHS
FOR im=0,11 DO BEGIN ;STARTING MONTH IN SEASON

print,'OPENING MONTH ',month(im),' MODEL ',MODEL_ID(imd)+'_'+CASE_ID(imd)

;RESTORING DATA
;DATA FILES

ofile='DATA/MOD_'+VAR+'_'+MODEL_ID(imd)+'_'+CASE_ID(imd)+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;tas2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile


ofile='DATA/MOD_BC_'+VAR+'_CORR_'+MODEL_ID(imd)+'_'+CASE_ID(imd)+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;tasc2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile

print,mnx,mny,mnx0,mny0

FOR imm=0,nmask-1 DO BEGIN ; LOOP OVER MASKS
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

ww=REFORM(tas2d(ma(0)-mnx0:ma(1)-mnx0,ma(2)-mny0:ma(3)-mny0,*))
tas2d_ac(im,imm,imd)=MEAN(ww,/NaN)
ww=REFORM(tasc2d(ma(0)-mnx0:ma(1)-mnx0,ma(2)-mny0:ma(3)-mny0,*))
tasc2d_ac(im,imm,imd)=MEAN(ww,/NaN)

ENDFOR  ;END MASK imm
ENDFOR  ;END MONTH im
ENDFOR ; END LOOP imd MODELS

;====================================================
;OBS
;LOOP OVER MONTHS
FOR im=0,11 DO BEGIN ;STARTING MONTH IN SEASON
print,'OPENING MONTH ',month(im),' MODEL OBS ',MODEL_OBS

ofile='DATA/OBS_'+VAR+'_'+MODEL_OBS+'_'+CASE_OBS+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
restore,filename=ofile
help,obs2d

FOR imm=0,nmask-1 DO BEGIN ; LOOP OVER MASKS
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
ww=REFORM(obs2d(ma(0)-nx0_o:ma(1)-nx0_o,ma(2)-ny0_o:ma(3)-ny0_o,*))
obs2d_ac(im,imm)=MEAN(ww,/NaN)
ENDFOR
ENDFOR


;=======================================
;PLOTS
IF var EQ 'Ptot' THEN BEGIN &$
	yrange=[0,15]  &$
ENDIF ELSE BEGIN &$
	yrange=[-5,25] &$
	obs2d_ac=obs2d_ac-273.15 &$
	tas2d_ac=tas2d_ac-273.15 &$
	tasc2d_ac=tasc2d_ac-273.15 &$
	tas2d_ave_ac=tas2d_ave_ac-273.15 &$
	tasc2d_ave_ac=tasc2d_ave_ac-273.15 &$
ENDELSE

tas2d_ac_max=FLTARR(12,nmask)
tas2d_ac_min=FLTARR(12,nmask)
tasc2d_ac_max=FLTARR(12,nmask)
tasc2d_ac_min=FLTARR(12,nmask)
FOR im=0,11 DO BEGIN  &$; LOOP OVER MONTHs
FOR imm=0,nmask-1 DO BEGIN  &$; &$ LOOP OVER MASKS
	tas2d_ac_max(im,imm)=MAX(tas2d_ac(im,imm,*)) &$
	tas2d_ac_min(im,imm)=MIN(tas2d_ac(im,imm,*)) &$
	tasc2d_ac_max(im,imm)=MAX(tasc2d_ac(im,imm,*)) &$
	tasc2d_ac_min(im,imm)=MIN(tasc2d_ac(im,imm,*)) &$
ENDFOR &$
ENDFOR



window,0,retain=2,ysize=750
loadct,39
!p.color=0
!p.background=255
!p.multi=[0,2,4]
FOR imm=0,nmask-1 DO BEGIN &$
	plot,obs2d_ac(*,imm),title=var+' MASK = '+mask(imm),color=0,thick=2.5,charsize=2.5,yrange=yrange &$
	oplot,tas2d_ave_ac(*,imm),color=250,thick=2.5 &$
	oplot,tas2d_ac_max(*,imm) &$
	oplot,tas2d_ac_min(*,imm) &$
	;FOR imd=0,nmod-1 DO BEGIN &$
	;	oplot,tas2d_ac(*,imm,imd)-273.15,color=25*(imd+1) &$
	;ENDFOR &$
ENDFOR

window,1,retain=2,ysize=750
loadct,39
!p.color=0
!p.background=255
!p.multi=[0,2,4]
FOR imm=0,nmask-1 DO BEGIN &$
	plot,obs2d_ac(*,imm),title=var+' CORRECTED MASK = '+mask(imm),color=0,thick=2.5,charsize=2.5 ,yrange=yrange &$
	oplot,tasc2d_ave_ac(*,imm),color=250,thick=2.5 &$
	oplot,tasc2d_ac_max(*,imm) &$
	oplot,tasc2d_ac_min(*,imm) &$
	;FOR imd=0,nmod-1 DO BEGIN &$
	;	oplot,tasc2d_ac(*,imm,imd)-273.15,color=25*(imd+1) &$
	;ENDFOR &$
ENDFOR

window,2,retain=2,ysize=750
loadct,39
!p.color=0
!p.background=255
!p.multi=[0,2,4]
FOR imm=0,nmask-1 DO BEGIN &$
	;plot,tas2d_ac(*,imm,0)-obs2d_ac(*,imm),title='BIAS '+var+' MASK = '+mask(imm),thick=2.5,charsize=2.5,yrange=[-5,5] &$
	plot,tas2d_ave_ac(*,imm)-obs2d_ac(*,imm),title='BIAS '+var+' MASK = '+mask(imm),thick=2.5,charsize=2.5,yrange=[-5,5] &$
	oplot,tas2d_ac_max(*,imm)-obs2d_ac(*,imm) &$
	oplot,tas2d_ac_min(*,imm)-obs2d_ac(*,imm) &$
	FOR imd=1,nmod-1 DO BEGIN &$
		oplot,tas2d_ac(*,imm,imd)-obs2d_ac(*,imm),color=25*(imd+1) &$
	ENDFOR &$
ENDFOR

window,3,retain=2,ysize=750
loadct,39
!p.color=0
!p.background=255
!p.multi=[0,2,4]
FOR imm=0,nmask-1 DO BEGIN &$
	;plot,tasc2d_ac(*,imm,0)-obs2d_ac(*,imm),title='BIAS '+var+' CORRECTED MASK = '+mask(imm),thick=2.5,charsize=2.5,yrange=[-5,5] &$
	plot,tasc2d_ave_ac(*,imm)-obs2d_ac(*,imm),title='BIAS '+var+' CORRECTED MASK = '+mask(imm),thick=2.5,charsize=2.5,yrange=[-5,5] &$
	oplot,tasc2d_ac_max(*,imm)-obs2d_ac(*,imm) &$
	oplot,tasc2d_ac_min(*,imm)-obs2d_ac(*,imm) &$
	;FOR imd=1,nmod-1 DO BEGIN &$
	;	oplot,tasc2d_ac(*,imm,imd)-obs2d_ac(*,imm),color=25*(imd+1) &$
	;ENDFOR &$
ENDFOR


STOP
END
