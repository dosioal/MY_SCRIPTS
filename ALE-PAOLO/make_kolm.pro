;
PRO make_kolm
;
DEVICE,decompose=0
;
;
;-----------------------------------------------------------------------------------------------------------
home='/media/disk/POSTPROC/BC_Regional/'

;MODEL_ID='DMI-HIRHAM5'
;CASE_ID='A1B_ECHAM5'

MODEL_ID='ETHZ-CLM'
CASE_ID='SCN_HadCM3Q0'

;MODEL_ID='KNMI-RACMO2'
;CASE_ID='A1B_ECHAM5-r3'

;MODEL_ID='ENS_AVE'

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


land2d_m=dblarr(nxm,nym)
land2d_m(*,*)=0

tas2d_m = FLTARR(nxm,nym,31.*NYEARS)
tas2d_m(*,*,*,*) = !VALUES.F_NAN
tasc2d_m = FLTARR(nxm,nym,31.*NYEARS)
tasc2d_m(*,*,*,*) = !VALUES.F_NAN
obs2d_m = FLTARR(nxm,nym,31.*NYEARS)
obs2d_m(*,*,*,*) = !VALUES.F_NAN

stat_tas2d = FLTARR(nxm,nym)
stat_tas2d(*,*,*,*) = !VALUES.F_NAN
stat_tasc2d = FLTARR(nxm,nym)
stat_tasc2d(*,*,*,*) = !VALUES.F_NAN
prob_tas2d = FLTARR(nxm,nym)
prob_tas2d(*,*,*,*) = !VALUES.F_NAN
prob_tasc2d = FLTARR(nxm,nym)
prob_tasc2d(*,*,*,*) = !VALUES.F_NAN

;LOOP OVER MONTHS
FOR im=MM_I,MM_E DO BEGIN
PRINT,'MONTH= ',month(im)

;RESTORING DATA
IF MODEL_ID EQ 'ENS_AVE' THEN $
ofile='DATA/ENS_AVE_MOD_'+VAR+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'  $
restore,filename=ofile
;tas2d_ave,nxm,nym,landmask_c,mndays
ELSE $
ofile='DATA/MOD_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;tas2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile

IF MODEL_ID EQ 'ENS_AVE' THEN $
ofile='DATA/ENS_AVE_MOD_'+VAR+'_CORR_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'  $
restore,filename=ofile
;tas2d_ave,nxm,nym,landmask_c,mndays
ELSE $
ofile='DATA/MOD_BC_'+VAR+'_CORR_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;tasc2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile

IF MODEL_ID EQ 'ENS_AVE' THEN  BEGIN
ofile='DATA/ENS_AVE_OBS_'+VAR+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'  
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

;IF MODEL_ID EQ 'ENS_AVE' THEN BEGIN
;tas2d_m(mnx0:mnx0+mnx-1,mny0:mny0+mny-1,0:MNDAYS-1)=tas2d_ave
;tasc2d_m(mnx0:mnx0+mnx-1,mny0:mny0+mny-1,0:MNDAYS-1)=tasc2d_ave
;obs2d_m(mnx0:mnx0+mnx-1,mny0:mny0+mny-1,0:MNDAYS-1)=obs2d_ave
;ENDIF ELSE BEGIN
;tas2d_m(mnx0:mnx0+mnx-1,mny0:mny0+mny-1,0:MNDAYS-1)=tas2d
;tasc2d_m(mnx0:mnx0+mnx-1,mny0:mny0+mny-1,0:MNDAYS-1)=tasc2d
;obs2d_m(mnx0:mnx0+mnx-1,mny0:mny0+mny-1,0:MNDAYS-1)=obs2d
;ENDELSE

;land2d_m(mnx0:mnx0+mnx-1,mny0:mny0+mny-1)=landmask_c

;##########################
;KOLMOGOROV STATISTICS
;##########################

print,'CALCULATING KOLMOGOROV STATISTCIS FOR MONTH ',im+1

FOR ix=0,nxm-1 DO BEGIN
FOR iy=0,nym-1 DO BEGIN
IF MODEL_ID EQ 'ENS_AVE' THEN BEGIN
ww=reform(tas2d_ave(ix,iy,*))
wwc=reform(tasc2d_ave(ix,iy,*))
wwo=reform(obs2d_ave(ix,iy,*))
ENDIF ELSE BEGIN
ww=reform(tas2d(ix,iy,*))
wwc=reform(tasc2d(ix,iy,*))
wwo=reform(obs2d(ix,iy,*))
ENDELSE
IF landmask_c(ix,iy) GT 0 AND wwo(0) GE 0 THEN BEGIN
ks2,ww,MNDAYS,wwo,MNDAYS,stat,prob,fn1,fn2,sor1,sor2
stat_tas2d(ix,iy)=stat
prob_tas2d(ix,iy)=prob
ks2,wwc,MNDAYS,wwo,MNDAYS,stat,prob,fn1,fn2,sor1,sor2
stat_tasc2d(ix,iy)=stat
prob_tasc2d(ix,iy)=prob
ENDIF
ENDFOR
ENDFOR

xx=80
yy=140

PRINT,'KS STAT TAS= ',stat_tas2d(xx,yy),' PROB TAS= ',prob_tas2d(xx,yy)
PRINT,'KS STAT TAS COR= ',stat_tasc2d(xx,yy),' PROB TAS COR= ',prob_tasc2d(xx,yy)

;SAVING DATA
IF MODEL_ID EQ 'ENS_AVE' THEN BEGIN
ofile='DATA/KS_STAT_'+VAR+'_ENSE_AVE_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
save,filename=ofile,stat_tas2d,prob_tas2d,landmask_c,nxm,nym
ofile='DATA/KS_STAT_'+VAR+'_CORR_ENS_AVE_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
save,filename=ofile,stat_tasc2d,prob_tasc2d,landmask_c,nxm,nym
ENDIF ELSE BEGIN
ofile='DATA/KS_STAT_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
save,filename=ofile,stat_tas2d,prob_tas2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c
ofile='DATA/KS_STAT_'+VAR+'_CORR_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
save,filename=ofile,stat_tasc2d,prob_tasc2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c
ENDELSE

;ENDFOR ;END MONTH LOOP


p_stat_tas2d = FLTARR(nxm,nym)
p_stat_tas2d(*,*,*,*) = -999.
p_stat_tasc2d = FLTARR(nxm,nym)
p_stat_tasc2d(*,*,*,*) = -999.
p_prob_tas2d = FLTARR(nxm,nym)
p_prob_tas2d(*,*,*,*) = -999.
p_prob_tasc2d = FLTARR(nxm,nym)
p_prob_tasc2d(*,*,*,*) = -999.

FOR ix=0,nxm-1 do begin &$
FOR iy=0,nym-1 do begin &$
IF landmask_c(ix,iy) EQ 1 AND (stat_tas2d(ix,iy) GE 0) THEN BEGIN &$
	p_stat_tas2d(ix,iy)=stat_tas2d(ix,iy) &$
	p_stat_tasc2d(ix,iy)=stat_tasc2d(ix,iy) &$
	p_prob_tas2d(ix,iy)=prob_tas2d(ix,iy) &$
	p_prob_tasc2d(ix,iy)=prob_tasc2d(ix,iy) &$
ENDIF &$
ENDFOR &$
ENDFOR

;PLOTS FOR CHECK
window,0+im,retain=2

;STAT AND PROB
loadct,39
!p.background=255
!p.color=0
!p.multi=[0,2,2]

levels=findgen(11)/10.
colors=reverse(findgen(10)*19+15)
colors=reverse(findgen(10)*22+30)
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='STAT '+VAR+' MONTH '+month(im),position=[0.05,0.55,0.45,0.95]
contour,p_stat_tas2d(*,*),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5
colorbar,min=0,max=1,divisions=N_ELEMENTS(levels)-1,c_colors=colors,charsize=1.5,color=0,position=[0.98,0.55,0.99,0.95],/vertical

contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='STAT '+VAR+' CORRECTED',position=[0.5,0.55,0.9,0.95]
contour,p_stat_tasc2d(*,*),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5

levels=[0,1e-6,5e-5,1e-4,5e-4,1e-3,5e-3,1e-2,5e-2,1e-1,5e-1,1]
lev_c=['0','1e-5','5e-5','1e-4','5e-4','1e-3','5e-3','1e-2','5e-2','1e-1','5e-1','1']
colors=findgen(11)*20+30
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='PROB '+vAR,position=[0.05,0.05,0.45,0.45]
contour,p_prob_tas2d(*,*),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5
colorbar,c_colors=colors,levels=lev_c,divisions=N_ELEMENTS(levels)-1,charsize=1.5,color=0,position=[0.98,0.05,0.99,0.45],/vertical

contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='PROB '+VAR+' CORRECTED',position=[0.5,0.05,0.9,0.45]
contour,p_prob_tasc2d(*,*),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5


ENDFOR ;END MONTH LOOP

STOP
END
