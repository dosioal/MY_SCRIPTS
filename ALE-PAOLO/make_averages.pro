;
PRO make_averages
;
DEVICE,decomposed=0
;
;
;-----------------------------------------------------------------------------------------------------------

home='/media/disk/POSTPROC/BC_Regional/'

MODEL_ID='DMI-HIRHAM5'
CASE_ID='A1B_ECHAM5'

;MODEL_ID='ETHZ-CLM'
;CASE_ID='SCN_HadCM3Q0'

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

;ND FOR SEASONAL EAN MONTHS START FROM DECEMBER!
month=['12','01','02','03','04','05','06','07','08','09','10','11']
NDAYS_M=[31,31,28,31,30,31,30,31,31,30,31,30]
MM_I=0
MM_E=11

;-----------------------------------------------------------------------------------------------------------
;

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


landmask_c=dblarr(nxm,nym)
landmask_c(*,*)=0

tas2d_m = FLTARR(nxm,nym,3,31.*NYEARS)
tas2d_m(*,*,*,*,*) = !VALUES.F_NAN
tasc2d_m = FLTARR(nxm,nym,3,31.*NYEARS)
tasc2d_m(*,*,*,*,*) = !VALUES.F_NAN
obs2d_m = FLTARR(nxm,nym,3,31.*NYEARS)
obs2d_m(*,*,*,*,*) = !VALUES.F_NAN
stat_tas2d_m = FLTARR(nxm,nym,3)
stat_tasc2d_m = FLTARR(nxm,nym,3)
prob_tas2d_m = FLTARR(nxm,nym,3)
prob_tasc2d_m = FLTARR(nxm,nym,3)
stat_tas2d_m(*,*,*)=!VALUES.F_NAN
stat_tasc2d_m(*,*,*)=!VALUES.F_NAN
prob_tas2d_m(*,*,*)=!VALUES.F_NAN
prob_tasc2d_m(*,*,*)=!VALUES.F_NAN

tas2d_sm = FLTARR(nxm,nym,4)
tasc2d_sm = FLTARR(nxm,nym,4)
obs2d_sm = FLTARR(nxm,nym,4)
tas2d_sm(*,*,*,*)=!VALUES.F_NAN
tasc2d_sm(*,*,*,*)=!VALUES.F_NAN
obs2d_sm(*,*,*,*)=!VALUES.F_NAN

stat_tas2d_sm = FLTARR(nxm,nym,4)
stat_tasc2d_sm = FLTARR(nxm,nym,4)
prob_tas2d_sm = FLTARR(nxm,nym,4)
prob_tasc2d_sm = FLTARR(nxm,nym,4)
stat_tas2d_sm(*,*,*,*)=!VALUES.F_NAN
stat_tasc2d_sm(*,*,*,*)=!VALUES.F_NAN
prob_tas2d_sm(*,*,*,*)=!VALUES.F_NAN
prob_tasc2d_sm(*,*,*,*)=!VALUES.F_NAN

window,5,retain=2
!p.multi=[0,2,2]

;LOOP OVER MONTHS
ism=0
FOR jm=0,11,3 DO BEGIN
imm=0
FOR im=jm,jm+2 DO BEGIN
PRINT,'SEASON = ',ism
PRINT,'MONTH IN YEAR im = ',month(im)
PRINT,'MONTH IN SEASON imm = ',imm

;RESTORING DATA
;TAS
IF MODEL_ID EQ 'ENS_AVE' THEN $
ofile='DATA/ENS_AVE_MOD_'+VAR+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'  $
;tas2d_ave,nxm,nym,landmask_c,mndays
ELSE $
ofile='DATA/MOD_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;tas2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile

;TASC
IF MODEL_ID EQ 'ENS_AVE' THEN $
ofile='DATA/ENS_AVE_MOD_'+VAR+'_CORR_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'  $
;tas2d_ave,nxm,nym,landmask_c,mndays
ELSE $
ofile='DATA/MOD_BC_'+VAR+'_CORR_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;tasc2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile

;OBS
IF MODEL_ID EQ 'ENS_AVE' THEN $
ofile='DATA/ENS_AVE_OBS_'+VAR+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'  $
;obs2d_ave,nxm,nym,landmask_c,mndays
ELSE $
ofile='DATA/OBS_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;obs2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
restore,filename=ofile
print,'RESTORING '+ofile

;STAT
IF MODEL_ID EQ 'ENS_AVE' THEN $
ofile='DATA/KS_STAT_'+VAR+'_ENSE_AVE_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'  $
;stat_tas2d,prob_tas2d,landmask_c,nxm,nym
restore,filename=ofile
ELSE $
ofile='DATA/KS_STAT_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;stat_tas2d,prob_tas2d,landmask_c,nxm,nym
restore,filename=ofile

;STATC
IF MODEL_ID EQ 'ENS_AVE' THEN BEGIN 
ofile='DATA/KS_STAT_'+VAR+'CORR_ENSE_AVE_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'  
;stat_tasc2d,prob_tasc2d,landmask_c,nxm,nym
restore,filename=ofile
ENDIF ELSE BEGIN
ofile='DATA/KS_STAT_'+VAR+'_CORR_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;stat_tasc2d,prob_tasc2d,landmask_c,nxm,nym,mnx,mny
restore,filename=ofile
nxm=mnx
nym=mny
ENDELSE

;IF MODEL_ID EQ 'ENS_AVE' THEN BEGIN
;tas2d_m(mnx0:mnx0+mnx-1,mny0:mny0+mny-1,imm,0:MNDAYS-1)=tas2d_ave
;tasc2d_m(mnx0:mnx0+mnx-1,mny0:mny0+mny-1,imm,0:MNDAYS-1)=tasc2d_ave
;obs2d_m(mnx0:mnx0+mnx-1,mny0:mny0+mny-1,imm,0:MNDAYS-1)=obs2d_ave
;ENDIF ELSE BEGIN
;tas2d_m(mnx0:mnx0+mnx-1,mny0:mny0+mny-1,imm,0:MNDAYS-1)=tas2d
;tasc2d_m(mnx0:mnx0+mnx-1,mny0:mny0+mny-1,imm,0:MNDAYS-1)=tasc2d
;obs2d_m(mnx0:mnx0+mnx-1,mny0:mny0+mny-1,imm,0:MNDAYS-1)=obs2d
;ENDELSE
;stat_tas2d_mm(mnx0:mnx0+mnx-1,mny0:mny0+mny-1,imm)=stat_tas2d_m
;stat_tas2d_mm(mnx0:mnx0+mnx-1,mny0:mny0+mny-1,imm)=stat_tas2d_m
;prob_tasc2d_mm(mnx0:mnx0+mnx-1,mny0:mny0+mny-1,imm)=prob_tasc2d_m
;prob_tasc2d_mm(mnx0:mnx0+mnx-1,mny0:mny0+mny-1,imm)=prob_tasc2d_m

;landmask_c(mnx0:mnx0+mnx-1,mny0:mny0+mny-1)=landmask_c

IF MODEL_ID EQ 'ENS_AVE' THEN BEGIN
tas2d_m(*,*,imm,0:MNDAYS-1)=tas2d_ave
tasc2d_m(*,*,imm,0:MNDAYS-1)=tasc2d_ave
obs2d_m(*,*,imm,0:MNDAYS-1)=obs2d_ave
ENDIF ELSE BEGIN
tas2d_m(*,*,imm,0:MNDAYS-1)=tas2d
tasc2d_m(*,*,imm,0:MNDAYS-1)=tasc2d
obs2d_m(*,*,imm,0:MNDAYS-1)=obs2d
ENDELSE
stat_tas2d_m(*,*,imm)=stat_tas2d
stat_tasc2d_m(*,*,imm)=stat_tasc2d
prob_tas2d_m(*,*,imm)=prob_tas2d
prob_tasc2d_m(*,*,imm)=prob_tasc2d

xx=80
yy=140
PRINT,'KS STAT TAS= ',stat_tas2d_m(xx,yy,imm),' PROB TAS= ',prob_tas2d_m(xx,yy,imm)
PRINT,'KS STAT TAS COR= ',stat_tasc2d_m(xx,yy,imm),' PROB TAS COR= ',prob_tasc2d_m(xx,yy,imm)


imm=imm+1

IF imm GT 2 THEN imm=0
ENDFOR  ;END im

tas_w=fltarr(nxm,nym,3*NYEARS*31)
;SEASONAL AVERAGES
print,'CALCULATING SEASONAL MEAN FOR SEASON ',ism,' MONTH ',im,jm
;TAS
tas_w(*,*,0:309)=REFORM(tas2d_m(*,*,0,*))
tas_w(*,*,310:619)=REFORM(tas2d_m(*,*,1,*))
tas_w(*,*,620:929)=REFORM(tas2d_m(*,*,2,*))
FOR ix=0,nxm-1 DO BEGIN
FOR iy=0,nym-1 DO BEGIN
tas2d_sm(ix,iy,ism)=MEAN(tas_w(ix,iy,*),/NAN)
ENDFOR
ENDFOR

plot,tas_w(100,100,*)-273.14
plots,[0,929],[tas2d_sm(xx,yy,ism)-273.14,tas2d_sm(xx,yy,ism)-273.14]


;TASC
tas_w(*,*,0:309)=REFORM(tasc2d_m(*,*,0,*))
tas_w(*,*,310:619)=REFORM(tasc2d_m(*,*,1,*))
tas_w(*,*,620:929)=REFORM(tasc2d_m(*,*,2,*))
FOR ix=0,mnx-1 DO BEGIN
FOR iy=0,mny-1 DO BEGIN
tasc2d_sm(ix,iy,ism)=MEAN(tas_w(ix,iy,*),/NAN)
ENDFOR
ENDFOR
;OBS
tas_w(*,*,0:309)=REFORM(obs2d_m(*,*,0,*))
tas_w(*,*,310:619)=REFORM(obs2d_m(*,*,1,*))
tas_w(*,*,620:929)=REFORM(obs2d_m(*,*,2,*))
FOR ix=0,mnx-1 DO BEGIN
FOR iy=0,mny-1 DO BEGIN
obs2d_sm(ix,iy,ism)=MEAN(tas_w(ix,iy,*),/NAN)
ENDFOR
ENDFOR

FOR ix=0,mnx-1 DO BEGIN
FOR iy=0,mny-1 DO BEGIN
stat_tas2d_sm(ix,iy,ism)=MEAN(stat_tas2d_m(ix,iy,*),/NAN)
stat_tasc2d_sm(ix,iy,ism)=MEAN(stat_tasc2d_m(ix,iy,*),/NAN)
prob_tas2d_sm(ix,iy,ism)=MEAN(prob_tas2d_m(ix,iy,*),/NAN)
prob_tasc2d_sm(ix,iy,ism)=MEAN(prob_tasc2d_m(ix,iy,*),/NAN)
ENDFOR
ENDFOR
PRINT,'KS STAT TAS SM= ',stat_tas2d_sm(xx,yy,ism),' PROB TAS= ',prob_tas2d_sm(xx,yy,ism)
PRINT,'KS STAT TAS COR SM= ',stat_tasc2d_sm(xx,yy,ism),' PROB TAS COR= ',prob_tasc2d_sm(xx,yy,ism)

;FOR i=0,NPOINTS_M-1 DO BEGIN
;STAT_TAS
;tas_w(0:309)=st_tas(i,0)
;tas_w(310:619)=st_tas(i,1)
;tas_w(620:929)=st_tas(i,2)
;stat_tas_sm(i,ism)=MEAN(tas_w,/NAN)
;;STAT_TASC
;tas_w(0:309)=st_tasc(i,0)
;tas_w(310:619)=st_tasc(i,1)
;tas_w(620:929)=st_tasc(i,2)
;stat_tasc_sm(i,ism)=MEAN(tas_w,/NAN)
;;PROB_TAS
;tas_w(0:309)=pr_tas(i,0)
;tas_w(310:619)=pr_tas(i,1)
;tas_w(620:929)=pr_tas(i,2)
;prob_tas_sm(i,ism)=MEAN(tas_w,/NAN)
;;PROB_TASC
;tas_w(0:309)=pr_tasc(i,0)
;tas_w(310:619)=pr_tasc(i,1)
;tas_w(620:929)=pr_tasc(i,2)
;prob_tasc_sm(i,ism)=MEAN(tas_w,/NAN)
;ENDFOR
;print,tas_sm(9000,*)

ism=ism+1

ENDFOR ; END LOOP MONTHS
;STOP

;tas2d_sm = FLTARR(nx,ny,4)
;tasc2d_sm = FLTARR(nx,ny,4)
;obs2d_sm = FLTARR(nx,ny,4)
;stat_tas2d_sm = FLTARR(nx,ny,4)
;stat_tasc2d_sm = FLTARR(nx,ny,4)
;;stat_tas2d_sm(*,*,*)=!VALUES.F_NAN
;tas2d_sm(*,*,*)=-999.
;tasc2d_sm(*,*,*)=-999.
;obs2d_sm(*,*,*)=-999.
;stat_tas2d_sm(*,*,*)=-999.
;stat_tasc2d_sm(*,*,*)=-999.
;prob_tas2d_sm = FLTARR(nx,ny,4)
;prob_tasc2d_sm = FLTARR(nx,ny,4)
;prob_tas2d_sm(*,*,*)=-999.
;prob_tasc2d_sm(*,*,*)=-999.
;FOR j=0,3 DO BEGIN &$
;FOR i=0L,NPOINTS_M-1 DO BEGIN &$
;         tas2d_sm(lon_m(i),lat_m(i),j)=tas_sm(i,j) &$
;         obs2d_sm(lon_m(i),lat_m(i),j)=obs_sm(i,j) &$
;         tasc2d_sm(lon_m(i),lat_m(i),j)=tasc_sm(i,j) &$
;         stat_tas2d_sm(lon_m(i),lat_m(i),j)=stat_tas_sm(i,j) &$
;         stat_tasc2d_sm(lon_m(i),lat_m(i),j)=stat_tasc_sm(i,j) &$
;         prob_tas2d_sm(lon_m(i),lat_m(i),j)=prob_tas_sm(i,j) &$
;         prob_tasc2d_sm(lon_m(i),lat_m(i),j)=prob_tasc_sm(i,j) &$
;ENDFOR &$
;ENDFOR

;SAVING DATA
season=['DJF','MAM','JJA','SON']
IF MODEL_ID EQ 'ENS_AVE' THEN BEGIN
ofile='DATA/ENS_AVE_'+VAR+'_SM_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'.dat'
save,filename=ofile,tas2d_sm,stat_tas2d_sm,prob_tas2d_sm,nxm,nym,landmask_c
ofile='DATA/ENS_AVE_'+VAR+'_SM_CORR_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'.dat'
save,filename=ofile,tasc2d_sm,stat_tasc2d_sm,prob_tasc2d_sm,nxm,nym,landmask_c
ofile='DATA/ENS_AVE_OBS_'+VAR+'_SM_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'.dat'
save,filename=ofile,obs2d_sm,nxm,nym,landmask_c
ENDIF ELSE BEGIN
ofile='DATA/'+VAR+'_SM_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'.dat'
save,filename=ofile,tas2d_sm,stat_tas2d_sm,prob_tas2d_sm,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c
ofile='DATA/'+VAR+'_SM_CORR_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'.dat'
save,filename=ofile,tasc2d_sm,stat_tasc2d_sm,prob_tasc2d_sm,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c
ofile='DATA/OBS_'+VAR+'_SM_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'.dat'
save,filename=ofile,obs2d_sm,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c
ENDELSE

p_tas2d_sm=fltarr(nxm,nym,4)
p_tasc2d_sm=fltarr(nxm,nym,4)
p_obs2d_sm=fltarr(nxm,nym,4)
p_stat_tas2d_sm=fltarr(nxm,nym,4)
p_stat_tasc2d_sm=fltarr(nxm,nym,4)
p_prob_tas2d_sm=fltarr(nxm,nym,4)
p_prob_tasc2d_sm=fltarr(nxm,nym,4)
p_tas2d_sm(*,*,*)=-999.
p_tasc2d_sm(*,*,*)=-999.
p_stat_tas2d_sm(*,*,*)=-999.
p_stat_tasc2d_sm(*,*,*)=-999.
p_prob_tas2d_sm(*,*,*)=-999.
p_prob_tasc2d_sm(*,*,*)=-999.
p_obs2d_sm(*,*,*)=-999.

FOR id=0,3 do begin &$
FOR ix=0,nxm-1 do begin &$
FOR iy=0,nym-1 do begin &$
IF obs2d_sm(ix,iy,id) GE 0 THEN p_obs2d_sm(ix,iy,id)=obs2d_sm(ix,iy,id) &$
IF tas2d_sm(ix,iy,id) GE 0 THEN p_tas2d_sm(ix,iy,id)=tas2d_sm(ix,iy,id) &$
IF tasc2d_sm(ix,iy,id) GE 0 THEN p_tasc2d_sm(ix,iy,id)=tasc2d_sm(ix,iy,id) &$
IF stat_tas2d_sm(ix,iy,id) GE 0 THEN p_stat_tas2d_sm(ix,iy,id)=stat_tas2d_sm(ix,iy,id) &$
IF stat_tasc2d_sm(ix,iy,id) GE 0 THEN p_stat_tasc2d_sm(ix,iy,id)=stat_tasc2d_sm(ix,iy,id) &$
IF prob_tas2d_sm(ix,iy,id) GE 0 THEN p_prob_tas2d_sm(ix,iy,id)=prob_tas2d_sm(ix,iy,id) &$
IF prob_tasc2d_sm(ix,iy,id) GE 0 THEN p_prob_tasc2d_sm(ix,iy,id)=prob_tasc2d_sm(ix,iy,id) &$
ENDFOR &$
ENDFOR &$
ENDFOR 

bias_tas=p_tas2d_sm-p_obs2d_sm
bias_tasc=p_tasc2d_sm-p_obs2d_sm


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
contour,p_tas2d_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' MAM',position=[0.5,0.55,0.9,0.95]
contour,p_tas2d_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' JJA',position=[0.05,0.05,0.45,0.45]
contour,p_tas2d_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' SON',position=[0.5,0.05,0.9,0.45]
contour,p_tas2d_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1,color=25,position=[0.96,0.55,0.99,0.95],/vertical

window,1,retain=2
!p.multi=[0,2,2]
;TAS CORR'
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORRECTED DJF',position=[0.05,0.55,0.45,0.95]
contour,p_tasc2d_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORRECTED MAM',position=[0.5,0.55,0.9,0.95]
contour,p_tasc2d_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORRECTED JJA',position=[0.05,0.05,0.45,0.45]
contour,p_tasc2d_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORRECTED SON',position=[0.5,0.05,0.9,0.45]
contour,p_tasc2d_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1,color=25,position=[0.96,0.55,0.99,0.95],/vertical

window,2,retain=2
!p.multi=[0,2,2]
;OBS'
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS DJF',position=[0.05,0.55,0.45,0.95]
contour,p_obs2d_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS MAM',position=[0.5,0.55,0.9,0.95]
contour,p_obs2d_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS JJA',position=[0.05,0.05,0.45,0.45]
contour,p_obs2d_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS SON',position=[0.5,0.05,0.9,0.45]
contour,p_obs2d_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
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

window,10,retain=2
!p.multi=[0,2,2]
;TAS CORR'
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='BIAS '+VAR+' DJF',position=[0.05,0.55,0.45,0.95]
contour,bias_tas(*,*,0),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' MAM',position=[0.5,0.55,0.9,0.95]
contour,bias_tas(*,*,1),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' JJA',position=[0.05,0.05,0.45,0.45]
contour,bias_tas(*,*,2),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' SON',position=[0.5,0.05,0.9,0.45]
contour,bias_tas(*,*,3),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue_b,max=maxvalue_b,c_colors=rgrColValues_b,divisions=iNumOfConts_b-1,charsize=1,color=25,position=[0.96,0.55,0.99,0.95],/vertical

window,11,retain=2
!p.multi=[0,2,2]
;OBS'
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title='BIAS '+VAR+' CORRECTED DJF',position=[0.05,0.55,0.45,0.95]
contour,bias_tasc(*,*,0),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' MAM',position=[0.5,0.55,0.9,0.95]
contour,bias_tasc(*,*,1),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' JJA',position=[0.05,0.05,0.45,0.45]
contour,bias_tasc(*,*,2),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' SON',position=[0.5,0.05,0.9,0.45]
contour,bias_tasc(*,*,3),levels=rgrContValues_b,/fill,c_colors=rgrColValues_b,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue_b,max=maxvalue_b,c_colors=rgrColValues_b,divisions=iNumOfConts_b-1,charsize=1,color=25,position=[0.96,0.55,0.99,0.95],/vertical




;STAT
loadct,39
!p.background=255
!p.color=0
!p.multi=[0,2,2]
levels=findgen(11)/10.
colors=reverse(findgen(10)*19+15)
colors=reverse(findgen(10)*20+30)
colors=reverse(findgen(10)*22+30)


window,6,retain=2
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='STAT '+VAR,position=[0.05,0.55,0.45,0.95]
contour,p_stat_tas2d_sm(*,*,0),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5

contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='STAT '+VAR,position=[0.5,0.55,0.9,0.95]
contour,p_stat_tas2d_sm(*,*,1),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5

contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='STAT '+VAR,position=[0.05,0.05,0.45,0.45]
contour,p_stat_tas2d_sm(*,*,2),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5

contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='STAT '+VAR,position=[0.5,0.05,0.9,0.45]
contour,p_stat_tas2d_sm(*,*,3),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5
colorbar,min=0,max=1,divisions=N_ELEMENTS(levels)-1,c_colors=colors,charsize=1,color=0,position=[0.96,0.55,0.99,0.95],/vertical

window,4,retain=2
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='STAT '+VAR+' CORRECTED',position=[0.05,0.55,0.45,0.95]
contour,p_stat_tasc2d_sm(*,*,0),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5

contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='STAT '+VAR,position=[0.5,0.55,0.9,0.95]
contour,p_stat_tasc2d_sm(*,*,1),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5

contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='STAT '+VAR,position=[0.05,0.05,0.45,0.45]
contour,p_stat_tasc2d_sm(*,*,2),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5

contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='STAT '+VAR,position=[0.5,0.05,0.9,0.45]
contour,p_stat_tasc2d_sm(*,*,3),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5
colorbar,min=0,max=1,divisions=N_ELEMENTS(levels)-1,c_colors=colors,charsize=1,color=0,position=[0.96,0.55,0.99,0.95],/vertical

;PROB
levels=[0,1e-6,5e-5,1e-4,5e-4,1e-3,5e-3,1e-2,5e-2,1e-1,5e-1,1]
lev_c=['0','1e-5','5e-5','1e-4','5e-4','1e-3','5e-3','1e-2','5e-2','1e-1','5e-1','1']
colors=findgen(11)*20+30
;colors(0)=255
window,3,retain=2
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='PROB '+VAR,position=[0.05,0.55,0.45,0.95]
contour,p_prob_tas2d_sm(*,*,0),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5

contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='PROB '+VAR,position=[0.5,0.55,0.9,0.95]
contour,p_prob_tas2d_sm(*,*,1),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5

contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='PROB '+VAR,position=[0.05,0.05,0.45,0.45]
contour,p_prob_tas2d_sm(*,*,2),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5

contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='PROB '+VAR,position=[0.5,0.05,0.9,0.45]
contour,p_prob_tas2d_sm(*,*,3),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5
colorbar,levels=lev_c,divisions=N_ELEMENTS(levels)-1,c_colors=colors,charsize=1,color=0,position=[0.96,0.55,0.99,0.95],/vertical

window,5,retain=2
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='PROB '+VAR+' CORRECTED',position=[0.05,0.55,0.45,0.95]
contour,p_prob_tasc2d_sm(*,*,0),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5

contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='PROB '+VAR+' CORRECTED',position=[0.5,0.55,0.9,0.95]
contour,p_prob_tasc2d_sm(*,*,1),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5

contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='PROB '+VAR+' CORRECTED',position=[0.05,0.05,0.45,0.45]
contour,p_prob_tasc2d_sm(*,*,2),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5

contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='PROB '+VAR+' CORRECTED',position=[0.5,0.05,0.9,0.45]
contour,p_prob_tasc2d_sm(*,*,3),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5
colorbar,levels=lev_c,divisions=N_ELEMENTS(levels)-1,c_colors=colors,charsize=1,color=0,position=[0.96,0.55,0.99,0.95],/vertical

STOP

;PS
LoadCT, 0, /Silent
;PLOTS
IF VAR EQ 'Ptot' THEN BEGIN
MinCol=20
MaxCol=155
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

set_plot,'ps'
PRINT,'Creating Postscriptfile: '
device,file='./PLOTS/NEW_PLOT_SM_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'.ps',/landscape,BITS_PER_PIXEL=8, COLOR=1,DECOMPOSED=0

!p.multi=[0,2,2]
;TAS'
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' DJF',position=[0.05,0.55,0.45,0.95]
contour,p_tas2d_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' MAM',position=[0.5,0.55,0.9,0.95]
contour,p_tas2d_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' JJA',position=[0.05,0.05,0.45,0.45]
contour,p_tas2d_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' SON',position=[0.5,0.05,0.9,0.45]
contour,p_tas2d_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1,color=255,position=[0.96,0.55,0.99,0.95],/vertical,/pscolor

!p.multi=[0,2,2]
;TAS CORR'
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORRECTED DJF',position=[0.05,0.55,0.45,0.95]
contour,p_tasc2d_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORRECTED MAM',position=[0.5,0.55,0.9,0.95]
contour,p_tasc2d_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORRECTED JJA',position=[0.05,0.05,0.45,0.45]
contour,p_tasc2d_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' CORRECTED SON',position=[0.5,0.05,0.9,0.45]
contour,p_tasc2d_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1,color=255,position=[0.96,0.55,0.99,0.95],/vertical,/pscolor

!p.multi=[0,2,2]
;OBS'
contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS DJF',position=[0.05,0.55,0.45,0.95]
contour,p_obs2d_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS MAM',position=[0.5,0.55,0.9,0.95]
contour,p_obs2d_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS JJA',position=[0.05,0.05,0.45,0.45]
contour,p_obs2d_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255

contour,landmask_c,xstyle=1,ystyle=1,c_colors=255,title=VAR+' OBS SON',position=[0.5,0.05,0.9,0.45]
contour,p_obs2d_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,landmask_c,xstyle=1,ystyle=1,/overplot,thick=0.5,color=255
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1,color=255,position=[0.96,0.55,0.99,0.95],/vertical,/pscolor

device,/close
set_plot,'x'

loadct,39
!p.background=255
!p.color=0
!p.multi=[0,2,2]
levels=findgen(11)/10.
colors=reverse(findgen(10)*19+15)
colors=revers(findgen(10)*20+30)

contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='STAT '+VAR,position=[0.05,0.55,0.45,0.95]
contour,stat_tas2d_sm(*,*,0),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5

contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='STAT '+VAR,position=[0.5,0.55,0.9,0.95]
contour,stat_tas2d_sm(*,*,1),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5

contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='STAT '+VAR,position=[0.05,0.05,0.45,0.45]
contour,stat_tas2d_sm(*,*,2),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5

contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='STAT '+VAR,position=[0.5,0.05,0.9,0.45]
contour,stat_tas2d_sm(*,*,3),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5
colorbar,min=0,max=1,divisions=N_ELEMENTS(levels)-1,c_colors=colors,charsize=1,color=0,position=[0.96,0.55,0.99,0.95],/vertical,/pscolor




!p.multi=[0,2,2]
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='STAT '+VAR+' CORRECTED',position=[0.05,0.55,0.45,0.95]
contour,stat_tasc2d_sm(*,*,0),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5

contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='STAT '+VAR,position=[0.5,0.55,0.9,0.95]
contour,stat_tasc2d_sm(*,*,1),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5

contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='STAT '+VAR,position=[0.05,0.05,0.45,0.45]
contour,stat_tasc2d_sm(*,*,2),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5

contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='STAT '+VAR,position=[0.5,0.05,0.9,0.45]
contour,stat_tasc2d_sm(*,*,3),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5
colorbar,min=0,max=1,divisions=N_ELEMENTS(levels)-1,c_colors=colors,charsize=1,color=0,position=[0.96,0.55,0.99,0.95],/vertical,/pscolor

;PROB
levels=[0,1e-6,5e-5,1e-4,5e-4,1e-3,5e-3,1e-2,5e-2,1e-1,5e-1,1]
lev_c=['0','1e-5','5e-5','1e-4','5e-4','1e-3','5e-3','1e-2','5e-2','1e-1','5e-1','1']
colors=findgen(11)*20+30
;colors(0)=255
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='PROB '+VAR,position=[0.05,0.55,0.45,0.95]
contour,prob_tas2d_sm(*,*,0),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5

contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='PROB '+VAR,position=[0.5,0.55,0.9,0.95]
contour,prob_tas2d_sm(*,*,1),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5

contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='PROB '+VAR,position=[0.05,0.05,0.45,0.45]
contour,prob_tas2d_sm(*,*,2),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5

contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='PROB '+VAR,position=[0.5,0.05,0.9,0.45]
contour,prob_tas2d_sm(*,*,3),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5
colorbar,levels=lev_c,divisions=N_ELEMENTS(levels)-1,c_colors=colors,charsize=1,color=0,position=[0.96,0.55,0.99,0.95],/vertical


contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='PROB '+VAR+' CORRECTED',position=[0.05,0.55,0.45,0.95]
contour,prob_tasc2d_sm(*,*,0),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5

contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='PROB '+VAR+' CORRECTED',position=[0.5,0.55,0.9,0.95]
contour,prob_tasc2d_sm(*,*,1),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5

contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='PROB '+VAR+' CORRECTED',position=[0.05,0.05,0.45,0.45]
contour,prob_tasc2d_sm(*,*,2),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5

contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,title='PROB '+VAR+' CORRECTED',position=[0.5,0.05,0.9,0.45]
contour,prob_tasc2d_sm(*,*,3),/overplot,levels=levels,/fill,c_colors=colors,xstyle=1,ystyle=1
contour,landmask_c,xstyle=1,ystyle=1,c_colors=0,/overplot,thick=0.5
colorbar,levels=lev_c,divisions=N_ELEMENTS(levels)-1,c_colors=colors,charsize=1,color=0,position=[0.96,0.55,0.99,0.95],/vertical,/pscolor



STOP
END
