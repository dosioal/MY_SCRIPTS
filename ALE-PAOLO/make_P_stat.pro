;
PRO make_P_stat
;
DEVICE,decomposed=0
;
;
;-----------------------------------------------------------------------------------------------------------

home='/media/disk/POSTPROC/BC_Regional/'

;MODEL_ID='DMI-HIRHAM5'
;CASE_ID='A1B_ECHAM5'

MODEL_ID='ETHZ-CLM'
CASE_ID='SCN_HadCM3Q0'

MODEL_ID='KNMI-RACMO2'
CASE_ID='A1B_ECHAM5-r3'


VAR='Ptot'

CONSTRUCTION_PERIOD_START  =  '1961'
CONSTRUCTION_PERIOD_STOP   =  '1990'

APPLICATION_PERIOD_START   =  '1991'
APPLICATION_PERIOD_STOP    =  '2000'

NYEARS=fix(APPLICATION_PERIOD_STOP)-fix(APPLICATION_PERIOD_START)+1

;OBS_DIR=home+MODEL_ID+'_'+CASE_ID+'_EOBS_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'/OBS_idl/'
;MOD_DIR=home+MODEL_ID+'_'+CASE_ID+'_EOBS_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'/BC_data/'

;IF CONSTRUCTION_PERIOD_START EQ '1961' AND CONSTRUCTION_PERIOD_STOP EQ '1990' THEN BEGIN
;MOD_BC_DIR=home+MODEL_ID+'_'+CASE_ID+'_EOBS_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'/BC_data/'
;MOD_BC_DIR=home+MODEL_ID+'_'+CASE_ID+'_EOBS_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_1961-2100/BC_data/'
;ENDIF ELSE BEGIN
;MOD_BC_DIR=home+MODEL_ID+'_'+CASE_ID+'_EOBS_'+CONSTRUCTION_PERIOD_START+'-'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'-'+APPLICATION_PERIOD_STOP+'/BC_data/'
;ENDELSE

;-----------------------------------------------------------------------------------------------------------

; specify the months to be bias corrected
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

tas2d_m=fltarr(nxm,nym,3*4,310)
tasc2d_m=fltarr(nxm,nym,3*4,310)
obs2d_m=fltarr(nxm,nym,3*4,310)
tas2d_m(*,*,*,*)=!VALUES.F_NAN
tasc2d_m(*,*,*,*)=!VALUES.F_NAN
obs2d_m(*,*,*,*)=!VALUES.F_NAN

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
ofile='DATA/ENS_AVE_MOD_'+VAR+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'  $
;tas2d_ave,nxm,nym,landmask_c,mndays
ELSE $
ofile='DATA/MOD_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;tas2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile

IF MODEL_ID EQ 'ENS_AVE' THEN $
ofile='DATA/ENS_AVE_MOD_'+VAR+'_CORR_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'  $
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

IF MODEL_ID EQ 'ENS_AVE' THEN BEGIN
tas2d_m(*,*,im,0:MNDAYS-1)=tas2d_ave
tasc2d_m(*,*,im,0:MNDAYS-1)=tasc2d_ave
obs2d_m(*,*,im,0:MNDAYS-1)=obs2d_ave
;tas2d_m(*,*,imm,0:MNDAYS-1)=tas2d_ave
;tasc2d_m(*,*,imm,0:MNDAYS-1)=tasc2d_ave
;obs2d_m(*,*,imm,0:MNDAYS-1)=obs2d_ave
ENDIF ELSE BEGIN
;tas2d_m(*,*,imm,0:MNDAYS-1)=tas2d
;tasc2d_m(*,*,imm,0:MNDAYS-1)=tasc2d
;obs2d_m(*,*,imm,0:MNDAYS-1)=obs2d
tas2d_m(*,*,im,0:MNDAYS-1)=tas2d
tasc2d_m(*,*,im,0:MNDAYS-1)=tasc2d
obs2d_m(*,*,im,0:MNDAYS-1)=obs2d
ENDELSE

imm=imm+1

IF imm GT 2 THEN imm=0
ENDFOR  ;END im

ENDFOR

tas_com=fltarr(nxm,nym,12,31,NYEARS)
tasc_com=fltarr(nxm,nym,12,31,NYEARS)
obs_com=fltarr(nxm,nym,12,31,NYEARS)
tas_com(*,*,*,*,*)=!VALUES.F_NAN
tasc_com(*,*,*,*,*)=!VALUES.F_NAN
obs_com(*,*,*,*,*)=!VALUES.F_NAN

FOR iyy=0,NYEARS-1 DO BEGIN &$
FOR im=0,NMONTHS-1 DO BEGIN &$
tas_com(*,*,im,0:NDAYS_M(im)-1,iyy)=reform(tas2d_m(*,*,im,NDAYS_M(im)*iyy:NDAYS_M(im)*(iyy+1)-1)) &$
tasc_com(*,*,im,0:NDAYS_M(im)-1,iyy)=reform(tasc2d_m(*,*,im,NDAYS_M(im)*iyy:NDAYS_M(im)*(iyy+1)-1)) &$
obs_com(*,*,im,0:NDAYS_M(im)-1,iyy)=reform(obs2d_m(*,*,im,NDAYS_M(im)*iyy:NDAYS_M(im)*(iyy+1)-1)) &$
ENDFOR &$
ENDFOR

tas_m=fltarr(nxm,nym,4,31*3,NYEARS)
tas_m(*,*,*,*,*)=!VALUES.F_NAN
tasc_m=fltarr(nxm,nym,4,31*3,NYEARS)
tasc_m(*,*,*,*,*)=!VALUES.F_NAN
obs_m=fltarr(nxm,nym,4,31*3,NYEARS)
obs_m(*,*,*,*,*)=!VALUES.F_NAN

FOR iy=0,NYEARS-1 DO BEGIN &$
FOR im=0,3 DO BEGIN &$
;	tas_m(*,im,0:30,iy)=tas_com(*,im*3,*,iy) &$
;	tas_m(*,im,31:61,iy)=tas_com(*,im*3+1,*,iy) &$
;	tas_m(*,im,62:92,iy)=tas_com(*,im*3+2,*,iy) &$
;print,im*3,NDAYS_M(im*3)-1,NDAYS_M(im*3)+NDAYS_M(im*3+1)-1,NDAYS_M(im*3)+NDAYS_M(im*3+1)+NDAYS_M(im*3+2)-1 &$
tas_m(*,*,im,0:NDAYS_M(im*3)-1,iy)=tas_com(*,*,im*3,0:NDAYS_M(im*3)-1,iy) &$
tas_m(*,*,im,NDAYS_M(im*3):NDAYS_M(im*3)+NDAYS_M(im*3+1)-1,iy)=tas_com(*,*,im*3+1,0:NDAYS_M(im*3+1)-1,iy) &$
tas_m(*,*,im,NDAYS_M(im*3)+NDAYS_M(im*3+1):NDAYS_M(im*3)+NDAYS_M(im*3+1)+NDAYS_M(im*3+2)-1,iy)=tas_com(*,*,im*3+2,0:NDAYS_M(im*3+2)-1,iy) &$
tasc_m(*,*,im,0:NDAYS_M(im*3)-1,iy)=tasc_com(*,*,im*3,0:NDAYS_M(im*3)-1,iy) &$
tasc_m(*,*,im,NDAYS_M(im*3):NDAYS_M(im*3)+NDAYS_M(im*3+1)-1,iy)=tasc_com(*,*,im*3+1,0:NDAYS_M(im*3+1)-1,iy) &$
tasc_m(*,*,im,NDAYS_M(im*3)+NDAYS_M(im*3+1):NDAYS_M(im*3)+NDAYS_M(im*3+1)+NDAYS_M(im*3+2)-1,iy)=tasc_com(*,*,im*3+2,0:NDAYS_M(im*3+2)-1,iy) &$
obs_m(*,*,im,0:NDAYS_M(im*3)-1,iy)=obs_com(*,*,im*3,0:NDAYS_M(im*3)-1,iy) &$
obs_m(*,*,im,NDAYS_M(im*3):NDAYS_M(im*3)+NDAYS_M(im*3+1)-1,iy)=obs_com(*,*,im*3+1,0:NDAYS_M(im*3+1)-1,iy) &$
obs_m(*,*,im,NDAYS_M(im*3)+NDAYS_M(im*3+1):NDAYS_M(im*3)+NDAYS_M(im*3+1)+NDAYS_M(im*3+2)-1,iy)=obs_com(*,*,im*3+2,0:NDAYS_M(im*3+2)-1,iy) &$
ENDFOR &$
ENDFOR 

Index_d_tas=fltarr(NYEARS)
Max_d_tas=fltarr(NYEARS)
Index_d_tasc=fltarr(NYEARS)
Max_d_tasc=fltarr(NYEARS)
Index_d_obs=fltarr(NYEARS)
Max_d_obs=fltarr(NYEARS)
Index_d_tas_m=fltarr(nxm,nym,4)
Max_d_tas_m=fltarr(nxm,nym,4)
Index_d_tasc_m=fltarr(nxm,nym,4)
Max_d_tasc_m=fltarr(nxm,nym,4)
Index_d_obs_m=fltarr(nxm,nym,4)
Max_d_obs_m=fltarr(nxm,nym,4)
Index_d_tas_m(*,*,*)=!VALUES.F_NAN
Index_d_tasc_m(*,*,*)=!VALUES.F_NAN
Index_d_obs_m(*,*,*)=!VALUES.F_NAN

hp=fltarr(NYEARS)
hp_tas_m=fltarr(nxm,nym,4)
hpc=fltarr(NYEARS)
hp_tasc_m=fltarr(nxm,nym,4)
hpo=fltarr(NYEARS)
hp_obs_m=fltarr(nxm,nym,4)


FOR ix=0,nxm-1 DO BEGIN &$
FOR iy=0,nym-1 DO BEGIN &$
IF land2d_m(ix,iy) EQ 1 THEN BEGIN
FOR im=0,3 DO BEGIN &$
FOR iyy=0,NYEARS-1 DO BEGIN &$

hp(iyy)=0. &$
hpc(iyy)=0. &$
hpo(iyy)=0. &$
th=WHERE(tas_m(ix,iy,im,*,iyy) GT 2.*MEAN(tas_m(ix,iy,im,*,*),/NaN),counth) &$
hpp=fltarr(NDAYS_S(im)) &$
hpp(*)=0. &$
FOR ii=0,counth-2 DO BEGIN &$                                               
IF th(ii+1)-th(ii) EQ 1 THEN  BEGIN &$
;	print,ii,th(ii),tas_m(i,im,th(ii),iy), hp(iy)+tas_m(i,im,th(ii),iy)&$
	hpp(th(ii))=hp(iyy)+tas_m(ix,iy,im,th(ii),iyy) &$
	hp(iyy)=hp(iyy)+tas_m(ix,iy,im,th(ii),iyy) &$
	IF ii LT counth-2 THEN  BEGIN &$
         IF th(ii+2)-th(ii+1) GT 1 THEN  BEGIN &$
;	 print,ii,th(ii+1), tas_m(i,im,th(ii+1),iy),hp(iy)+tas_m(i,im,th(ii+1),iy)&$
         hpp(th(ii+1))=hp(iyy)+tas_m(ix,iy,im,th(ii+1),iyy) &$
       ;  hp(iy)=hp(iy)+tas_m(i,im,th(ii+1),iy) &$
	 hp(iyy)=0. &$
;	 print,hpp(ii) &$
         ENDIF &$
        ENDIF ELSE hpp(ii)=hp(iyy)+tas_m(ix,iy,im,th(counth-1),iyy) &$
ENDIF &$
ENDFOR  &$   ;END ii LOOP
hp(iyy)=MEAN(hpp) &$

th=WHERE(tasc_m(ix,iy,im,*,iyy) GT 2.*MEAN(tasc_m(ix,iy,im,*,*),/NaN),counth) &$
hpp=fltarr(NDAYS_S(im)) &$
hpp(*)=0. &$
FOR ii=0,counth-2 DO BEGIN &$                                               
IF th(ii+1)-th(ii) EQ 1 THEN  BEGIN &$
;	print,ii,th(ii),tas_m(i,im,th(ii),iy), hp(iy)+tas_m(i,im,th(ii),iy)&$
	hpp(th(ii))=hpc(iyy)+tasc_m(ix,iy,im,th(ii),iyy) &$
	hpc(iyy)=hpc(iyy)+tasc_m(ix,iy,im,th(ii),iyy) &$
	IF ii LT counth-2 THEN  BEGIN &$
         IF th(ii+2)-th(ii+1) GT 1 THEN  BEGIN &$
;	 print,ii,th(ii+1), tas_m(i,im,th(ii+1),iy),hp(iy)+tas_m(i,im,th(ii+1),iy)&$
         hpp(th(ii+1))=hpc(iyy)+tasc_m(ix,iy,im,th(ii+1),iyy) &$
       ;  hpc(iy)=hpc(iy)+tasc_m(i,im,th(ii+1),iy) &$
	 hpc(iy)=0. &$
;	 print,hpp(ii) &$
         ENDIF &$
        ENDIF ELSE hpp(ii)=hpc(iyy)+tasc_m(ix,iy,im,th(counth-1),iyy) &$
ENDIF &$
ENDFOR  &$   ;END ii LOOP
hpc(iyy)=MEAN(hpp) &$

th=WHERE(obs_m(ix,iy,im,*,iyy) GT 2.*MEAN(obs_m(ix,iy,im,*,*),/NaN),counth) &$
hpp=fltarr(NDAYS_S(im)) &$
hpp(*)=0. &$
FOR ii=0,counth-2 DO BEGIN &$                                               
IF th(ii+1)-th(ii) EQ 1 THEN  BEGIN &$
;	print,ii,th(ii),tas_m(i,im,th(ii),iy), hp(iy)+tas_m(i,im,th(ii),iy)&$
	hpp(th(ii))=hpo(iyy)+obs_m(ix,iy,im,th(ii),iyy) &$
	hpo(iyy)=hpo(iyy)+obs_m(ix,iy,im,th(ii),iyy) &$
	IF ii LT counth-2 THEN  BEGIN &$
         IF th(ii+2)-th(ii+1) GT 1 THEN  BEGIN &$
;	 print,ii,th(ii+1), tas_m(i,im,th(ii+1),iy),hp(iy)+tas_m(i,im,th(ii+1),iy)&$
         hpp(th(ii+1))=hpo(iyy)+obs_m(ix,iy,im,th(ii+1),iyy) &$
;         hpo(iy)=hpo(iy)+obs_m(i,im,th(ii+1),iy) &$
	 hpo(iyy)=0. &$
;	 print,hpp(ii) &$
         ENDIF &$
        ENDIF ELSE hpp(ii)=hpo(iyy)+obs_m(ix,iy,im,th(counth-1),iyy) &$
ENDIF &$
ENDFOR  &$   ;END ii LOOP
hpo(iyy)=MEAN(hpp) &$

td=WHERE(tas_m(ix,iy,im,*,iyy) GT 0.01,count) &$
tdc=WHERE(tasc_m(ix,iy,im,*,iyy) GT 0.01,countc) &$
tdo=WHERE(obs_m(ix,iy,im,*,iyy) GT 0.01,counto) &$

IF count GT 1 THEN BEGIN &$
ww=fltarr(count+1) &$
ww(0)=td(0)-1 &$
FOR ii=1,count-1 do begin &$
	ww(ii)=td(ii)-td(ii-1)-1 &$
ENDFOR &$
ww(count)=NDAYS_S(im)-td(count-1)-1 &$
Index_d_tas(iyy)=MEAN(ww) &$
Max_d_tas(iyy)=MAX(ww) &$
ENDIF ELSE IF count EQ 0 THEN BEGIN &$
Index_d_tas(iyy)=N_ELEMENTS(tas_m(ix,iy,im,*,iyy)) &$
Max_d_tas(iyy)=N_ELEMENTS(tas_m(ix,iy,im,*,iyy)) &$
ENDIF ELSE BEGIN &$
Index_d_tas(iyy)=td &$
Max_d_tas(iyy)=td &$
ENDELSE &$

IF countc GT 1 THEN BEGIN &$
ww=fltarr(countc+1) &$
ww(0)=tdc(0)-1 &$
FOR ii=1,countc-1 do begin &$
	ww(ii)=tdc(ii)-tdc(ii-1)-1 &$
ENDFOR &$
ww(countc)=NDAYS_S(im)-tdc(countc-1)-1 &$
Index_d_tasc(iy)=MEAN(ww) &$
Max_d_tasc(iy)=MAX(ww) &$
ENDIF ELSE IF countc EQ 0 THEN BEGIN &$
Index_d_tasc(iy)=N_ELEMENTS(tasc_m(i,im,*,iy)) &$
Max_d_tasc(iy)=N_ELEMENTS(tasc_m(i,im,*,iy)) &$
ENDIF ELSE BEGIN &$
Index_d_tasc(iy)=tdc &$
Max_d_tasc(iy)=tdc &$
ENDELSE &$

IF counto GT 1 THEN BEGIN &$
ww=fltarr(counto+1) &$
ww(0)=tdo(0)-1 &$
FOR ii=1,counto-1 do begin &$
	ww(ii)=tdo(ii)-tdo(ii-1)-1 &$
ENDFOR &$
ww(counto)=NDAYS_S(im)-tdo(counto-1)-1 &$
Index_d_obs(iyy)=MEAN(ww) &$
Max_d_obs(iyy)=MAX(ww) &$
ENDIF ELSE IF counto EQ 0 THEN BEGIN &$
Index_d_obs(iyy)=N_ELEMENTS(obs_m(ix,iy,im,*,iyy)) &$
Max_d_obs(iyy)=N_ELEMENTS(obs_m(ix,iy,im,*,iyy)) &$
ENDIF ELSE BEGIN &$
Index_d_obs(iyy)=tdo &$
Max_d_obs(iyy)=tdo &$
ENDELSE &$

ENDFOR &$   ;END YEAR LOOP iy

Index_d_tas_m(ix,iy,im)=ROUND(MEAN(Index_d_tas)) &$
Index_d_tasc_m(ix,iy,im)=ROUND(MEAN(Index_d_tasc)) &$
Index_d_obs_m(ix,iy,im)=ROUND(MEAN(Index_d_obs)) &$
Max_d_tas_m(ix,iy,im)=ROUND(MEAN(Max_d_tas)) &$
Max_d_tasc_m(ix,iy,im)=ROUND(MEAN(Max_d_tasc)) &$
Max_d_obs_m(ix,iy,im)=ROUND(MEAN(Max_d_obs)) &$

hp_tas_m(ix,iy,im)=ROUND(MEAN(hp)) &$
hp_tasc_m(ix,iy,im)=ROUND(MEAN(hpc)) &$
hp_obs_m(ix,iy,im)=ROUND(MEAN(hpo)) &$
ENDFOR &$ ;end im

ENDIF
ENDFOR &$ ;end iy
ENDFOR ; end ix

;SAVING DATA
IF MODEL_ID EQ 'ENS_AVE' THEN BEGIN
ofile='DATA/DROUGHT_'+VAR+'_SM_ENSE_AVE_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'.dat'
save,filename=ofile,Index_d_tas_m,Max_d_tas_m,hp_tas_m,nxm,nym,mnx0,mny0,landmask_c
ofile='DATA/DROUGHT_'+VAR+'_SM_CORR_ENS_AVE_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'.dat'
save,filename=ofile,Index_d_tasc_m,Max_d_tasc_m,hp_tasc_m,nxm,nym,mnx0,mny0,landmask_c
ofile='DATA/DROUGHT_OBS_SM_ENS_AVE_'+CONSTRUUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'.dat'
save,filename=ofile,Index_d_obs_m,Max_d_obs_m,hp_obs_m,nxm,nym,mnx0,mny0,landmask_c
ENDIF ELSE BEGIN
ofile='DATA/DROUGHT_'+VAR+'_SM_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'.dat'
save,filename=ofile,Index_d_tas_m,Max_d_tas_m,hp_tas_m,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c
ofile='DATA/DROUGHT_'+VAR+'_SM_CORR_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'.dat'
save,filename=ofile,Index_d_tasc_m,Max_d_tasc_m,hp_tasc_m,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c
ofile='DATA/DROUGHT_OBS_SM_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'.dat'
save,filename=ofile,Index_d_obs_m,Max_d_obs_m,hp_obs_m,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c
ENDELSE

;PLOTS

Id2d_tas_sm = FLTARR(nxm,nym,4)
Id2d_tasc_sm = FLTARR(nxm,nym,4)
Id2d_obs_sm = FLTARR(nxm,nym,4)
Im2d_tas_sm = FLTARR(nxm,nym,4)
Im2d_tasc_sm = FLTARR(nxm,nym,4)
Im2d_obs_sm = FLTARR(nxm,nym,4)
hp2d_tas_sm = FLTARR(nxm,nym,4)
hp2d_tasc_sm = FLTARR(nxm,nym,4)
hp2d_obs_sm = FLTARR(nxm,nym,4)
Id2d_tas_sm(*,*,*)=-999.
Id2d_tasc_sm(*,*,*)=-999.
Id2d_obs_sm(*,*,*)=-999.
Im2d_tas_sm(*,*,*)=-999.
Im2d_tasc_sm(*,*,*)=-999.
Im2d_obs_sm(*,*,*)=-999.
hp2d_tas_sm(*,*,*)=-999.
hp2d_tasc_sm(*,*,*)=-999.
hp2d_obs_sm(*,*,*)=-999.
FOR j=0,3 DO BEGIN &$
FOR ix=0L,nxm-1 DO BEGIN &$
FOR iy=0L,nym-1 DO BEGIN &$
IF landmask_c(ix,iy) EQ 1 AND (Index_d_tas_m(ix,iy,j) GE 0) THEN BEGIN &$
         Id2d_tas_sm(ix,iy,j)=Index_d_tas_m(ix,iy,j) &$
         Id2d_tasc_sm(ix,iy,j)=Index_d_tasc_m(ix,iy,j) &$
         Id2d_obs_sm(ix,iy,j)=Index_d_obs_m(ix,iy,j) &$
         Im2d_tas_sm(ix,iy,j)=Max_d_tas_m(ix,iy,j) &$
         Im2d_tasc_sm(ix,iy,j)=Max_d_tasc_m(ix,iy,j) &$
         Im2d_obs_sm(ix,iy,j)=Max_d_obs_m(ix,iy,j) &$
         hp2d_tas_sm(ix,iy,j)=hp_tas_m(ix,iy,j) &$
         hp2d_tasc_sm(ix,iy,j)=hp_tasc_m(ix,iy,j) &$
         hp2d_obs_sm(ix,iy,j)=hp_obs_m(ix,iy,j) &$
ENDIF
ENDFOR &$
ENDFOR &$
ENDFOR


;PLOTS
i=9000 
MinCol=20
MaxCol=254
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
rgrContValues(N_ELEMENTS(rgrContValues)-1)=100
rgrColValues=[0,25,60,80,100,120,180,190,200,250]
;ColorTBL=62
;SetColorTable,ColorTBL
!p.background=0
!p.color=255
loadct,39
!p.color=0
!p.background=255

window,0,retain=2
!p.multi=[0,2,2]
;TAS'
contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' DJF',position=[0.05,0.55,0.45,0.95]
contour,Id2d_tas_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1.5,color=25,position=[0.96,0.55,0.99,0.95],/vertical

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' MAM',position=[0.5,0.55,0.9,0.95]
contour,Id2d_tas_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' JJA',position=[0.05,0.05,0.45,0.45]
contour,Id2d_tas_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' SON',position=[0.5,0.05,0.9,0.45]
contour,Id2d_tas_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

window,1,retain=2
!p.multi=[0,2,2]
;TAS CORR'
contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' CORRECTED DJF',position=[0.05,0.55,0.45,0.95]
contour,Id2d_tasc_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1.5,color=25,position=[0.96,0.55,0.99,0.95],/vertical

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' CORRECTED MAM',position=[0.5,0.55,0.9,0.95]
contour,Id2d_tasc_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' CORRECTED JJA',position=[0.05,0.05,0.45,0.45]
contour,Id2d_tasc_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' CORRECTED SON',position=[0.5,0.05,0.9,0.45]
contour,Id2d_tasc_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

window,2,retain=2
!p.multi=[0,2,2]
;OBS'
contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' OBS DJF',position=[0.05,0.55,0.45,0.95]
contour,Id2d_obs_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1.5,color=25,position=[0.96,0.55,0.99,0.95],/vertical

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' OBS MAM',position=[0.5,0.55,0.9,0.95]
contour,Id2d_obs_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' OBS JJA',position=[0.05,0.05,0.45,0.45]
contour,Id2d_obs_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' OBS SON',position=[0.5,0.05,0.9,0.45]
contour,Id2d_obs_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

Minvalue=0
Maxvalue=20
ContourDist=2 
rInterval=Maxvalue-Minvalue
iNumOfConts=FIX(rInterval/ContourDist)+1
rgrContValues = INDGEN(iNumOfConts)*ContourDist + MinValue
rgrContValues(N_ELEMENTS(rgrContValues)-1)=100
window,3,retain=2
!p.multi=[0,2,2]
;TAS'
contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' DJF',position=[0.05,0.55,0.45,0.95]
contour,Im2d_tas_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1.5,color=25,position=[0.96,0.55,0.99,0.95],/vertical

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' MAM',position=[0.5,0.55,0.9,0.95]
contour,Im2d_tas_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' JJA',position=[0.05,0.05,0.45,0.45]
contour,Im2d_tas_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' SON',position=[0.5,0.05,0.9,0.45]
contour,Im2d_tas_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

window,4,retain=2
!p.multi=[0,2,2]
;TAS CORR'
contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' CORRECTED DJF',position=[0.05,0.55,0.45,0.95]
contour,Im2d_tasc_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1.5,color=25,position=[0.96,0.55,0.99,0.95],/vertical

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' CORRECTED MAM',position=[0.5,0.55,0.9,0.95]
contour,Im2d_tasc_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' CORRECTED JJA',position=[0.05,0.05,0.45,0.45]
contour,Im2d_tasc_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' CORRECTED SON',position=[0.5,0.05,0.9,0.45]
contour,Im2d_tasc_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

window,5,retain=2
!p.multi=[0,2,2]
;OBS'
contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' OBS DJF',position=[0.05,0.55,0.45,0.95]
contour,Im2d_obs_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1.5,color=25,position=[0.96,0.55,0.99,0.95],/vertical

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' OBS MAM',position=[0.5,0.55,0.9,0.95]
contour,Im2d_obs_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' OBS JJA',position=[0.05,0.05,0.45,0.45]
contour,Im2d_obs_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' OBS SON',position=[0.5,0.05,0.9,0.45]
contour,Im2d_obs_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

Minvalue=0
Maxvalue=10
ContourDist=1
rInterval=Maxvalue-Minvalue
iNumOfConts=FIX(rInterval/ContourDist)+1
rgrContValues = INDGEN(iNumOfConts)*ContourDist + MinValue
rgrContValues(N_ELEMENTS(rgrContValues)-1)=100
window,6,retain=2
!p.multi=[0,2,2]
;TAS'
contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' DJF',position=[0.05,0.55,0.45,0.95]
contour,hp2d_tas_sm(*,*,0),levels=rgrContValues,/fill,c_colors=REVERSE(rgrColValues),xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0
colorbar,min=minvalue,max=maxvalue,c_colors=REVERSE(rgrColValues),divisions=iNumOfConts-1,charsize=1.5,color=25,position=[0.96,0.55,0.99,0.95],/vertical

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' MAM',position=[0.5,0.55,0.9,0.95]
contour,hp2d_tas_sm(*,*,1),levels=rgrContValues,/fill,c_colors=REVERSE(rgrColValues),xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' JJA',position=[0.05,0.05,0.45,0.45]
contour,hp2d_tas_sm(*,*,2),levels=rgrContValues,/fill,c_colors=REVERSE(rgrColValues),xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' SON',position=[0.5,0.05,0.9,0.45]
contour,hp2d_tas_sm(*,*,3),levels=rgrContValues,/fill,c_colors=REVERSE(rgrColValues),xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

window,7,retain=2
!p.multi=[0,2,2]
;TAS'
contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' DJF',position=[0.05,0.55,0.45,0.95]
contour,hp2d_tasc_sm(*,*,0),levels=rgrContValues,/fill,c_colors=REVERSE(rgrColValues),xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0
colorbar,min=minvalue,max=maxvalue,c_colors=REVERSE(rgrColValues),divisions=iNumOfConts-1,charsize=1.5,color=25,position=[0.96,0.55,0.99,0.95],/vertical

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' MAM',position=[0.5,0.55,0.9,0.95]
contour,hp2d_tasc_sm(*,*,1),levels=rgrContValues,/fill,c_colors=REVERSE(rgrColValues),xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' JJA',position=[0.05,0.05,0.45,0.45]
contour,hp2d_tasc_sm(*,*,2),levels=rgrContValues,/fill,c_colors=REVERSE(rgrColValues),xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' SON',position=[0.5,0.05,0.9,0.45]
contour,hp2d_tasc_sm(*,*,3),levels=rgrContValues,/fill,c_colors=REVERSE(rgrColValues),xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

window,8,retain=2
!p.multi=[0,2,2]
;TAS'
contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' DJF',position=[0.05,0.55,0.45,0.95]
contour,hp2d_obs_sm(*,*,0),levels=rgrContValues,/fill,c_colors=REVERSE(rgrColValues),xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0
colorbar,min=minvalue,max=maxvalue,c_colors=REVERSE(rgrColValues),divisions=iNumOfConts-1,charsize=1.5,color=25,position=[0.96,0.55,0.99,0.95],/vertical

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' MAM',position=[0.5,0.55,0.9,0.95]
contour,hp2d_obs_sm(*,*,1),levels=rgrContValues,/fill,c_colors=REVERSE(rgrColValues),xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' JJA',position=[0.05,0.05,0.45,0.45]
contour,hp2d_obs_sm(*,*,2),levels=rgrContValues,/fill,c_colors=REVERSE(rgrColValues),xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=VAR+' SON',position=[0.5,0.05,0.9,0.45]
contour,hp2d_obs_sm(*,*,3),levels=rgrContValues,/fill,c_colors=REVERSE(rgrColValues),xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0


;PS PLOTS
LoadCT, 0, /Silent

i=9000 
MinCol=20
MaxCol=254
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
rgrContValues(N_ELEMENTS(rgrContValues)-1)=100
rgrColValues=[0,25,60,80,100,120,180,190,200,250]
;ColorTBL=62
;SetColorTable,ColorTBL
!p.background=0
!p.color=255
loadct,39
!p.color=0
!p.background=255

set_plot,'ps'
PRINT,'Creating Postscriptfile: '
device,file='./PLOTS/PLOT_SM_PREP_INDEX_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'.ps',/landscape,BITS_PER_PIXEL=8, COLOR=1,DECOMPOSED=0

!p.multi=[0,2,2]
;TAS'
contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title='DROUGHT INDEX DJF',position=[0.05,0.55,0.45,0.95]
contour,Id2d_tas_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title='MAM',position=[0.5,0.55,0.9,0.95]
contour,Id2d_tas_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title='JJA',position=[0.05,0.05,0.45,0.45]
contour,Id2d_tas_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title='SON',position=[0.5,0.05,0.9,0.45]
contour,Id2d_tas_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1.5,color=25,position=[0.96,0.55,0.99,0.95],/vertical

!p.multi=[0,2,2]
;TAS CORR'
contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title='DROUGHT INDEX CORRECTED DJF',position=[0.05,0.55,0.45,0.95]
contour,Id2d_tasc_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=' CORRECTED MAM',position=[0.5,0.55,0.9,0.95]
contour,Id2d_tasc_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=' CORRECTED JJA',position=[0.05,0.05,0.45,0.45]
contour,Id2d_tasc_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=' CORRECTED SON',position=[0.5,0.05,0.9,0.45]
contour,Id2d_tasc_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1.5,color=25,position=[0.96,0.55,0.99,0.95],/vertical

!p.multi=[0,2,2]
;OBS'
contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title='DROUGHT INDEX OBS DJF',position=[0.05,0.55,0.45,0.95]
contour,Id2d_obs_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=' OBS MAM',position=[0.5,0.55,0.9,0.95]
contour,Id2d_obs_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=' OBS JJA',position=[0.05,0.05,0.45,0.45]
contour,Id2d_obs_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=' OBS SON',position=[0.5,0.05,0.9,0.45]
contour,Id2d_obs_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1.5,color=25,position=[0.96,0.55,0.99,0.95],/vertical

Minvalue=0
Maxvalue=20
ContourDist=2 
rInterval=Maxvalue-Minvalue
iNumOfConts=FIX(rInterval/ContourDist)+1
rgrContValues = INDGEN(iNumOfConts)*ContourDist + MinValue
rgrContValues(N_ELEMENTS(rgrContValues)-1)=100
!p.multi=[0,2,2]
;TAS'
contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title='MAX DROUGHT DJF',position=[0.05,0.55,0.45,0.95]
contour,Im2d_tas_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=' MAM',position=[0.5,0.55,0.9,0.95]
contour,Im2d_tas_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=' JJA',position=[0.05,0.05,0.45,0.45]
contour,Im2d_tas_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=' SON',position=[0.5,0.05,0.9,0.45]
contour,Im2d_tas_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1.5,color=25,position=[0.96,0.55,0.99,0.95],/vertical

!p.multi=[0,2,2]
;TAS CORR'
contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title='MAX DROUGHT CORRECTED DJF',position=[0.05,0.55,0.45,0.95]
contour,Im2d_tasc_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=' CORRECTED MAM',position=[0.5,0.55,0.9,0.95]
contour,Im2d_tasc_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=' CORRECTED JJA',position=[0.05,0.05,0.45,0.45]
contour,Im2d_tasc_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=' CORRECTED SON',position=[0.5,0.05,0.9,0.45]
contour,Im2d_tasc_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1.5,color=25,position=[0.96,0.55,0.99,0.95],/vertical

!p.multi=[0,2,2]
;OBS'
contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title='MAX DROUGHT OBS DJF',position=[0.05,0.55,0.45,0.95]
contour,Im2d_obs_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=' OBS MAM',position=[0.5,0.55,0.9,0.95]
contour,Im2d_obs_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=' OBS JJA',position=[0.05,0.05,0.45,0.45]
contour,Im2d_obs_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=' OBS SON',position=[0.5,0.05,0.9,0.45]
contour,Im2d_obs_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1.5,color=25,position=[0.96,0.55,0.99,0.95],/vertical

Minvalue=0
Maxvalue=10
ContourDist=1
rInterval=Maxvalue-Minvalue
iNumOfConts=FIX(rInterval/ContourDist)+1
rgrContValues = INDGEN(iNumOfConts)*ContourDist + MinValue
rgrContValues(N_ELEMENTS(rgrContValues)-1)=100
!p.multi=[0,2,2]
;TAS'
contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title='HEAVY PREC DJF',position=[0.05,0.55,0.45,0.95]
contour,hp2d_tas_sm(*,*,0),levels=rgrContValues,/fill,c_colors=REVERSE(rgrColValues),xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=' MAM',position=[0.5,0.55,0.9,0.95]
contour,hp2d_tas_sm(*,*,1),levels=rgrContValues,/fill,c_colors=REVERSE(rgrColValues),xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=' JJA',position=[0.05,0.05,0.45,0.45]
contour,hp2d_tas_sm(*,*,2),levels=rgrContValues,/fill,c_colors=REVERSE(rgrColValues),xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=' SON',position=[0.5,0.05,0.9,0.45]
contour,hp2d_tas_sm(*,*,3),levels=rgrContValues,/fill,c_colors=REVERSE(rgrColValues),xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0
colorbar,min=minvalue,max=maxvalue,c_colors=REVERSE(rgrColValues),divisions=iNumOfConts-1,charsize=1.5,color=25,position=[0.96,0.55,0.99,0.95],/vertical

!p.multi=[0,2,2]
;TAS'
contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title='HEAVY PREC CORRECTED DJF',position=[0.05,0.55,0.45,0.95]
contour,hp2d_tasc_sm(*,*,0),levels=rgrContValues,/fill,c_colors=REVERSE(rgrColValues),xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=' MAM',position=[0.5,0.55,0.9,0.95]
contour,hp2d_tasc_sm(*,*,1),levels=rgrContValues,/fill,c_colors=REVERSE(rgrColValues),xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=' JJA',position=[0.05,0.05,0.45,0.45]
contour,hp2d_tasc_sm(*,*,2),levels=rgrContValues,/fill,c_colors=REVERSE(rgrColValues),xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=' SON',position=[0.5,0.05,0.9,0.45]
contour,hp2d_tasc_sm(*,*,3),levels=rgrContValues,/fill,c_colors=REVERSE(rgrColValues),xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0
colorbar,min=minvalue,max=maxvalue,c_colors=REVERSE(rgrColValues),divisions=iNumOfConts-1,charsize=1.5,color=25,position=[0.96,0.55,0.99,0.95],/vertical

!p.multi=[0,2,2]
;TAS'
contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title='HEAVY PREC OBS DJF',position=[0.05,0.55,0.45,0.95]
contour,hp2d_obs_sm(*,*,0),levels=rgrContValues,/fill,c_colors=REVERSE(rgrColValues),xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=' MAM',position=[0.5,0.55,0.9,0.95]
contour,hp2d_obs_sm(*,*,1),levels=rgrContValues,/fill,c_colors=REVERSE(rgrColValues),xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=' JJA',position=[0.05,0.05,0.45,0.45]
contour,hp2d_obs_sm(*,*,2),levels=rgrContValues,/fill,c_colors=REVERSE(rgrColValues),xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title=' SON',position=[0.5,0.05,0.9,0.45]
contour,hp2d_obs_sm(*,*,3),levels=rgrContValues,/fill,c_colors=REVERSE(rgrColValues),xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0
colorbar,min=minvalue,max=maxvalue,c_colors=REVERSE(rgrColValues),divisions=iNumOfConts-1,charsize=1.5,color=25,position=[0.96,0.55,0.99,0.95],/vertical

device,/close
set_plot,'x'

STOP
END
