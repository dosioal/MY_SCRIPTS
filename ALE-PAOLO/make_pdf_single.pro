;
PRO make_pdf_single
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
VAR='T'

CONSTRUCTION_PERIOD_START  =  '1961'
CONSTRUCTION_PERIOD_STOP   =  '1990'

APPLICATION_PERIOD_START   =  '1991'
APPLICATION_PERIOD_STOP    =  '2000'

NYEARS=fix(APPLICATION_PERIOD_STOP)-fix(APPLICATION_PERIOD_START)+1

;ns=4
;season=['DJF','MAM','JJA','SON']

month=['12','01','02','03','04','05','06','07','08','09','10','11']
NMONTHS=12
NDAYS=fltarr(12)
NDAYS_M=[31,31,28,31,30,31,30,31,31,30,31,30]

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

binsz=1
mint=230.
maxt=320.
nbins=(maxt-mint)/binsz+1
;hist = HISTOGRAM(data)
;bins = FINDGEN(N_ELEMENTS(hist)) + MIN(data)
bins = FINDGEN(nbins) + mint

pdf_tas2d_sm_m=FLTARR(nm,4,nbins)
pdf_tasc2d_sm_m=FLTARR(nm,4,nbins)
pdf_obs2d_sm_m=FLTARR(nm,4,nbins)
pdf_tas2d_sm_m(*,*,*)=!VALUES.F_NAN
pdf_tasc2d_sm_m(*,*,*)=!VALUES.F_NAN
pdf_obs2d_sm_m(*,*,*)=!VALUES.F_NAN

print,'OPENING MONTH ',month(0),' MODEL ',MODEL_ID+'_'+CASE_ID
IF MODEL_ID EQ 'ENS_AVE' THEN BEGIN
ofile='DATA/ENS_AVE_MOD_'+VAR+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(0)+'.dat'
;tas2d_ave,nxm,nym,landmask_c,mndays
print,'OPENING',ofile
restore,filename=ofile
;DELVARX,tas2d_ave,mndays,/FREE_MEM
ENDIF ELSE BEGIN
ofile='DATA/MOD_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(0)+'.dat'
;tas2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'OPENING',ofile
restore,filename=ofile
nxm=mnx
nym=mny
;DELVARX,tas2d,mx0,my0,mndays,/FREE_MEM
ENDELSE

;AVERAGES OVER MASKS
FOR immm=0,nm-1 DO BEGIN ; LOOP OVER MASKS
;immm=0

print,'MASK: ',mask(immm)
ma=fltarr(4)
IF mask(immm) eq 'BI' then ma=BI
IF mask(immm) eq 'IP' then ma=IP
IF mask(immm) eq 'FR' then ma=FR
IF mask(immm) eq 'ME' then ma=ME
IF mask(immm) eq 'SC' then ma=SC
IF mask(immm) eq 'AL' then ma=AL
IF mask(immm) eq 'MD' then ma=MD
IF mask(immm) eq 'EA' then ma=EA


;-----------------------------------------------------------------------------------------------------------

tas2d_m=fltarr(ma(1)-ma(0)+1,ma(3)-ma(2)+1,3*4,310)
tasc2d_m=fltarr(ma(1)-ma(0)+1,ma(3)-ma(2)+1,3*4,310)
obs2d_m=fltarr(ma(1)-ma(0)+1,ma(3)-ma(2)+1,3*4,310)
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

land2d_m=REFORM(landmask_c(ma(0):ma(1),ma(2):ma(3)))

IF MODEL_ID EQ 'ENS_AVE' THEN BEGIN
tas2d_m(*,*,im,0:MNDAYS-1)=REFORM(tas2d_ave(ma(0):ma(1),ma(2):ma(3),*))
tasc2d_m(*,*,im,0:MNDAYS-1)=REFORM(tasc2d_ave(ma(0):ma(1),ma(2):ma(3),*))
obs2d_m(*,*,im,0:MNDAYS-1)=REFORM(obs2d_ave(ma(0):ma(1),ma(2):ma(3),*))
ENDIF ELSE BEGIN
tas2d_m(*,*,im,0:MNDAYS-1)=REFORM(tas2d(ma(0):ma(1),ma(2):ma(3),*))
tasc2d_m(*,*,im,0:MNDAYS-1)=REFORM(tasc2d(ma(0):ma(1),ma(2):ma(3),*))
obs2d_m(*,*,im,0:MNDAYS-1)=REFORM(obs2d(ma(0):ma(1),ma(2):ma(3),*))
print,tas2d(25+ma(0),5+ma(2),0),tas2d_m(25,5,im,0)
ENDELSE

imm=imm+1

IF imm GT 2 THEN imm=0
ENDFOR  ;END im

ENDFOR ;END imm
;STOP

nxm=ma(1)-ma(0)+1
nym=ma(3)-ma(2)+1

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
;DELVARX,tas2d_m,tasc2d_m,obs2d_m,/FREE_MEM

tas_m=fltarr(nxm,nym,4,31*3,NYEARS)
tas_m(*,*,*,*,*)=!VALUES.F_NAN
tasc_m=fltarr(nxm,nym,4,31*3,NYEARS)
tasc_m(*,*,*,*,*)=!VALUES.F_NAN
obs_m=fltarr(nxm,nym,4,31*3,NYEARS)
obs_m(*,*,*,*,*)=!VALUES.F_NAN

FOR iy=0,NYEARS-1 DO BEGIN &$
FOR im=0,3 DO BEGIN &$
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

;DELVARX,tas_com,tasc_com,obs_com,/FREE_MEM

print,'CALCULATING PDF'

pdf_tas2d_sm = FLTARR(nxm,nym,4,nbins)
pdf_tasc2d_sm = FLTARR(nxm,nym,4,nbins)
pdf_obs2d_sm = FLTARR(nxm,nym,4,nbins)
pdf_tas2d_sm(*,*,*,*)=!VALUES.F_NAN
pdf_tasc2d_sm(*,*,*,*)=!VALUES.F_NAN
pdf_obs2d_sm(*,*,*,*)=!VALUES.F_NAN

FOR im=0,3 DO BEGIN &$; SEASON LOOP
FOR ix=0,nxm-1 DO BEGIN &$
FOR iy=0,nym-1 DO BEGIN &$
IF land2d_m(ix,iy) EQ 1 THEN BEGIN &$

pwork=FLTARR(nbins,nyears) &$
FOR iyy=0,NYEARS-1 DO BEGIN  &$ ;SEASON LOOP
pwork(*,iyy)=HISTOGRAM(REFORM(tas_m(ix,iy,im,*,iyy)),min=mint,max=maxt,binsize=binsz,/NaN) &$
ENDFOR &$
FOR ib=0,nbins-1 DO BEGIN &$
pdf_tas2d_sm(ix,iy,im,ib)=MEAN(pwork(ib,*),/NaN) &$
ENDFOR &$

pwork=FLTARR(nbins,nyears) &$
FOR iyy=0,NYEARS-1 DO BEGIN  &$ ;SEASON LOOP
pwork(*,iyy)=HISTOGRAM(REFORM(tasc_m(ix,iy,im,*,iyy)),min=mint,max=maxt,binsize=binsz,/NaN) &$
ENDFOR &$
FOR ib=0,nbins-1 DO BEGIN &$
pdf_tasc2d_sm(ix,iy,im,ib)=MEAN(pwork(ib,*),/NaN) &$
ENDFOR &$

pwork=FLTARR(nbins,nyears) &$
FOR iyy=0,NYEARS-1 DO BEGIN  &$; SEASON LOOP
pwork(*,iyy)=HISTOGRAM(REFORM(obs_m(ix,iy,im,*,iyy)),min=mint,max=maxt,binsize=binsz,/NaN) &$
ENDFOR &$
FOR ib=0,nbins-1 DO BEGIN &$
pdf_obs2d_sm(ix,iy,im,ib)=MEAN(pwork(ib,*),/NaN) &$
ENDFOR &$

ENDIF &$
ENDFOR &$
ENDFOR &$
ENDFOR


FOR is=0,3 DO BEGIN &$; SEASON LOOP
ww=fltarr(nbins)
wwo=fltarr(nbins)
wwc=fltarr(nbins)
FOR ib=0,nbins-1 DO BEGIN &$
pdf_tas2d_sm_m(immm,is,ib)=MEAN(pdf_tas2d_sm(*,*,is,ib),/NaN) &$
pdf_tasc2d_sm_m(immm,is,ib)=MEAN(pdf_tasc2d_sm(*,*,is,ib),/NaN) &$
pdf_obs2d_sm_m(immm,is,ib)=MEAN(pdf_obs2d_sm(*,*,is,ib),/NaN) &$
ww(ib)=pdf_tas2d_sm_m(immm,is,ib)*bins(ib)/N_ELEMENTS(reform(pdf_tas2d_sm_m(immm,is,*))) &$
wwc(ib)=pdf_tasc2d_sm_m(immm,is,ib)*bins(ib)/N_ELEMENTS(reform(pdf_tas2d_sm_m(immm,is,*))) &$
wwo(ib)=pdf_obs2d_sm_m(immm,is,ib)*bins(ib)/N_ELEMENTS(reform(pdf_tas2d_sm_m(immm,is,*))) &$
ENDFOR &$
print,MASK(immm),is,total(ww)-273.15,total(wwc)-273.15,total(wwo)-273.15
ENDFOR

!p.color=0
!p.background=255


window,0+immm,retain=2
loadct,39
!p.multi=[0,2,2]
pdf_data=reform(pdf_tas2d_sm_m(immm,0,*))
pdf_datac=reform(pdf_tasc2d_sm_m(immm,0,*))
pdf_datao=reform(pdf_obs2d_sm_m(immm,0,*))
PLOT, bins-273.15, pdf_datao/N_ELEMENTS(pdf_data),xrange=[-20,20],yrange=[0,0.16],xstyle=1,ystyle=1,title=mask(immm)
OPLOT, bins-273.15, pdf_data/N_ELEMENTS(pdf_data),color=50
OPLOT, bins-273.15, pdf_datac/N_ELEMENTS(pdf_data),color=200
pdf_data=reform(pdf_tas2d_sm_m(immm,1,*))
pdf_datac=reform(pdf_tasc2d_sm_m(immm,1,*))
pdf_datao=reform(pdf_obs2d_sm_m(immm,1,*))
PLOT, bins-273.15, pdf_datao/N_ELEMENTS(pdf_data),xrange=[-10,30],yrange=[0,0.16],xstyle=1,ystyle=1
OPLOT, bins-273.15, pdf_data/N_ELEMENTS(pdf_data),color=50
OPLOT, bins-273.15, pdf_datac/N_ELEMENTS(pdf_data),color=200
pdf_data=reform(pdf_tas2d_sm_m(immm,2,*))
pdf_datac=reform(pdf_tasc2d_sm_m(immm,2,*))
pdf_datao=reform(pdf_obs2d_sm_m(immm,2,*))
PLOT, bins-273.15, pdf_datao/N_ELEMENTS(pdf_data),xrange=[0,40],yrange=[0,0.16],xstyle=1,ystyle=1
OPLOT, bins-273.15, pdf_data/N_ELEMENTS(pdf_data),color=50
OPLOT, bins-273.15, pdf_datac/N_ELEMENTS(pdf_data),color=200
pdf_data=reform(pdf_tas2d_sm_m(immm,3,*))
pdf_datac=reform(pdf_tasc2d_sm_m(immm,3,*))
pdf_datao=reform(pdf_obs2d_sm_m(immm,3,*))
PLOT, bins-273.15, pdf_datao/N_ELEMENTS(pdf_data),xrange=[-10,30],yrange=[0,0.16],xstyle=1,ystyle=1
OPLOT, bins-273.15, pdf_data/N_ELEMENTS(pdf_data),color=50
OPLOT, bins-273.15, pdf_datac/N_ELEMENTS(pdf_data),color=200


;SAVIN"G DATA
pdf_data=reform(pdf_tas2d_sm_m(immm,*,*))
pdf_datac=reform(pdf_tasc2d_sm_m(immm,*,*))
pdf_datao=reform(pdf_obs2d_sm_m(immm,*,*))
IF MODEL_ID EQ 'ENS_AVE' THEN BEGIN
ofile='DATA/PDF_'+VAR+'_ENSE_AVE_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+mask(immm)+'.dat'
save,filename=ofile,pdf_data,bins
ofile='DATA/PDF_'+VAR+'_CORR_ENS_AVE_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+mask(immm)+'.dat'
save,filename=ofile,pdf_datac,bins
ENDIF ELSE BEGIN
ofile='DATA/PDF_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+mask(immm)+'.dat'
save,filename=ofile,pdf_data,bins
ofile='DATA/PDF_'+VAR+'_CORR_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+mask(immm)+'.dat'
save,filename=ofile,pdf_datac,bins
ENDELSE
file='DATA/PDF_OBS_'+VAR+'_'+MODEL_ID+'_'+CASE_ID+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+mask(immm)+'.dat'
save,filename=ofile,pdf_datao,bins


ENDFOR ; END MASK LOOP
STOP

END
