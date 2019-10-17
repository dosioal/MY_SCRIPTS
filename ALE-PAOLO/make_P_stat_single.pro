;
PRO make_P_stat_single
;
DEVICE,decomposed=0
;
;
;-----------------------------------------------------------------------------------------------------------


;DATA= 'OBS'
MODEL_OBS='KNMI-RACMO2'
CASE_OBS='A1B_ECHAM5-r3'

;DATA= 'ENS_AVE'
DATA= 'ENS_AVE CORR'

;DATA= 'MOD'
;DATA= 'CORR'

IF DATA eq 'OBS' or DATA eq 'ENS_AVE' or DATA eq 'ENS_AVE CORR' THEN BEGIN
nmod=1
ENDIF ELSE BEGIN
MODEL_ID=['C4IRCA3','DMI-HIRHAM5','DMI-HIRHAM5','ETHZ-CLM','KNMI-RACMO2','MPI-M-REMO','SMHIRCA','SMHIRCA']
CASE_ID=['A1B_HadCM3Q16','A1B_ARPEGE','A1B_ECHAM5','SCN_HadCM3Q0','A1B_ECHAM5-r3','SCN_ECHAM5','A1B_BCM','A1B_ECHAM5-r3']
nmod=1.*N_ELEMENTS(MODEL_ID)
ENDELSE


CONSTRUCTION_PERIOD_START  =  '1961'
CONSTRUCTION_PERIOD_STOP   =  '1990'

APPLICATION_PERIOD_START   =  '1991'
APPLICATION_PERIOD_STOP    =  '2000'

NYEARS=fix(APPLICATION_PERIOD_STOP)-fix(APPLICATION_PERIOD_START)+1

VAR='Ptot'

month=['12','01','02','03','04','05','06','07','08','09','10','11']
NMONTHS=12
NDAYS=fltarr(12)
NDAYS_M=[31,31,28,31,30,31,30,31,31,30,31,30]
NDAYS_S=[31+31+28,31+30+31,30+31+31,30+31+30]

;=========================
;MODEL LOOP
FOR imd=0,nmod-1 DO BEGIN

;DIMENSION
CASE DATA OF
'MOD': BEGIN
print,'OPENING MONTH ',month(0),' MODEL ',MODEL_ID(imd)+'_'+CASE_ID(imd)
ofile='DATA/MOD_'+VAR+'_'+MODEL_ID(imd)+'_'+CASE_ID(imd)+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(0)+'.dat'
;tas2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'OPENING',ofile
restore,filename=ofile
nxm=mnx
nym=mny
land2d_m=landmask_c
END

'CORR': BEGIN
print,'OPENING MONTH ',month(0),' MODEL ',MODEL_ID(imd)+'_'+CASE_ID(imd)
ofile='DATA/MOD_BC_'+VAR+'_CORR_'+MODEL_ID(imd)+'_'+CASE_ID(imd)+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(0)+'.dat'
;tasc2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'OPENING',ofile
restore,filename=ofile
nxm=mnx
nym=mny
land2d_m=landmask_c
END

'OBS': BEGIN
ofile='DATA/OBS_'+VAR+'_'+MODEL_OBS+'_'+CASE_OBS+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(0)+'.dat'
;obs2d,nxm,nym,nx_o,ny_o,nx0_o,ny0_o,landmask_a,ndays_o
restore,filename=ofile
print,'RESTORING '+ofile
nxm=nx_o
nym=ny_o
land2d_m=landmask_a
END

'ENS_AVE': BEGIN
ofile='DATA/ENS_AVE_MOD_'+VAR+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(0)+'.dat'
;tas2d_ave,tas2d_ave_sd,nxm,nym,landmask_com,mndays
restore,filename=ofile
print,'RESTORING '+ofile
land2d_m=landmask_com
END

'ENS_AVE CORR': BEGIN
ofile='DATA/ENS_AVE_MOD_'+VAR+'_CORR_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(0)+'.dat'
;tas2d_ave,tas2d_ave_sd,nxm,nym,landmask_com,mndays
restore,filename=ofile
print,'RESTORING '+ofile
land2d_m=landmask_com
END

ENDCASE

tas2d_m=fltarr(nxm,nym,3*4,310)

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

CASE DATA OF
'MOD': BEGIN
print,'OPENING MONTH ',month(0),' MODEL ',MODEL_ID(imd)+'_'+CASE_ID(imd)
ofile='DATA/MOD_'+VAR+'_'+MODEL_ID(imd)+'_'+CASE_ID(imd)+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;tas2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'OPENING',ofile
restore,filename=ofile
nxm=mnx
nym=mny
land2d_m=landmask_c
tas2d_m(*,*,im,0:MNDAYS-1)=tas2d
END

'CORR': BEGIN
ofile='DATA/MOD_BC_'+VAR+'_CORR_'+MODEL_ID(imd)+'_'+CASE_ID(imd)+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;tasc2d,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c,mndays
print,'RESTORING '+ofile
restore,filename=ofile
nxm=mnx
nym=mny
land2d_m=landmask_c
tas2d_m(*,*,im,0:MNDAYS-1)=tasc2d
END

'OBS': BEGIN
ofile='DATA/OBS_'+VAR+'_'+MODEL_OBS+'_'+CASE_OBS+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;obs2d,nxm,nym,nx_o,ny_o,nx0_o,ny0_o,landmask_a,ndays_o
restore,filename=ofile
print,'RESTORING '+ofile
nxm=nx_o
nym=ny_o
land2d_m=landmask_a
tas2d_m(*,*,im,0:MNDAYS-1)=obs2d
END

'ENS_AVE': BEGIN
ofile='DATA/ENS_AVE_MOD_'+VAR+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;tas2d_ave,tas2d_ave_sd,nxm,nym,landmask_com,mndays
restore,filename=ofile
print,'RESTORING '+ofile
land2d_m=landmask_com
tas2d_m(*,*,im,0:MNDAYS-1)=tas2d_ave
END

'ENS_AVE CORR': BEGIN
ofile='DATA/ENS_AVE_MOD_'+VAR+'_CORR_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
;tas2d_ave,tas2d_ave_sd,nxm,nym,landmask_com,mndays
restore,filename=ofile
print,'RESTORING '+ofile
land2d_m=landmask_com
tas2d_m(*,*,im,0:MNDAYS-1)=tasc2d_ave
END

ENDCASE

imm=imm+1

IF imm GT 2 THEN imm=0
ENDFOR  ;END im

ENDFOR

tas_com=fltarr(nxm,nym,12,31,NYEARS)

FOR iyy=0,NYEARS-1 DO BEGIN &$
FOR im=0,NMONTHS-1 DO BEGIN &$
tas_com(*,*,im,0:NDAYS_M(im)-1,iyy)=reform(tas2d_m(*,*,im,NDAYS_M(im)*iyy:NDAYS_M(im)*(iyy+1)-1)) &$
ENDFOR &$
ENDFOR

tas_m=fltarr(nxm,nym,4,31*3,NYEARS)
tas_m(*,*,*,*,*)=!VALUES.F_NAN

FOR iy=0,NYEARS-1 DO BEGIN &$
FOR im=0,3 DO BEGIN &$
;	tas_m(*,im,0:30,iy)=tas_com(*,im*3,*,iy) &$
;	tas_m(*,im,31:61,iy)=tas_com(*,im*3+1,*,iy) &$
;	tas_m(*,im,62:92,iy)=tas_com(*,im*3+2,*,iy) &$
;print,im*3,NDAYS_M(im*3)-1,NDAYS_M(im*3)+NDAYS_M(im*3+1)-1,NDAYS_M(im*3)+NDAYS_M(im*3+1)+NDAYS_M(im*3+2)-1 &$
tas_m(*,*,im,0:NDAYS_M(im*3)-1,iy)=tas_com(*,*,im*3,0:NDAYS_M(im*3)-1,iy) &$
tas_m(*,*,im,NDAYS_M(im*3):NDAYS_M(im*3)+NDAYS_M(im*3+1)-1,iy)=tas_com(*,*,im*3+1,0:NDAYS_M(im*3+1)-1,iy) &$
tas_m(*,*,im,NDAYS_M(im*3)+NDAYS_M(im*3+1):NDAYS_M(im*3)+NDAYS_M(im*3+1)+NDAYS_M(im*3+2)-1,iy)=tas_com(*,*,im*3+2,0:NDAYS_M(im*3+2)-1,iy) &$
ENDFOR &$
ENDFOR 

Index_d_tas=fltarr(NYEARS)
Max_d_tas=fltarr(NYEARS)
Index_d_tas_m=fltarr(nxm,nym,4)
Max_d_tas_m=fltarr(nxm,nym,4)

Index_d_tas_m(*,*,*)=!VALUES.F_NAN

hp=fltarr(NYEARS)
hp_tas_m=fltarr(nxm,nym,4)


FOR ix=0,nxm-1 DO BEGIN &$
FOR iy=0,nym-1 DO BEGIN &$
IF land2d_m(ix,iy) EQ 1 THEN BEGIN
FOR im=0,3 DO BEGIN &$
FOR iyy=0,NYEARS-1 DO BEGIN &$

hp(iyy)=0. &$
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

td=WHERE(tas_m(ix,iy,im,*,iyy) GT 0.01,count) &$

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


ENDFOR &$   ;END YEAR LOOP iy

Index_d_tas_m(ix,iy,im)=ROUND(MEAN(Index_d_tas)) &$
Max_d_tas_m(ix,iy,im)=ROUND(MEAN(Max_d_tas)) &$
hp_tas_m(ix,iy,im)=ROUND(MEAN(hp)) &$
ENDFOR &$ ;end im

ENDIF
ENDFOR &$ ;end iy
ENDFOR ; end ix

;SAVING DATA

CASE DATA OF
'ENS_AVE': BEGIN
print,'SAVING MONTH '+month(im)+' ENS AVER'
ofile='DATA/DROUGHT_'+DATA+'_SM_ENS_AVE_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'.dat'
save,filename=ofile,Index_d_tas_m,Max_d_tas_m,hp_tas_m,nxm,nym,landmask_com
END

'ENS_AVE CORR': BEGIN
print,'SAVING MONTH '+month(im)+' ENS AVER CORR'
ofile='DATA/DROUGHT_'+DATA+'_SM_ENS_AVE_CORR_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'.dat'
save,filename=ofile,Index_d_tas_m,Max_d_tas_m,hp_tas_m,nxm,nym,landmask_com
END

'MOD': BEGIN
print,'SAVING MONTH '+month(im)+' MODEL '+MODEL_ID(imd)+'_'+CASE_ID(imd)
ofile='DATA/DROUGHT_'+DATA+'_SM_'+MODEL_ID(imd)+'_'+CASE_ID(imd)+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'.dat'
save,filename=ofile,Index_d_tas_m,Max_d_tas_m,hp_tas_m,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c
END 

'CORR': BEGIN
print,'SAVING MONTH '+month(im)+' MODEL CORR'+MODEL_ID(imd)+'_'+CASE_ID(imd)
ofile='DATA/DROUGHT_'+DATA+'_SM_CORR_'+MODEL_ID(imd)+'_'+CASE_ID(imd)+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'.dat'
save,filename=ofile,Index_d_tas_m,Max_d_tas_m,hp_tas_m,nxm,nym,mx0,my0,mnx,mny,mnx0,mny0,landmask_c
END

'OBS': BEGIN
print,'SAVING MONTH '+month(im)+' OBS '+MODEL_OBS
ofile='DATA/DROUGHT_OBS_'+VAR+'_'+MODEL_OBS+'_'+CASE_OBS+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'_'+month(im)+'.dat'
save,filename=ofile,Index_d_tas_m,Max_d_tas_m,hp_tas_m,nxm,nym,nx_o,ny_o,nx0_o,ny0_o,landmask_a
END

ENDCASE


;PLOTS

Id2d_tas_sm = FLTARR(nxm,nym,4)
Im2d_tas_sm = FLTARR(nxm,nym,4)
hp2d_tas_sm = FLTARR(nxm,nym,4)
Id2d_tas_sm(*,*,*)=-999.
Im2d_tas_sm(*,*,*)=-999.
hp2d_tas_sm(*,*,*)=-999.
FOR j=0,3 DO BEGIN &$
FOR ix=0L,nxm-1 DO BEGIN &$
FOR iy=0L,nym-1 DO BEGIN &$
IF land2d_m(ix,iy) EQ 1 AND (Index_d_tas_m(ix,iy,j) GE 0) THEN BEGIN &$
         Id2d_tas_sm(ix,iy,j)=Index_d_tas_m(ix,iy,j) &$
         Im2d_tas_sm(ix,iy,j)=Max_d_tas_m(ix,iy,j) &$
         hp2d_tas_sm(ix,iy,j)=hp_tas_m(ix,iy,j) &$
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
contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title='Drought index DJF '+DATA,position=[0.05,0.55,0.45,0.95]
contour,Id2d_tas_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1.5,color=25,position=[0.96,0.55,0.99,0.95],/vertical

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title='Drought index MAM',position=[0.5,0.55,0.9,0.95]
contour,Id2d_tas_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title='Drought index JJA',position=[0.05,0.05,0.45,0.45]
contour,Id2d_tas_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title='Drought index SON',position=[0.5,0.05,0.9,0.45]
contour,Id2d_tas_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
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
contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title='Max dry spell length DJF '+DATA,position=[0.05,0.55,0.45,0.95]
contour,Im2d_tas_sm(*,*,0),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0
colorbar,min=minvalue,max=maxvalue,c_colors=rgrColValues,divisions=iNumOfConts-1,charsize=1.5,color=25,position=[0.96,0.55,0.99,0.95],/vertical

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title='Max dry spell length MAM',position=[0.5,0.55,0.9,0.95]
contour,Im2d_tas_sm(*,*,1),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title='Max dry spell length JJA',position=[0.05,0.05,0.45,0.45]
contour,Im2d_tas_sm(*,*,2),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title='Max dry spell length SON',position=[0.5,0.05,0.9,0.45]
contour,Im2d_tas_sm(*,*,3),levels=rgrContValues,/fill,c_colors=rgrColValues,xstyle=1,ystyle=1,/overplot
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
contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title='Heavy Prec. Events DJF '+DATA,position=[0.05,0.55,0.45,0.95]
contour,hp2d_tas_sm(*,*,0),levels=rgrContValues,/fill,c_colors=REVERSE(rgrColValues),xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0
colorbar,min=minvalue,max=maxvalue,c_colors=REVERSE(rgrColValues),divisions=iNumOfConts-1,charsize=1.5,color=25,position=[0.96,0.55,0.99,0.95],/vertical

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title='Heavy Prec. Events MAM',position=[0.5,0.55,0.9,0.95]
contour,hp2d_tas_sm(*,*,1),levels=rgrContValues,/fill,c_colors=REVERSE(rgrColValues),xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title='Heavy Prec. Events JJA',position=[0.05,0.05,0.45,0.45]
contour,hp2d_tas_sm(*,*,2),levels=rgrContValues,/fill,c_colors=REVERSE(rgrColValues),xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

contour,land2d_m,xstyle=1,ystyle=1,c_colors=0,title='Heavy Prec. Events SON',position=[0.5,0.05,0.9,0.45]
contour,hp2d_tas_sm(*,*,3),levels=rgrContValues,/fill,c_colors=REVERSE(rgrColValues),xstyle=1,ystyle=1,/overplot
contour,land2d_m,xstyle=1,ystyle=1,/overplot,thick=0.5,color=0

goto,SKIP
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
device,file='./PLOTS/PLOT_SM_PREP_INDEX_'+MODEL_ID(imd)+'_'+CASE_ID(imd)+'_'+CONSTRUCTION_PERIOD_START+'_'+CONSTRUCTION_PERIOD_STOP+'_'+APPLICATION_PERIOD_START+'_'+APPLICATION_PERIOD_STOP+'.ps',/landscape,BITS_PER_PIXEL=8, COLOR=1,DECOMPOSED=0

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

SKIP:

ENDFOR ;END MODEL LOOP

STOP
END
