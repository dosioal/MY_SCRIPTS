;i;THIS IDL FILE CONTAINS THE PROCEDURES REQUIRED
;;FOR THE APPLICATION OF THE CORRECTED TIME SERIES
;;
;; WORK BLOCK LEADER: S. HAGEMANN
;;
;;
;;
;; FILENAME: apply_T_cor.pro
;; AUTHORS:  C. PIANI AND J. O. HAERTER
;; DATE:     NOVEMBER 17, 2009
;; PROJECT:  EU WATCH PROJECT
;; requires: definitions.pro, definitions_internal.pro, functions.pro
;;
; Applies interpolated monthly bias correction factors for temperature
; to the given application period.
;______________________________________________________________
;
;
close,3
print,'APPLYING T CORR'
openw,3,'log_apply_T_cor.txt'
; the main loop over all the months
for mon=0,(n_elements(month)-1) do begin
;
monm1=mon-1
if (monm1 eq -1) then monm1=11
monp1=mon+1
if (monp1 eq 12) then monp1=0
;
;Restoring three months of correction factors. This is necessary so
;the correction factors can be interpolated from day to day.
;
; restore the previous month correction factors
restore,outputdir+'tas_cor_'+derivation_period+'_'+month(monm1)+runToCorrect+'.dat'
Am1=a_tas
afm1=a_tf
adm1=a_td
;
; restore the current month correction factors
restore,outputdir+'tas_cor_'+derivation_period+'_'+month(mon)+runToCorrect+'.dat'
A=a_tas
af=a_tf
ad=a_td
;
; restore the following month correction factors
restore,outputdir+'tas_cor_'+derivation_period+'_'+month(monp1)+runToCorrect+'.dat'
Ap1=a_tas
afp1=a_tf
adp1=a_td
;
print,'done restoring BC params: tas_cor_'+derivation_period+'_'+month(mon);*********
;*******************************************************************
; loading the data for the model data to be corrected.
pfileT=pathmodel+MODELroot_T+application_period+'_'+month(mon)+run+'.dat'
print,'restoring ' + pfileT
restore,pfileT
help,idldata
tas_e=idldata
l=n_elements(tas_e(0,*))
;
pfileTmax=pathmodel+MODELroot_Tmin+application_period+'_'+month(mon)+run+'.dat'
print,'restoring ' + pfileTmax
restore,pfileTmax
help,idldata
tmin_e=idldata
l=n_elements(tmin_e(0,*))
;
pfileTmin=pathmodel+MODELroot_Tmax+application_period+'_'+month(mon)+run+'.dat'
print,'restoring ' + pfileTmin
restore,pfileTmin
help,idldata
tmax_e=idldata
l=n_elements(tmax_e(0,*))
;
tas_c=tas_e
tmin_c=tmin_e
tmax_c=tmax_e
;
print,'done restoring model: '+pfileT
print,'done restoring model: '+pfileTmax
print,'done restoring model: '+pfileTmin
;******************************************************************
; Performing a basic consistency check: are there negative
; temperatures, are there negative diurnal ranges or days where not tmin<=tmean<=tmax.
;
print,'Checking consistency of input data ...'
errors=0
IF (min(tas_e) LT 0.0) THEN BEGIN
    print,'Model has negative temperature!', min(tas_e),where(min(tas_e) LT 0.0) 
    errors=1
ENDIF
IF (min(tmax_e-tas_e) LT 0.0) THEN BEGIN
    cc=tmax_e-tas_e
    aa=where(cc LT 0.0,count)
    bb=ARRAY_INDICES(cc,aa)
    print,'Model has points with Tmax<Tmean in',count,' points!(',count*100./N_ELEMENTS(cc),' %)'
    print,tmax_e(bb(0),bb(1)),tas_e(bb(0),bb(1))
    ;print,bb[0,*]
    ;print,bb[1,*]
    print,'correcting the data...'
    tmax_e(bb[0,*],bb[1,*])=tas_e(bb[0,*],bb[1,*])
    print,tmax_e(bb(0),bb(1)),tas_e(bb(0),bb(1))
    errors=1
    ;print,'Rewriting '+pfiletmax
    ;save,filename=pfiletmax,tmax_e
ENDIF
IF (min(tmax_e-tas_e) LT 0.0) THEN STOP ;Checking again...

IF (max(tmin_e-tas_e) GT 0.0) THEN BEGIN
    cc=tmin_e-tas_e
    aa=where(cc GT 0.0,count)
    bb=ARRAY_INDICES(cc,aa)
    print,'Model has points with Tmin>Tmean in',count,' points!(',count*100./N_ELEMENTS(cc),' %)'
    print,tmin_e(bb(0),bb(1)),tas_e(bb(0),bb(1))
    ;print,bb[0,*]
    ;print,bb[1,*]
    print,'correcting the data...'
    tmin_c(bb[0,*],bb[1,*])=tas_e(bb[0,*],bb[1,*])
    print,tmin_e(bb(0),bb(1)),tas_e(bb(0),bb(1))
    errors=1
    ;print,'Rewriting '+pfiletmin
    ;save,filename=pfiletmin,tmin_e
ENDIF
IF (min(tmin_e-tas_e) GT 0.0) THEN STOP ;Checking again...

IF (min(tmax_e-tmin_e) LT 0.0) THEN BEGIN
    cc=tmax_e-tmin_e
    aa=where(cc LT 0.0,count)
    bb=ARRAY_INDICES(cc,aa)
    print,'Model has negative diurnal range in',count,' points!(',count*100./N_ELEMENTS(cc),' %)'
    print,tmin_e(bb(0),bb(1)),tmax_e(bb(0),bb(1))
    ;print,bb[0,*]
   ; print,bb[1,*]
    print,'correcting the data...'
    tmin_e(bb[0,*],bb[1,*])=tas_e(bb[0,*],bb[1,*])
    tmax_e(bb[0,*],bb[1,*])=tas_e(bb[0,*],bb[1,*])
    print,tmin_e(bb(0),bb(1)),tmax_e(bb(0),bb(1))
    errors=1
    ;print,'Rewriting '+pfiletmax
    ;save,filename=pfiletmax,tmax_e
    ;print,'Rewriting '+pfiletmin
    ;save,filename=pfiletmin,tmin_e
ENDIF
IF (errors eq 0) THEN BEGIN
    print,'Data passed basic consistency check.'
ENDIF ELSE BEGIN
    print,'There are possible data inconsistencies.'
ENDELSE
;
; ******************************************************************
;
yr=APPLICATION_PERIOD_START
nyear=1L*(APPLICATION_PERIOD_STOP)-1L*(APPLICATION_PERIOD_START)+1
ind     = 0 ; a counter for the days
ind_mon = 0
;
; assign the time values.
FOR y=0,nyear-1 DO BEGIN
    IF (yearl_m EQ 365) THEN BEGIN	
    nd=julday(month(mon)+1,1,yr+y)-julday(month(mon),1,yr+y); days in the current month
    ENDIF ELSE BEGIN
    nd=30   
    ENDELSE 
;    print,'NDAYS in MONTH=',nd

    for iday=0,nd-1 do begin
        pr_e_day=reform(idldata(*,ind))*86400.0      
        pr_c_day=pr_e_day
;
        d=-0.5+(iday*1.0/nd) 
;
; Weighting factors for the previous month (dm1), the current month
; (d0) and the following month (dp1) are evaluated, such that for the
; first (last) day of the month the correction factors of the previous
; (following) month are equally weighted , i.e. dm1=d0=0.5
; (dp1=d0=0.5), and for the days in the middle of the month d0=1,
; dp1=dm1=0.
;
        dm1=(abs(d)-d)*0.5
        d0=1-abs(d)
        dp1=(d+abs(d))*0.5
;        
; producing a weighted average of the three pairs of coefficients.
IF (month(mon) EQ 11 OR  month(mon) EQ 10)  then begin
ENDIF
        a_iday  = Am1*dm1  +  A*d0 +  Ap1*dp1
        ad_iday = Adm1*dm1 + Ad*d0 + Adp1*dp1
        af_iday = Afm1*dm1 + Af*d0 + Afp1*dp1
;
;START space LOOP over all land points
        for n=0L,NUMLANDPOINTS-1 do begin
;
            T=tas_e(n,ind)
            Td=Tmax_e(n,ind)-Tmin_e(n,ind)
;            T=tas_c(n,ind)
;            Td=Tmax_c(n,ind)-Tmin_c(n,ind)
            IF (Td GT 0.0) THEN Tf=(tas_e(n,ind)-Tmin_e(n,ind))/Td
;            IF (Td GT 0.0) THEN Tf=(tas_c(n,ind)-Tmin_c(n,ind))/Td
            IF (Td EQ 0.0) THEN Tf=0.5
            IF (Td LT 0.0) THEN BEGIN
		    print, 'Model has negative diurnal range in',n,ind,'Tmax=',Tmax_c(n,ind),'Tmin=',Tmin_c(n,ind),Tmax_c(n,ind)-Tmin_c(n,ind),Td
	    STOP
            ENDIF
;
            T_corr=  a_iday(n,0)+a_iday(n,1)*T
            Td_corr=ad_iday(n,0)+ad_iday(n,1)*Td
            Td_corr=0.5*(Td_corr+abs(Td_corr))
            Tf_corr=af_iday(n,0)+af_iday(n,1)*Tf
            Tf_corr=0.5*(Tf_corr+abs(Tf_corr))
            IF (Tf_corr GT 1.0) THEN If_corr=1.0
;
            tas_c(n,ind)=T_corr
            tmin_c(n,ind)=T_corr-Td_corr*Tf_corr
            tmax_c(n,ind)=T_corr+Td_corr*(1-Tf_corr)
;
        endfor
;

        ind=ind+1
;**************************************************************
    endfor
endfor
print,'days corrected: ',ind
;print,tas_c(0,0),tmax_c(0,0),tmin_c(0,0)
;
; printing information on the period completed.
print,'BC complete: construction decade ', derivation_period, ', application period: ', application_period, ', month ',month(mon)
;
; saving the corrected timeseries in the pathmodel folder
save,filename=pathmodel+'T_BCed_'+derivation_period+'_'+application_period+'_'+month(mon)+run+'.dat',tas_c
save,filename=pathmodel+'Tmin_BCed_'+derivation_period+'_'+application_period+'_'+month(mon)+run+'.dat',tmin_c
save,filename=pathmodel+'Tmax_BCed_'+derivation_period+'_'+application_period+'_'+month(mon)+run+'.dat',tmax_c
endfor
close,3
end
