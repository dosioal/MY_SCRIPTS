;;THIS IDL FILE CONTAINS THE PROCEDURES REQUIRED
;;FOR THE APPLICATION OF THE CORRECTED TIME SERIES
;;
;; WORK BLOCK LEADER: S. HAGEMANN
;;
;;
;;
;; FILENAME: apply_P_cor.pro
;; AUTHORS:  C. PIANI AND J. O. HAERTER
;; DATE:     SEPTEMBER 28, 2009
;; PROJECT:  EU WATCH PROJECT
;; requires: definitions.pro, definitions_internal.pro, functions.pro
;;
; Applies interpolated monthly bias correction factors for
; precipitation to the given application period.
;______________________________________________________________
;
;
close,3
openw,3,'log_apply_P_cor.txt'
; the main loop over all the months
for mon=0,n_elements(month)-1 do begin
;
monm1=mon-1
if (monm1 eq -1) then monm1=11
monp1=mon+1
if (monp1 eq 12) then monp1=0
;
; restore the current month correction factors
restore,outputdir+'pr_cor_'+derivation_period+'_'+month(mon)+run+'.dat'
;
A0=a_pr
B0=b_pr
T0=tau_pr
X00=x0_pr
;
print,'done restoring BC params: pr_cor_'+derivation_period+'_'+month(mon);*********
;*******************************************************************
; loading the data for the model data to be corrected.
pfile=pathmodel+MODELroot+application_period+'_'+month(mon)+run+'.dat'
print,'restoring ' + pfile
restore,pfile
;
l=n_elements(idldata(0,*))
pr_c=idldata
pr_total=idldata
;
; loading the data for the model snowfall data, this is important so
; that the original snow/total_precip ratio can be preserved. 
pfile=pathmodel+MODELroot_PRSN+application_period+'_'+month(mon)+run+'.dat'
print,'restoring ' + pfile
restore,pfile
;
pr_c_prsn=idldata
;
; below 0.001 mm/d we do not attempt to separate snow and rain as this
; is rediculous. Also, we need to avoid producing singularities.
threshold=0.001/86400.0
snow_total_ratio=pr_c*0.0
we=where(pr_c gt threshold)
snow_total_ratio(we)=(1.0*pr_c_prsn(we))/(1.0*pr_c(we))
;
print,'done restoring model: '+pfile
;******************************************************************
;
; Performing a basic consistency check and exiting if any data is negative.
print,'Checking consistency of input data ...'
IF (min(pr_c) LT -1e-10 OR min(pr_c_prsn) LT -1e-10) THEN BEGIN
    print,'Model has negative precipitation or snow!'
    print,'This will likely result in erroneous output.'
ENDIF ELSE BEGIN
    print,'Data passed basic consistency check.'
ENDELSE
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
;
    FOR iday=0,nd-1 DO BEGIN
        pr_e_day=reform(pr_total(*,ind))*86400.0      
        pr_c_day=pr_e_day
;
;START space LOOP over all land points
        FOR i=0L,NUMLANDPOINTS-1 DO BEGIN
            n=i  
            P=pr_e_day(n)
            A= A0(n)
            B= B0(n)
            X0=X00(n)
            T=T0(n)
;
            IF (T GT 1e30) THEN p_c=P*B+A ; values for x<x_0 are negative by construction !!!                               
            IF (T LT Tau_cutoff) THEN BEGIN
                IF (P GE X0) THEN p_c=( A + B * (P-X0) ) * (   1-exp(  -(P-X0)/T  )  )
                IF (P LT X0) THEN p_c=0.0
                IF (P LT 0) THEN print,3,'P = ',P
            ENDIF
            IF (T LE 1e30 && T GE Tau_cutoff) THEN STOP
            p_c=0.5*(p_c+abs(p_c)) ; this is putting all neg points eq 0. 
;
            pr_c_day(n)=p_c
        endfor
;
        pr_c(*,ind)=pr_c_day/86400.0         
        ind=ind+1
;**************************************************************
    endfor
endfor
print,'days corrected: ',ind

; printing information on the period completed.
print,'BC complete: construction decade ', derivation_period, ', application period: ', application_period, ', month ',month(mon)
;
pr_c_snow=snow_total_ratio*pr_c
;
; saving the corrected timeseries in the pathmodel folder
save,filename=pathmodel+'Ptot_BCed_'+derivation_period+'_'+application_period+'_'+month(mon)+run+'.dat',pr_c
;
; saving the corrected timeseries in the pathmodel folder
save,filename=pathmodel+'Psno_BCed_'+derivation_period+'_'+application_period+'_'+month(mon)+run+'.dat',pr_c_snow
;
endfor
close,3
end
