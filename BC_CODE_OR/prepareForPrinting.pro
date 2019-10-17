; assume 'definitions.pro' has been run
; assume 'definitions_internal.pro' has been run
; assume 'convertOrgFiles.pro' has been run
; 
;
IF (OUTPUT_DATAFORMAT_2D EQ 1 OR OUTPUT_DATAFORMAT_1D EQ 1) THEN BEGIN
; specify the months to be converted to the original netcdf format.
    ourmonth=['01','07']
if (T_Correct EQ 1 and P_Correct EQ 1) then types=['Ptot_BCed_','Psno_BCed_','T_BCed_','Tmax_BCed_','Tmin_BCed_']
if (T_Correct EQ 1 and P_Correct EQ 0) then types=['T_BCed_','Tmax_BCed_','Tmin_BCed_']
if (T_Correct EQ 0 and P_Correct EQ 1) then types=['Ptot_BCed_','Psno_BCed_']
;
;the main loop over the types to be output
for typ=0,(n_elements(types)-1) do begin
    operator=['timmean']
    if (types(typ) EQ 'Ptot_BCed_') then operator=['eca_r10mm','timvar','timmean']
    if (types(typ) EQ 'T_BCed_') then operator=['eca_fd','timvar','timmean']
print,'working on : ',operator
;
; the main loop over all months
    for mon=0,(n_elements(ourmonth)-1) do begin
; model data has now been read and the 2d array is set to zero.
;*******************************************************************   
;
        prcddir=pathdat+BC_runnumber+'/'+'processed/'
        spawn,'mkdir -p '+prcddir
; specify the output file containing the netcdf data.
        orgfile=pathmodel+types(typ)+derivation_period+'_'+application_period+'_'+ourmonth(mon)+run+'.nc'
;        
        for op=0, n_elements(operator)-1 do begin
            type=types(typ)
            prcdfile=prcddir+type+derivation_period+'_'+application_period+'_'+ourmonth(mon)+run+'_'+operator(op)+'.nc'
            IF (type EQ 'Ptot_BCed_' OR type EQ 'Psno_BCed_') THEN BEGIN
                spawn,'cdo '+operator(op)+' -mulc,86400 '+orgfile+' dummy'
                spawn,'cdo setdate,nodate dummy dummy1'
                spawn,'cdo -f srv copy -ln -addc,1 dummy1 dummy2'
                spawn,'cdo -f nc -g grid.txt copy dummy2 '+prcdfile
                print,'cdo '+operator(op)+'-mulc,86400 '+orgfile+' '+prcdfile
                IF (type EQ 'Ptot_BCed_') THEN BEGIN
                    IF (operator(op) EQ 'timmean') THEN spawn,prcddir+'plot_BC_precip_mean_auto.gmt '+prcdfile
                    IF (operator(op) EQ 'timvar') THEN spawn,prcddir+'plot_BC_precip_var_auto.gmt '+prcdfile
                ENDIF
            ENDIF ELSE BEGIN
                spawn,'cdo '+operator(op)+' '+orgfile+' dummy'
                spawn,'cdo setdate,nodate dummy dummy1'
                spawn,'cdo -f srv copy dummy1 dummy2'
                spawn,'cdo -f nc -g grid.txt copy dummy2 '+prcdfile
                print,'cdo '+operator(op)+' '+orgfile+' '+prcdfile
                IF (type EQ 'T_BCed_') THEN BEGIN
                    IF (operator(op) EQ 'timmean') THEN spawn,prcddir+'plot_BC_temp_mean_auto.gmt '+prcdfile
                    IF (operator(op) EQ 'timvar') THEN spawn,prcddir+'plot_BC_temp_var_auto.gmt '+prcdfile
                ENDIF
            ENDELSE

        endfor
; 
    endfor
endfor
endif
end
