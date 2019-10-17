;;THIS IDL FILE CONVERTS A BIAS CORRECTED IDL SAVED FILE TO A LATLON
;;NETCDF FILE OR COMPRESSED WFD NETCDF FILE.
;;
;; WORK BLOCK LEADER: S. HAGEMANN
;;
;;
;;
;; FILENAME: idl2latlon_v1.pro
;; AUTHORS:  C. PIANI AND J. O. HAERTER
;; DATE:     NOVEMBER 18, 2009
;; PROJECT:  EU WATCH PROJECT
;; REQUIRES: definitions.pro, definitions_internal.pro, create1dNCDF.pro, createNCDF_v2.pro
;;
;;
;; __________________________________________________________________
;;
;;
;
IF (OUTPUT_DATAFORMAT_2D EQ 1 OR OUTPUT_DATAFORMAT_1D EQ 1) THEN BEGIN
;
if (T_Correct EQ 1 and P_Correct EQ 1) then types=['Ptot_BCed_','Psno_BCed_','T_BCed_','Tmax_BCed_','Tmin_BCed_']
if (T_Correct EQ 1 and P_Correct EQ 0) then types=['T_BCed_','Tmax_BCed_','Tmin_BCed_']
if (T_Correct EQ 0 and P_Correct EQ 1) then types=['Ptot_BCed_','Psno_BCed_']
;
;
;the main loop over the types to be output
for typ=0,(n_elements(types)-1) do begin
; the main loop over all months
for mon=0,(n_elements(month)-1) do begin
; model data has now been read and the 2d array is set to zero.
;*******************************************************************
; loading the data for the model data to be corrected.
    pfile=pathmodel+types(typ)+derivation_period+'_'+application_period+'_'+month(mon)+run+'.dat'
    
; specify the output file containing the netcdf data.
    outfile=pathmodel+types(typ)+derivation_period+'_'+application_period+'_'+month(mon)+run+'.nc'
    
; specify the output file containing the compressed netcdf data.
    outfileCompressed=pathmodel+'COMPRESSED_'+types(typ)+derivation_period+'_'+application_period+'_'+month(mon)+run+'.nc'
    
    restore,pfile
;
    print,'done restoring corrected data: '+derivation_period+'-> '+application_period+'.' 
;
;
    if (types(typ) eq 'Ptot_BCed_') then begin
        l=n_elements(pr_c(0,*))    
        datarr=pr_c
        header=['precip','mm/s','total precipitation','total model precipitation']
    endif
    if (types(typ) eq 'Psno_BCed_') then begin
        l=n_elements(pr_c_snow(0,*))    
        datarr=pr_c_snow
        header=['snow','mm/s','snow precipitation','model snow precipitation']
    endif
    if (types(typ) eq 'T_BCed_') then begin
        l=n_elements(tas_c(0,*))    
        datarr=tas_c
        header=['temp2','Kelvin','mean daily temperature','model temperature']
    endif
    if (types(typ) eq 'Tmin_BCed_') then begin
        l=n_elements(tmin_c(0,*))    
        datarr=tmin_c
        header=['Tmin','Kelvin','min. daily temperature','model min. temperature']
    endif
    if (types(typ) eq 'Tmax_BCed_') then begin
        l=n_elements(tmax_c(0,*))    
        datarr=tmax_c
        header=['Tmax','Kelvin','max. daily temperature','model max. temperature']
    endif
;
    print,'done restoring idl data: '+pfile
;******************************************************************
; the total number of days to be written to netcdf format.
    ndays=n_elements(datarr(1,*))  
    
; The data array required for passing the data to the createNCDF subroutine.
    data = FLTARR(ndays,dims(1),dims(0))
    data = data*0.0-9999.0
    
; total number of land points.
    nlandpoints=n_elements(datarr(*,1))
    
; some conversion to lat-lon coordinates
    FOR i=0L,NUMLANDPOINTS-1 DO BEGIN
        nlon=lon(i)
        nlat=lat(i)
        FOR t=0,ndays-1 DO BEGIN
            data(t,nlat,nlon)=datarr(i,t)
        ENDFOR
    ENDFOR
;
; write data to a 2D netcdf file.
    IF (OUTPUT_DATAFORMAT_2D EQ 1) THEN BEGIN
        print,createNCDF_v2(dims(0),dims(1),APPLICATION_PERIOD_START,1L*(APPLICATION_PERIOD_STOP)-1L*(APPLICATION_PERIOD_START)+1,month(1L*mon),ndays,data,outfile,header,yearl_m)
    ENDIF
;
; write the data to the compressed WFD format.
    IF (OUTPUT_DATAFORMAT_1D EQ 1) THEN BEGIN
        print,create1dNCDF(NUMLANDPOINTS,nxx,nyy,navlon,navlat,APPLICATION_PERIOD_START,1L*(APPLICATION_PERIOD_STOP)-1L*(APPLICATION_PERIOD_START)+1,month(1L*mon),ndays,datarr,outfileCompressed,header,yearl_m)
    ENDIF
;
endfor                          ; MONTH FOR LOOP ENDS
;
if (n_elements(month) eq 12) then begin
    IF (OUTPUT_DATAFORMAT_2D EQ 1) THEN BEGIN
        outfileFinal=pathFinalData+types(typ)+derivation_period+'_'+application_period+run+'.nc'
        outfileAll=pathmodel+types(typ)+derivation_period+'_'+application_period+'_'+month+run+'.nc'
        spawn,'cdo mergetime '+outfileAll(0)+' '+outfileAll(1)+' '+outfileAll(2)+' '+outfileAll(3)+' '+outfileAll(4)+' '+outfileAll(5)+' '+outfileAll(6)+' '+outfileAll(7)+' '+outfileAll(8)+' '+outfileAll(9)+' '+outfileAll(10)+' '+outfileAll(11)+' '+outfileFinal
    ENDIF
;
    IF (OUTPUT_DATAFORMAT_1D EQ 1) THEN BEGIN
        outfileAll=pathmodel+'COMPRESSED_'+types(typ)+derivation_period+'_'+application_period+'_'+month+run+'.nc'
        outfileFinal=pathFinalData+'COMPRESSED_'+types(typ)+derivation_period+'_'+application_period+run+'.nc'
        spawn,'cdo mergetime '+outfileAll(0)+' '+outfileAll(1)+' '+outfileAll(2)+' '+outfileAll(3)+' '+outfileAll(4)+' '+outfileAll(5)+' '+outfileAll(6)+' '+outfileAll(7)+' '+outfileAll(8)+' '+outfileAll(9)+' '+outfileAll(10)+' '+outfileAll(11)+' '+outfileFinal
    ENDIF
endif
;
endfor                          ; TYPES FOR LOOP ENDS
endif                           ; END OF PRIMARY IF LOOP
print,'PROGRAM SUCCESSFUL!'
end                             ; PROGRAM ENDS.
