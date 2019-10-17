;;THIS IDL FILE CONVERTS A BIAS CORRECTED IDL SAVED FILE TO A LATLON
;;NETCDF FILE.
;;
;; WORK BLOCK LEADER: S. HAGEMANN
;;
;; FILENAME: idl2latlon_v1.pro
;; AUTHORS:  C. PIANI AND J. O. HAERTER
;; DATE:     SEPTEMBER 29, 2009
;; PROJECT:  EU WATCH PROJECT
;; REQUIRES: definitions.pro
;; __________________________________________________________________
;;
;;
; The start and end times for the data to be converted from the idl
; format to the netcdf lat-lon format.
;
; specify a data format: 0 -> regular lat lon grid, 1 -> compressed
; WFD format as netcdf file.
function idl2latlon_function_v1,standard,DATAFORMAT,infile,outfile,YEAR_START,YEAR_STOP,mon,lat,lon,month,header,dims
;    
    restore,infile
;
    print,'done restoring '+ infile
;
   if (standard eq 1) then pr_e=idldata
   if (standard eq 0) then begin
       pos = strpos(infile,'obs')
       if pos[0] gt -1L then begin
           pr_e=pr
           print,'pr'
       endif else begin
           pos = strpos(infile,'BCed')
           if pos[0] gt -1L then begin
               pr_e=pr_c
           endif else begin 
               pr_e=pr_e
           endelse
       endelse
   endif
;
    l=n_elements(pr_e(0,*))
;
    outfileCompressed='COMPRESSED_'+outfile
;
    print,'done restoring idl data: '+infile
;******************************************************************
; the total number of days to be written to netcdf format.
    ndays=n_elements(pr_e(1,*))  
    
; The data array required for passing the data to the createNCDF subroutine.
    data = FLTARR(ndays,dims(1),dims(0))
    data = 0.0*data-9999.9
    
; total number of land points.
    if (ndays gt 1) then begin
        nlandpoints=n_elements(pr_e(*,1))
    endif else begin
        nlandpoints=n_elements(pr_e)
    endelse
    
; some conversion to lat-lon coordinates
    IF (ndays GT 1) THEN BEGIN
        FOR i=0L,nlandpoints-1 DO BEGIN
            nlon=lon(i)
            nlat=lat(i)
            FOR t=0,ndays-1 DO BEGIN
                data(t,nlat,nlon)=pr_e(i,t)
            ENDFOR
        ENDFOR
    ENDIF ELSE BEGIN
        FOR i=0L,nlandpoints-1 DO BEGIN
            nlon=lon(i)
            nlat=lat(i)
            data(0,nlat,nlon)=pr_e(i)
        ENDFOR
        print,mean(data)
    ENDELSE
;
; write the data to the netcdf file 'outfile'.
    IF (ndays GT 1) THEN BEGIN
        IF (DATAFORMAT EQ 0) THEN BEGIN
            print,createNCDF_v2(dims(0),dims(1),YEAR_START,1L*(YEAR_STOP)-1L*(YEAR_START)+1,month(1L*mon),ndays,data,outfile,header)
        ENDIF
    ENDIF ELSE BEGIN
        IF (DATAFORMAT EQ 0) THEN BEGIN
            print,createNCDF_v2_1ts(dims(0),dims(1),YEAR_START,1L*(YEAR_STOP)-1L*(YEAR_START)+1,month(1L*mon),ndays,data,outfile,header)
        ENDIF
    ENDELSE
;
;if desired, output also the compressed netcdf file in the OBS format.
    IF (DATAFORMAT EQ 1) THEN BEGIN
        print,create1dNCDF(NUMLANDPOINTS,nxx,nyy,navlon,navlat,YEAR_START,1L*(YEAR_STOP)-1L*(YEAR_START)+1,month(1L*mon),ndays,pr_e,outfileCompressed,header)
    ENDIF
;
end                             ; PROGRAM ENDS.
