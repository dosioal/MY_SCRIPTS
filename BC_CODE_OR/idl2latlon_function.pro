;;THIS IDL FILE CONVERTS A BIAS CORRECTED IDL SAVED FILE TO A LATLON
;;NETCDF FILE.
;;
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
function idl2latlon_function,standard,DATAFORMAT,infile,outfile,YEAR_START,YEAR_STOP,mon,lat,lon,month
;    
    restore,infile
;
    print,'done restoring '+ infile
;
   if (standard eq 1) then pr_e=idldata
   if (standard eq 0 )then begin
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
;   header=['precip','mm/s','total precipitation','model
;   precipitation']
   header=['variable','unit','long variable','title']
    l=n_elements(pr_e(0,*))
;
    outfileCompressed='COMPRESSED_'+outfile
;
    print,'done restoring idl data: '+infile
;******************************************************************
; the total number of days to be written to netcdf format.
    ndays=n_elements(pr_e(1,*))  
    
; The data array required for passing the data to the createNCDF subroutine.
    data = FLTARR(ndays,360,720)
    
; total number of land points.
    nlandpoints=n_elements(pr_e(*,1))
    
; some conversion to lat-lon coordinates
    lon0=-179.75
    lat0=89.75
    FOR i=0L,nlandpoints-1 DO BEGIN
        nlon=(lon(i)-lon0)/0.5 
        nlat=(lat0-lat(i))/0.5
        FOR t=0,ndays-1 DO BEGIN
            data(t,nlat,nlon)=pr_e(i,t)
        ENDFOR
    ENDFOR
;
; write the data to the netcdf file 'outfile'.
    IF (DATAFORMAT EQ 0) THEN BEGIN
        print,createNCDF_v2(720,360,YEAR_START,1L*(YEAR_STOP)-1L*(YEAR_START)+1,month(1L*mon),ndays,data,outfile,header)
    ENDIF
;
;if desired, output also the compressed netcdf file in the WFD format.
    IF (DATAFORMAT EQ 1) THEN BEGIN
        print,create1dNCDF(NUMLANDPOINTS,nxx,nyy,navlon,navlat,YEAR_START,1L*(YEAR_STOP)-1L*(YEAR_START)+1,month(1L*mon),ndays,pr_e,outfileCompressed,header)
    ENDIF
;
end                             ; PROGRAM ENDS.
