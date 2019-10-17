PRO prepareData, switcher, inFile, outFile, runToCorrect, varnm, year_1, year_2, month, numlp,land,multf,yearl
; switcher determines the file type of the original data,
; (0=compressed WFD; 1=latlon)
; inFile is file name of original file
; outFile is file name of output file
; varnm is the name of the variable to be read, e.g. 'aprl', 'temp2',
; 't2min'
; year_1 is the start year, year_2 is the stop year of the data to be
; read
; month is the month to select
; multf is an optional multfactor that can convert rain e.g. from mm/s to mm/d
; numl is the number of land points 
;
date1=year_1+'-01-01'
date2=year_2+'-12-31'
print,'PREPARE DATA FOR PERIOD: ',date1,' ',date2
print,'cdo seldate,'+date1+','+date2+' '+inFile+' dummy'
spawn,'cdo seldate,'+date1+','+date2+' '+inFile+' dummy'
;
; convert a compressed wfd file for the period year_1 to year_2 to 12
; idl saved files for the same period for all months of the year. get
; the original data from inFile and store the final data in outFile.
; dummy now has a chunk for data for the period from date1 to date2
IF (yearl EQ 365) THEN BEGIN
for mon=0,(n_elements(month)-1) do begin
    spawn,'cdo selmon,'+month(mon)+' dummy dummy1',result
    print,result
    outF=outFile+'_'+month(mon)+runToCorrect+'.dat'
    IF (switcher EQ 0) THEN BEGIN ; the file comes as a compressed netcdf
        print,ncdf2idl('dummy1',outF,1,varnm,multf,numlp,land)
    ENDIF
;
    IF (switcher EQ 1) THEN BEGIN ; the file comes as a lat lon netcdf
        print,ncdf2idl('dummy1',outF,0,varnm,multf,numlp,land)
    ENDIF
endfor
;ALE
ENDIF ELSE BEGIN
print,'360 DAY YEAR!'

spawn,'cdo splitsel,360 dummy dummy2_',result
print,result
ny=1L*year_2-1L*year_1

FOR yy=0,ny DO BEGIN
IF yy LT 10 THEN yyy='0'+STRTRIM(STRMID(yy,2),1) ELSE yyy=STRTRIM(STRMID(yy,2),1)
print,'ALE YEAR ',yyy
print,'cdo splitsel,30 dummy2_0000'+yyy+'.* dummy3_'+yyy+'_',result
spawn,'cdo splitsel,30 dummy2_0000'+yyy+'.nc dummy3_'+yyy+'_',result
spawn,'cdo splitsel,30 dummy2_0000'+yyy+'.nc2 dummy3_'+yyy+'_',result
print,result
ENDFOR ;year cycle

FOR mon=0,(n_elements(month)-1) DO BEGIN
print,'ALE MONTH ',mon
IF mon LT 10 THEN mmm='0'+STRTRIM(STRMID(mon,2),1) ELSE mmm=STRTRIM(STRMID(mon,2),1)
print,'cdo mergetime dummy3_*_0000'+mmm+'.* dummy4',result
spawn,'cdo mergetime dummy3_*_0000'+mmm+'.* dummy4',result
print,result
outF=outFile+'_'+month(mon)+runToCorrect+'.dat'
IF (switcher EQ 0) THEN BEGIN ; the file comes as a compressed netcdf
print,ncdf2idl('dummy4',outF,1,varnm,multf,numlp,land)
ENDIF
IF (switcher EQ 1) THEN BEGIN ; the file comes as a lat lon netcdf
print,ncdf2idl('dummy4',outF,0,varnm,multf,numlp,land)
ENDIF
spawn,'rm dummy4'
ENDFOR ;month cycle

ENDELSE
;ALE


spawn,'rm dummy'
spawn,'rm dummy1'
spawn,'rm dummy2*'
spawn,'rm dummy3*'
end
