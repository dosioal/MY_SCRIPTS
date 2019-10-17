FUNCTION convert_org_data_function, outFile, year1, year2, month, lat, lon, convert, rtc, header, dims

for mon=0,(n_elements(month)-1) do begin    
    inF=outFile+'_'+month(mon)+rtc+'.dat'
    outF=outFile+'_'+month(mon)+rtc+'.nc'
    outF_timmean=outFile+'_'+month(mon)+rtc+'_timmean.nc'
;
    if (convert EQ 1) THEN BEGIN
        print,idl2latlon_function_v1(1,0,inF,outF,year1,year2,mon,lat,lon,month,header,dims)
        spawn,'cdo timmean '+outF+' '+outF_timmean
    ENDIF
;
endfor
end
   
