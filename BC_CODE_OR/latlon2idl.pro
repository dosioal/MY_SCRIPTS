close,2
openw,2,'logfile.txt'
; pathdat is the general working directory
pathdat='/scratch/local1/users/m214106/claudio/'
;open the WFD land point file
id=NCDF_OPEN(pathdat+'WFD/WFD-land-lat-long-z.nc')
; read the data from the file
NCDF_VARGET,id,'land',land
NCDF_VARGET,id,'Latitude',lat
NCDF_VARGET,id,'Longitude',lon
NCDF_CLOSE,id
;
START='1960'
STOP='1970'
;
; specify the months to be bias corrected
month=['01','02','03','04','05','06','07','08','09','10','11','12']
type=['12523_E23', '12524_E24', '31009_S09']
;month=['12']
varname=aprl;'temp2' ; aprl
;multfactor=1.0
multfactor=86400
;
; the main loop over all model runs
for typ=0,(n_elements(type)-1) do begin
; the main loop over all months
for mon=0,(n_elements(month)-1) do begin
;
;*******************************************************************
; specifying the path to the model data.
pfile=pathdat+'modelData/modelData/PMERGED_LATLON'+START+'_'+STOP+'_'+month(mon)+'_'+type(typ)+'.nc'
; opening the model data.
id=NCDF_OPEN(pfile)
; the variable 'aprl' is read from the file specified and stored in
; the idl variable dataarray
NCDF_VARGET,id,varname,dataarray

; closing the read socket.
NCDF_CLOSE,id
; l is the number of timesteps that was read
l=n_elements(dataarray(0,0,*))
; JAN and DEC have 1 months each, therefore the actual number of days
; is specified manually.
;; if (mon eq 0 or mon eq 11) then l=310
; make a new 1d idl array that is compatible with the structure of the WFD.
outdataarray=fltarr(67420,l)
; the loop transfers the land points of the 2d array to the 1d array,
; making its structure compatible with the WFD.
for i=0,l-1 do begin
   dum=reform(dataarray(*,*,i))
   outdataarray(*,i)=dum(land-1)*multfactor
endfor

dataarray=0
; model data has now been read and the 2d array is set to zero.
print,'done restoring '+START+' to '+STOP+' model only';******************
;*******************************************************************
save,filename=pathdat+'modelData/'+varname+'_'+START+'_'+STOP+'_'+month(mon)+'_'+type(typ)+'.dat',$
              outdataarray
;
endfor
;
endfor
;
close,2
end
