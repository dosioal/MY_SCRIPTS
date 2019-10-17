pro make_mask_ascii_CRU  ;infile,loopend

; some settings
DEVICE,decompose=0

;************************************************************
ekenn='CRU'

;READ_CONSTANT_FIELDS
	c_file='/media/ext_disk/DATA/CRU/cru_ts_2.10/grid/halfdeg.elv' ;ens044ns044
	pollat =  39.25
	pollon = -162.0
iLUN=10

; open datafile for reading (with error handling)
OPENR,iLUN,c_file,ERROR=lerror,/GET_LUN

 ; skip header information, then read relevant grid information
FOR ii=0,2 DO READF,iLUN,FORMAT='(1x)'
READF,iLUN,FORMAT='(6x,F7.2,1x,F7.2,8x,F7.2,1x,F7.2,12x,I4,1x,I4)',$
	        rLonMin,rLonMax,rLatMin,rLatMax,tnx,tny
READF,iLUN,FORMAT='(42x,F10.4,15x)',rMulti

; calculate indices of required area with respect to the CRU grid.
rDXGrid = (rLonMax - rLonMin) / FLOAT(tnx)
rDYGrid = (rLatMax - rLatMin) / FLOAT(tny)

; create arrays which will hold the data values
lat=MAKE_ARRAY(tnx,tny,/FLOAT)
lon=MAKE_ARRAY(tnx,tny,/FLOAT)
hsurf=MAKE_ARRAY(tnx,tny,/LONG)

; read dataset
WHILE (NOT EOF(iLUN)) DO BEGIN
 READF,iLUN,FORMAT='(9x,I4,1x,I4)',iGridX,iGridY
 READF,iLUN,FORMAT='(I5)',iHeight
	    hsurf[iGridX-1,iGridY-1]=iHeight
ENDWHILE
CLOSE,iLUN & FREE_LUN,iLUN


; create lat/lon information arbitrarily
rDXGrid = (rLonMax - rLonMin) / FLOAT(tnx)
rDYGrid = (rLatMax - rLatMin) / FLOAT(tny)
;latVec=FLTARR(iYDiff)
;lonVec=FLTARR(iXDiff)
latVec=FLTARR(tny)
lonVec=FLTARR(tnx)

; calculate lat/lons in a general way
FOR ii=0,tnx-1 DO lonVec[ii]=rLonMin+0.5+FLOAT(ii)*rDXGrid
FOR jj=0,tny-1 DO latVec[jj]=rLatMin+0.5+FLOAT(jj)*rDYGrid

FOR ii=0,tnx-1 DO lat[ii,*]=latVec[*]
FOR jj=0,tny-1 DO lon[*,jj]=lonVec[*]


print,'DIMENSIONS: nx,ny,nt'
print,tnx,tny
help,lon
help,lat
help,hsurf
help,fr_land
help,soiltyp

;READ_VARIABLE_FIELDS
iEndYear=1980
iStartYear=1971
iDataEnd=2002
sStartYear = STRING(iStartYear, FORMAT='(I04)')
sEndYear   = STRING(iEndYear, FORMAT='(I04)')
	t_file='/media/ext_disk/DATA/CRU/cru_ts_2.10/data_dec/cru_ts_2_10.'+sStartYear+'-'+sEndYear+'.pre' ;ens044ns044
	p_file='/media/ext_disk/DATA/CRU/cru_ts_2.10/data_dec/cru_ts_2_10.'+sStartYear+'-'+sEndYear+'.pre' ;ens044ns044

; compute the number of months during the period in question
iMonths = ( ( iEndYear - iStartYear ) + 1 ) * 12

iYearDiff  = iEndYear - iStartYear
rgiStartYear = INTARR(4) & rgiEndYear = INTARR(4)
FOR ii=0,3 DO rgiStartYear[ii]=FIX(STRMID(sStartYear,ii,1))
FOR ii=0,3 DO rgiEndYear[ii]=FIX(STRMID(sEndYear,ii,1))
rgiStartYear[3]=1
IF rgiEndYear[3] NE 0 THEN BEGIN
          rgiEndYear[3]=0
          rgiEndYear[2]++
      IF rgiEndYear[2] GT 9 THEN BEGIN
          rgiEndYear[1]++
         rgiEndYear[2]=0
       ENDIF
       IF rgiEndYear[1] GT 9 THEN BEGIN
         rgiEndYear[0]++
       rgiEndYear[1]=0
      ENDIF
ENDIF
sStartYear = STRJOIN(STRING(rgiStartYear[*],FORMAT='(I1)'))
sEndYear   = STRJOIN(STRING(rgiEndYear[*],FORMAT='(I1)'))
print,sStartYear
print,sEndYear


OPENR,iLUN,t_file,ERROR=lerror,/GET_LUN
; skip header information, then read relevant grid information
FOR ii=0,2 DO READF,iLUN,FORMAT='(1x)'
READF,iLUN,FORMAT='(6x,F7.2,1x,F7.2,8x,F7.2,1x,F7.2,12x,I4,1x,I4)',$
	    rLonMin,rLonMax,rLatMin,rLatMax,iNX,iNY
READF,iLUN,FORMAT='(42x,F10.4,15x)',rMulti
; create arrays which will hold the data values
ArrDummy=MAKE_ARRAY(iNX,iNY,iMonths,/LONG)
ArrDummy[*,*,*]=-999
rgiMonth=MAKE_ARRAY(12,/LONG)

; number of lines to skip before/after data of interest
	    iSkipLinesBefore = iStartYear - 1901
	    iSkipLinesAfter  = iDataEnd - iEndYear
	    print,iSkipLinesBefore,iSkipLinesAfter
		        iSkipLinesBefore = iStartYear - iDataEnd + 1
			    iSkipLinesAfter  = iDataEnd - iEndYear
	    print,iSkipLinesBefore,iSkipLinesAfter
			        iSkipLinesBefore = iStartYear - FIX(sStartYear)
				    iSkipLinesAfter  = FIX(sEndYear) - iEndYear
	    print,iSkipLinesBefore,iSkipLinesAfter



WHILE (NOT EOF(iLUN)) DO BEGIN
    READF,iLUN,FORMAT='(9x,I4,1x,I4)',iGridX,iGridY
    IF iSkipLinesBefore NE 0 THEN BEGIN
	        FOR yy=0,iSkipLinesBefore-1 DO BEGIN
		                READF,iLUN,FORMAT='(1x)'
			        ENDFOR
			    ENDIF
		        FOR yy=1,iEndYear-iStartYear+1 DO BEGIN
		            READF,iLUN,FORMAT='(12I5)',rgiMonth
	            ArrDummy[iGridX-1,iGridY-1,(yy-1)*12:yy*12-1]=rgiMonth[0:11]
    ENDFOR
    IF iSkipLinesAfter NE 0 THEN BEGIN
	        FOR yy=0,iSkipLinesAfter-1 DO BEGIN
		                READF,iLUN,FORMAT='(1x)'
			        ENDFOR
			    ENDIF
		    ENDWHILE
	CLOSE,iLUN
FREE_LUN,iLUN

help,rLonMin,rLonMax,rLatMin,rLatMax,iNX,iNY
help,Arrdummy
STOP

;*******************
; SETTING PLOTS
;*******************
DEVICE,decompose=0
loadct,39
!p.background=255
!p.color=0
nlevels=11
;limits=[lat(0,0),lon(0,0),lat(0,tny-1),lon(0,tny-1),lat(tnx-1,tny-1),lon(tnx-1,tny-1),lat(tnx-1,0),lon(tnx-1,0)]
limits=[30,-10,75,-10,75,65,30,65]

xsize=750.
ysize=550.

;************************
;MASKS
;************************
;    W,E,S,N
BI=[-10,2,50,59]
IP=[-10,3,36,44]
FR=[-5,5,44,50]
ME=[2,16,48,55]
SC=[5,30,55,75]
AL=[5,15,44,48]
MD=[3,25,36,44]
EA=[16,30,44,55]
EU=[-15,60,35,75]
;************************
;DOMAIN
;************************
window,0,retain=2,xsize=xsize,ysize=ysize
!p.multi=0
levels=[-100,0,5,50,200,500,750,1000,1250,1500,2000,2599,3000,4500]
colors=90+findgen(n_elements(levels))*(255-90)/n_elements(levels)
colors=[100,100,130,140,160,170,180,190,200,210,220,230,240,250,255]

;MAP_SET, /LAMBERT, pollat, pollon+180,0,  /GRID, LATDEL=10, LONDEL=10, /CONTINENTS, /HORIZON,limit=limits, $
MAP_SET, /LAMBERT,  /GRID, LATDEL=10, LONDEL=10, /CONTINENTS, /HORIZON,limit=limits, $
E_CONT={COUNTRIES:0, COASTS:1},title='DOMAIN',/isotropic,/noerase,/advance,position=[0,0.2,0.98,0.95]
contour,hsurf,levels=levels,c_colors=colors,lon,lat,/fill,xstyle=1,ystyle=1,/overplot
map_grid,color=0
map_continents,/coasts,color=0
;BI
plots,BI(0),BI(2)
plots,BI(0),BI(3),/continue
plots,BI(1),BI(3),/continue
plots,BI(1),BI(2),/continue
plots,BI(0),BI(2),/continue
;IP
plots,IP(0),IP(2)
plots,IP(0),IP(3),/continue
plots,IP(1),IP(3),/continue
plots,IP(1),IP(2),/continue
plots,IP(0),IP(2),/continue
;FR
plots,FR(0),FR(2)
plots,FR(0),FR(3),/continue
plots,FR(1),FR(3),/continue
plots,FR(1),FR(2),/continue
plots,FR(0),FR(2),/continue
;ME
plots,ME(0),ME(2)
plots,ME(0),ME(3),/continue
plots,ME(1),ME(3),/continue
plots,ME(1),ME(2),/continue
plots,ME(0),ME(2),/continue
;SC
plots,SC(0),SC(2)
plots,SC(0),SC(3),/continue
plots,SC(1),SC(3),/continue
plots,SC(1),SC(2),/continue
plots,SC(0),SC(2),/continue
;AL
plots,AL(0),AL(2)
plots,AL(0),AL(3),/continue
plots,AL(1),AL(3),/continue
plots,AL(1),AL(2),/continue
plots,AL(0),AL(2),/continue
;MD
plots,MD(0),MD(2)
plots,MD(0),MD(3),/continue
plots,MD(1),MD(3),/continue
plots,MD(1),MD(2),/continue
plots,MD(0),MD(2),/continue
;EA
plots,EA(0),EA(2)
plots,EA(0),EA(3),/continue
plots,EA(1),EA(3),/continue
plots,EA(1),EA(2),/continue
plots,EA(0),EA(2),/continue

text=['Alt (m)', 'm']
lposition=[0.1,0.9,0.10,0.15]
;call_procedure,'t_downlegend',text,levels,colors,lposition

;*********************
; AVERAGES OVER MASK
;*********************
MASK:
mask=''
print,'MASK? (BI,IP,FR,ME,SC,AL,MD,EA EU)'
read,mask

ma=fltarr(4)
IF mask eq 'BI' then ma=BI
IF mask eq 'IP' then ma=IP
IF mask eq 'FR' then ma=FR
IF mask eq 'ME' then ma=ME
IF mask eq 'SC' then ma=SC
IF mask eq 'AL' then ma=AL
IF mask eq 'MD' then ma=MD
IF mask eq 'EA' then ma=EA
IF mask eq 'EU' then ma=EU

field_mask=fltarr(tnx,tny)
field_mask(*,*)=0
FOR i=0,tnx-1 do begin
for j=0,tny-1 do begin
count=0.
if ( ma(0) LE lon(i,j) AND lon(i,j) LE ma(1) AND ma(2) LE lat(i,j) AND lat(i,j) LE ma(3)) THEN BEGIN
;if fr_land(i,j) GT 0.5 then begin
if hsurf(i,j) GT 0 then begin
field_mask(i,j)=1
count=count+1.
endif
endif
endfor
endfor
help,count

window,4,retain=2
!p.multi=0
contour,field_mask,title=mask,xstyle=1,ystyle=1,/fill


choice=''
PRINT,'WRITE TO FILE? (y/n)'
read, choice
IF choice eq 'n' THEN GOTO,GOON

close,10
OPENW,10,ekenn+'_'+mask+'.txt'
for iy=0,tny-1 do begin
for ix=0,tnx-1 do begin
PRINTF,10,lat(ix,iy),lon(ix,iy),field_mask(ix,iy)
endfor
endfor
close,10

; ------------------------------------------------------------------
;   - - - : : : # # # write subregions to nc file # # # : : : - - -
; ------------------------------------------------------------------

rgiNumOfRegions=field_mask[UNIQ(field_mask,SORT(field_mask))]
print,rgiNumOfRegions 
rgiX=indgen(tnx)
rgiY=indgen(tny)
;STOP
; before we start writing the variables to disk let's check if there
; have been set some of the nc_* tags.

   ; write the results to the defined output file
sSubregionsFile = ekenn+'_'+mask+'.nc'
print,sSubregionsFile
	 iNcId  = NCDF_CREATE(sSubregionsFile,/CLOBBER)

	 ; define every dimension and variable needed for the subregions file
	 ; and write it to the netcdf file
	 iLonId = NCDF_DIMDEF(iNcId,'x',tnx)
	 iLatId = NCDF_DIMDEF(iNcId,'y',tny)
	 iNumId = NCDF_DIMDEF(iNcId,'n',N_ELEMENTS(rgiNumOfRegions))
	 print,inumId

	 ; put global attributes in the file
	 NCDF_ATTPUT, iNcId, /GLOBAL, 'title',mask

; first variables are the x and y vector
iVarID = NCDF_VARDEF(iNcId,'x',[iLonId],/FLOAT)
NCDF_ATTPUT, iNcId, iVarId, 'long_name', 'x coordinate', /CHAR
NCDF_ATTPUT, iNcId, iVarId, 'units', '1', /CHAR
NCDF_ATTPUT, iNcId, iVarId, 'comment', $
	             'dimensionless counter for gridpoints in west-east direction', /CHAR
NCDF_CONTROL, iNcId, /ENDEF
NCDF_VARPUT,  iNcId, iVarId, rgiX
NCDF_CONTROL, iNcId, /REDEF

iVarID = NCDF_VARDEF(iNcId,'y',[iLatId],/FLOAT)
NCDF_ATTPUT, iNcId, iVarId, 'long_name', 'y coordinate', /CHAR
NCDF_ATTPUT, iNcId, iVarId, 'units', '1', /CHAR
NCDF_ATTPUT, iNcId, iVarId, 'comment', $
                 'dimensionless counter for gridpoints in north-south direction', /CHAR
NCDF_CONTROL, iNcId, /ENDEF
NCDF_VARPUT,  iNcId, iVarId, rgiY
NCDF_CONTROL, iNcId, /REDEF

; now comes the mask itself
iVarID = NCDF_VARDEF(iNcId,'mask',[iLonId,iLatId],/LONG)
NCDF_ATTPUT, iNcId, iVarId, 'long_name', 'subregion mask index', /CHAR
NCDF_ATTPUT, iNcId, iVarId, 'units', '1', /CHAR
NCDF_ATTPUT, iNcId, iVarId, 'grid_mapping','x y',/CHAR
NCDF_CONTROL, iNcId, /ENDEF
NCDF_VARPUT,  iNcId, iVarId,field_mask 
NCDF_CONTROL, iNcId, /REDEF
; then the meta info
iVarID = NCDF_VARDEF(iNcId,'meta',iNumId,/LONG)
NCDF_ATTPUT, iNcId, iVarId, 'long_name', 'subregion index', /CHAR
NCDF_ATTPUT, iNcId, iVarId, $
	             'comment', '0 = region to be disregarded in evaluation ' + $
	           STRING(10B) + '1 .. n subregions to be evaluated'
NCDF_ATTPUT, iNcId, iVarId, 'units', '1', /CHAR
NCDF_CONTROL, iNcId, /ENDEF
NCDF_VARPUT,  iNcId, iVarId, rgiNumOfRegions

NCDF_CLOSE, iNcId

GOON:

choice=''
PRINT,'ANOTHER MASK? (y n)'
read,choice
IF choice eq 'y' THEN GOTO,MASK


STOP


; End procedure.
END
