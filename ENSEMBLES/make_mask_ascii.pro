pro make_mask_ascii  ;infile,loopend

; some settings
DEVICE,decompose=0

;************************************************************
ekenn='Europe_044'

;READ_CONSTANT_FIELDS
	c_file='/media/ext_disk2/CLM/SIMUL/ens044/data/test.nc' ;ens044ns044
	pollat =  39.25
	pollon = -162.0

ncid = NCDF_OPEN(strcompress(c_file, /remove_all))
; Retrieve general information about this netCDF file.
ncidinfo = NCDF_INQUIRE(ncid)	;ndims,nvars,ngatts,recdim
idname = STRARR(ncidinfo.ndims)
idsize = INTARR(ncidinfo.ndims)
; information about dimensions
for i=0, ncidinfo.ndims-1 do begin
 NCDF_DIMINQ,ncid,i,name,size		;names, size
 idname(i) = name
 idsize(i) = size
endfor
; Place the desired variables in local arrays.
varnames = STRARR(ncidinfo.Nvars)
for i=0, ncidinfo.Nvars-1 do begin 
   vardata = NCDF_VARINQ(ncid, i)	;name,datatype,ndims,natts,dim
   varnames(i) = vardata.Name
endfor

	 tnx = 1
 	 tny = 1
	 tnz = 1
FOR i=0, ncidinfo.Nvars-1 DO BEGIN 
   vardata = NCDF_VARINQ(ncid, i)	;name,datatype,ndims,natts
     varndims = vardata.ndims
     vardims  = vardata.dim
     FOR j=0,varndims-1 DO BEGIN
      FOR jj=0,ncidinfo.ndims-1 DO BEGIN
	IF (vardims[j] EQ jj) THEN BEGIN
	 print,idname(jj),jj,idsize(jj)
   	 CASE (idname(jj)) OF
	    "rlon" : BEGIN
			tnx    = idsize(jj)
			xcname = idname(jj)
			print,tnx
		     END
	    "rlat" : BEGIN
			tny    = idsize(jj)
			ycname = idname(jj)
			print,tny
		     END
	    "time" : BEGIN
			tnt    = idsize(jj)
			tcname = idname(jj)
		     END
	    ELSE   : BEGIN
			tnz    = idsize(jj)
			zcname = idname(jj)
		     END
	  END
         ENDIF
      ENDFOR
     ENDFOR
; Read data
     tmpfield=fltarr(tnx,tny)
    NCDF_VARGET, ncid, vardata.Name, tmpfield
    IF (vardata.Name EQ 'lon') THEN lon=tmpfield
    IF (vardata.Name EQ 'lat') THEN lat=tmpfield
    IF (vardata.Name EQ 'xlon') THEN xlon=tmpfield
    IF (vardata.Name EQ 'ylat') THEN ylat=tmpfield
    IF (vardata.Name EQ 'rlon') THEN rlon=tmpfield
    IF (vardata.Name EQ 'rlat') THEN rlat=tmpfield
    IF (vardata.Name EQ 'HSURF') THEN HSURF=tmpfield
    IF (vardata.Name EQ 'FR_LAND') THEN FR_LAND=tmpfield
    IF (vardata.Name EQ 'SOILTYP') THEN SOILTYP=tmpfield
    IF (vardata.Name EQ 'txlon') THEN txlon=tmpfield
    IF (vardata.Name EQ 'tylat') THEN tylat=tmpfield
    IF (vardata.Name EQ 'rotated_pole') THEN rotated_pole=tmpfield
ENDFOR

NCDF_CLOSE, ncid

print,'DIMENSIONS: nx,ny,nt'
print,tnx,tny,tnt
help,lon
help,xlon
help,rlon
help,rotated_pole
help,hsurf
help,fr_land
help,soiltyp

;*******************
; SETTING PLOTS
;*******************
DEVICE,decompose=0
loadct,39
!p.background=255
!p.color=0
nlevels=11
limits=[lat(0,0),lon(0,0),lat(0,tny-1),lon(0,tny-1),lat(tnx-1,tny-1),lon(tnx-1,tny-1),lat(tnx-1,0),lon(tnx-1,0)]

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

MAP_SET, /LAMBERT, pollat, pollon+180,0,  /GRID, LATDEL=10, LONDEL=10, /CONTINENTS, /HORIZON,limit=limits, $
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
if fr_land(i,j) GT 0.5 then begin
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
OPENW,10,mask+'.txt'
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
