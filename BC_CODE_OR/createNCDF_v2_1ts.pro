;;THIS IDL FUNCTION PRODUCES A NETCDF FILE WITH A REGULAR LATLON GRID 
;;FROM A GIVEN ARRAY OF MONTHLY PRECIPITATION VALUES OVER 10 YEARS
;;
;; FILENAME: createNCDF_v2.pro
;; AUTHORS:  C. PIANI AND J. O. HAERTER
;; DATE:     SEPTEMBER 30, 2009
;; PROJECT:  EU WATCH PROJECT
;;
;; ___________________________________________________________
;;
;
;
; nx (ny) - number of grid points in x (y) direction
; yr      - starting year
; nyear   - total number of years to be written
; mn      - the month to be written
; nt      - total number of days for the given month (i.e. 310 for
;           10 x january)
; data    - the 3d idl array containing the timeseries of the field
; fname   - the file name to be written to
function createNCDF_v2_1ts,nx,ny,yr,nyear,mn,nt,data,fname,header
print,'nx = ',nx,', ny = ',ny, ', nt = ', nt
print,'yr = ',yr,', nyear = ',nyear,', mn = ',mn
print,mean(data)
;
; Create a new NetCDF file with the filename fname  
id = NCDF_CREATE(fname, /CLOBBER)  
;
; Fill the file with default values  
NCDF_CONTROL, id, /FILL  

; Define the years for the data at hand and make an array for the
; hours to be output to the netcdf file.
years = yr+indgen(nyear)
hours = lonarr(nt)
ind   = 0 
;
; assign the time values.
y=0
hours(0) = (yr+y)*10000L+mn*100+1
;
xid = NCDF_DIMDEF(id, 'x', nx)         ; Make dimensions.  
yid = NCDF_DIMDEF(id, 'y', ny)         ; Make dimensions.  
zid = NCDF_DIMDEF(id, 'time', /UNLIMITED) ; The time dimension is open-end. 
;
; Define variables.
hid = NCDF_VARDEF(id, 'time', [zid], /LONG)  
vid = NCDF_VARDEF(id, header(0), [xid,yid,zid], /FLOAT)  
NCDF_ATTPUT, id, vid, 'units', header(1)
NCDF_ATTPUT, id, vid, 'long_name', header(2)  
NCDF_ATTPUT, id, hid, 'long_name', 'date'  
NCDF_ATTPUT, id, /GLOBAL, 'Title', header(3)  
;
; Put file in data mode:  
NCDF_CONTROL, id, /ENDEF  
; Input data:  
NCDF_VARPUT, id, hid, hours   
it=0
;
FOR iy=0,ny-1 DO BEGIN    
    NCDF_VARPUT, id,  vid, $  
      REFORM(data(it,iy,*)), OFFSET=[0,iy,it]  
ENDFOR
;
NCDF_CLOSE, id ; Close the NetCDF file.  
end
