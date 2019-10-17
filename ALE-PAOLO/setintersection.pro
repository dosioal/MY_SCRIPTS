PRO setintersection, a, b,value
;help,a,b
minab = min(a, MAX=maxa) > min(b, MAX=maxb) ;Only need intersection of ranges
maxab = maxa < maxb
 
 ;If either set is empty, or their ranges don't intersect: result = NULL.
if (maxab lt minab or maxab lt 0) then value= -1
 r = where((histogram(a, MIN=minab, MAX=maxab) ne 0) and  $
         (histogram(b, MIN=minab, MAX=maxab) ne 0), count)
if count eq 0 then value= -1 else value= r + minab
end

