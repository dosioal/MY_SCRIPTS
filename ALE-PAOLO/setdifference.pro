PRO SetDifference,a,b,value ; = a and (not b) = elements in A but not in B

mina = min(a, MAX=maxa)
minb = min(b, MAX=maxb)
if (minb gt maxa) or (maxb lt mina) then value= a ;No intersection...
r = where((histogram(a, MIN=mina, MAX=maxa) ne 0) and $
	          (histogram(b, MIN=mina, MAX=maxa) eq 0), count)
if count eq 0 then value= -1 else value= r + mina
end


