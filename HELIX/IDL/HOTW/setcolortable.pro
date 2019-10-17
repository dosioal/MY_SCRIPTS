PRO setcolortable,iColTab

    ;these tables should only be used in PLOTS with the same number of contours as
    ;colors in the color table!
    CASE iColTab OF
        41: BEGIN
                        ;-- 18 step Dark Red to Blue (diverging)
            ColTbl=FltArr(18,3)
            ColTbl[*,0]=[0.142,0.097,0.160,0.240,0.340,0.460,0.600,0.740,0.950,1.000, $
                         1.000,1.000,1.000,1.000,1.000,0.920,0.850,0.650]*255.
            ColTbl[*,1]=[0.000,0.112,0.342,0.531,0.692,0.829,0.920,0.978,1.000,1.000, $
                         0.948,0.840,0.676,0.472,0.240,0.155,0.085,0.000]*255.
            ColTbl[*,2]=[0.850,0.970,1.000,1.000,1.000,1.000,1.000,1.000,1.000,0.950, $
                         0.740,0.600,0.460,0.340,0.240,0.210,0.187,0.130]*255.
        END
        42: BEGIN
                        ;-- 9 step Dark Red to White
            ColTbl=FltArr(9,3)
            ColTbl[*,0]=[1.0,1.000,1.000,1.000,1.000,1.000,0.970,0.850,0.650]*255.
            ColTbl[*,1]=[1.0,1.000,0.948,0.840,0.676,0.472,0.155,0.085,0.000]*255.
            ColTbl[*,2]=[1.0,0.920,0.740,0.600,0.460,0.340,0.210,0.187,0.130]*255.
        END
        43: BEGIN
                        ;-- 18 step Blue to Dark Orange (diverging)
            ColTbl=FltArr(18,3)
            ColTbl[*,0]=[0.000,0.000,0.000,0.000,0.200,0.400,0.600,0.700,0.910,1.000, $
                         1.000,1.000,1.000,1.000,1.000,0.800,0.600,0.400]*255.
            ColTbl[*,1]=[0.300,0.400,0.500,0.600,0.700,0.800,0.950,1.000,1.000,0.980, $
                         0.900,0.793,0.680,0.560,0.433,0.333,0.240,0.153]*255.
            ColTbl[*,2]=[0.300,0.400,0.500,0.600,0.700,0.800,0.950,1.000,1.000,0.930, $
                         0.800,0.600,0.400,0.200,0.000,0.000,0.000,0.000]*255.
        END
        44: BEGIN
                        ;-- 11 step Dark Red to Dark Blue (white centered)
            ColTbl=FltArr(11,3)
            ColTbl[*,0]=[190,203,217,232,246,255,218,163,109, 55,  0]
            ColTbl[*,1]=[  0, 54,108,163,218,255,218,163,109, 55,  0]
            ColTbl[*,2]=[  0, 54,108,163,218,255,255,255,255,255,255]
        END
        45: BEGIN
                        ;-- 11 step black to black (white centered)
            ColTbl=FltArr(11,3)
            ColTbl[*,0]=[  0, 51,102,153,204,255,204,153,102, 51,  0]
            ColTbl[*,1]=[  0, 51,102,153,204,255,204,153,102, 51,  0]
            ColTbl[*,2]=[  0, 51,102,153,204,255,204,153,102, 51,  0]
        END
        46: BEGIN
                        ;-- 16 step white to brown (over red)
            ColTbl=FltArr(16,3)
            ColTbl[*,0]=[255,249,242,236,231,225,218,212,$
                         206,200,194,181,160,142,122,103]
            ColTbl[*,1]=[255,231,206,182,159,135,110, 86,$
                          62, 38, 14,  7, 17, 29, 40, 52]
            ColTbl[*,2]=[255,231,206,182,159,135,110, 86,$
                          62, 38, 14,  7, 17, 29, 40, 52]
        END
        47: BEGIN
                        ;-- 21 step dark blue to dark red (white centered)
            ColTbl=FltArr(21,3)
            ColTbl[*,0]=[103,123,142,164,184,202,216,228,239,250,255,$
                         237,191,143, 98, 56, 41, 32, 21, 11,  1]
            ColTbl[*,1]=[ 52, 52, 51, 52, 51, 56, 98,144,191,236,255,$
                         241,207,171,137,104, 82, 62, 41, 21,  1]
            ColTbl[*,2]=[ 52, 41, 30, 21, 10,  8, 58,116,174,231,255,$
                         246,223,199,177,155,143,133,123,113,103]
        END
        48: BEGIN
                        ;-- 20 step blue to red (double white centered)
            ColTbl=FltArr(20,3)
            ColTbl[*,0]=[ 36, 24, 40, 61, 86,117,153,188,234,255,$
                         255,255,255,255,255,255,255,247,216,165]
            ColTbl[*,1]=[  0, 28, 87,135,176,211,234,249,255,255,$
                         255,255,241,214,172,120, 61, 39, 21,  0]
            ColTbl[*,2]=[216,247,255,255,255,255,255,255,255,255,$
                         255,234,188,153,117, 86, 61, 53, 47, 33]
        END
        51: BEGIN
                        ;-- 10 step white to blue (for precipitation)
            ColTbl=FltArr(10,3)
            ColTbl[*,0]=[255,229,203,177,151,123, 98, 71, 46, 19]
            ColTbl[*,1]=[255,236,217,197,178,158,140,119,100, 81]
            ColTbl[*,2]=[255,244,232,220,209,196,186,173,163,150]
        END
        52: BEGIN
                        ;-- 15 step white to blue (for precipitation)
            ColTbl=FltArr(15,3)
            ColTbl[*,0]=[255,238,221,205,188,171,154,137,121,104, 86, 70, 53, 36, 20]
            ColTbl[*,1]=[255,242,230,218,206,193,181,169,156,144,130,119,106, 94, 82]
            ColTbl[*,2]=[255,247,239,233,226,218,211,203,196,188,179,173,165,158,151]
        END
        53: BEGIN
                        ;-- 10 step white to purple (for precipitation)
            ColTbl=FltArr(10,3)
            ColTbl[*,0]=[255,217,178,138,100, 61, 23, 18, 60,102]
            ColTbl[*,1]=[255,237,218,198,181,162,143,108, 54,  0]
            ColTbl[*,2]=[255,255,255,255,255,255,255,227,164,102]
        END
        54: BEGIN
                        ;-- 15 step white to purple (for precipitation)
            ColTbl=FltArr(15,3)
            ColTbl[*,0]=[255,230,203,181,156,131,106, 80, 56, 30,  8, 22, 49, 76,102]
            ColTbl[*,1]=[255,242,230,219,207,195,183,170,159,146,134,105, 70, 35,  0]
            ColTbl[*,2]=[255,255,255,255,255,255,255,255,255,255,254,224,183,143,102]
        END
        55: BEGIN
                        ;-- 10 step white over greenish blue to blue to
                        ;   purple)
            ColTbl=FltArr(10,3)
            ColTbl[*,0]=[255,126,170,208,165,120, 74, 33, 56,102]
            ColTbl[*,1]=[255,173,207,216,191,166,140,113, 61,  0]
            ColTbl[*,2]=[255,173,211,235,234,232,231,226,172,102]
        END
        60: BEGIN
                        ;-- 14 distinct colours (intended for use with
                        ;   taylor plots)
            ColTbl=FltArr(14,3)
            ColTbl[*,0]=[255,255,  0,  0,  0,255,127,127,  0,  0,  0,127,  0,127]
            ColTbl[*,1]=[  0,  0,  0,255,255,255,  0,  0,  0,127,127,127,  0,127]
            ColTbl[*,2]=[  0,255,255,255,  0,  0,  0,127,127,127,  0,  0,  0,127]
        END
        61: BEGIN
                        ;-- 21 distinct colours (intended for use with
                        ;   taylor plots)
            ColTbl=FltArr(21,3)
            ColTbl[*,0]=[ 85,170,255,  0,  0,  0,  0,  0,  0, 85,  0, 85,$
                         170,  0,170,255,  0,255,  0, 85,170]
            ColTbl[*,1]=[  0,  0,  0, 85,170,255,  0,  0,  0, 85, 85,  0,$
                         170,170,  0,255,255,  0,  0, 85,170]
            ColTbl[*,2]=[  0,  0,  0,  0,  0,  0, 85,170,255,  0, 85, 85,$
                           0,170,170,  0,255,255,  0, 85,170]
        END
        62: BEGIN
                        ;-- 20 step Dark Red to Blue (diverging) +Black and white
            ColTbl=FltArr(20,3)
            ColTbl[*,0]=[1.000,0.000,0.000,0.000,0.000,0.200,0.400,0.600,0.700,0.910,1.000, $
                         1.000,1.000,1.000,1.000,1.000,0.800,0.600,0.400,0.000]*255.
            ColTbl[*,1]=[1.000,0.300,0.400,0.500,0.600,0.700,0.800,0.950,1.000,1.000,0.980, $
                         0.900,0.793,0.680,0.560,0.433,0.333,0.240,0.153,0.000]*255.
            ColTbl[*,2]=[1.000,0.300,0.400,0.500,0.600,0.700,0.800,0.950,1.000,1.000,0.930, $
                         0.800,0.600,0.400,0.200,0.000,0.000,0.000,0.000,0.000]*255.
        END
    ENDCASE

    ColTbl=CONGRID(ColTbl,256,3)
    TVLCT, ColTbl

;    iNColors = N_ELEMENTS(ColTbl[*,0])
;    print,iNColors

        PRINT,  'Custom color table successfully set up with '; + $
;               STRTRIM(STRING(iNColors,FORMAT='(I2)'),2) + ' colors.'


END

