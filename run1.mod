;; 1. Based on: 
;; 2. Description: 1-comp oral, linear elim
;; 3. Label:
;; x1. Author:

$PROBLEM PK

$INPUT ID TIME DV AMT CMT MDV EVID

$DATA nm_001.csv ;IGNORE=@

$SUBROUTINES ADVAN2 TRANS2

$PK
CL = THETA(1) * EXP(ETA(1))
V  = THETA(2) * EXP(ETA(2))
KA = THETA(3) * EXP(ETA(3))
S1 = V

$ERROR
IPRED = F
    W = SQRT(THETA(4)**2*IPRED**2 + THETA(5)**2)
    Y = IPRED + W*EPS(1)
 IRES = DV-IPRED
IWRES = IRES/W

$THETA
(0.1) ; CL
(0.1) ; V
(0.1) ; KA
(0, .1) ; Prop.RE (sd)
(0, 1)  ; Add.RE (sd)

$OMEGA
(0.1) ; IIV CL
(0.1) ; IIV V
(0.1) ; IIV KA

$SIGMA
1 FIX ; Proportional error PK

$EST METHOD=1 INTER MAXEVAL=2000 NOABORT SIG=3 PRINT=1 POSTHOC
$COV

; Xpose
$TABLE ID TIME DV MDV EVID IPRED IWRES ONEHEADER NOPRINT FILE=sdtab003
$TABLE CL V KA FIRSTONLY ONEHEADER NOPRINT FILE=patab003

