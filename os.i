; Beam Racer * https://beamracer.net
; Video and Display List coprocessor board for the Commodore 64
; Copyright (C)2019-2020 Mad Hackers Lab
;
; https://github.com/madhackerslab/beamracer-examples
;

LINNUM   = $14      ; Integer Line Number Value
LINNUMLO = $14
LINNUMHI = $15
FREEZP   = $fb

BASIC_BASE  = $a000
FRMNUM   = $ad8a ; Evaluate a Numeric Expression and/or Check for Data Type Mismatch
CHKCLS   = $aef7 ; Check for and Skip Closing Parentheses
CHKOPN   = $aefa ; Check for and Skip Opening Parentheses
CHKCOM   = $aefd ; Check for and Skip Comma
AYINT    = $b1bf ; Convert a Floating Point Number to a Signed Integer
GETBYT   = $b79e ; Parse A into uint8_t in X
GETADR   = $b7f7 ; Convert a Floating Point Number to an Unsigned Two-Byte Integer in LINNUM ($14/$15)

