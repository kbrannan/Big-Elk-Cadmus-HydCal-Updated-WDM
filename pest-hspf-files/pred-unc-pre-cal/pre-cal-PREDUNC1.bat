@echo off
setlocal enableDelayedExpansion
set "nobs=44"
FOR /l %%N in (1,1,%nobs%) do (
    set "npoop=000%%N"
	set "pname=!npoop:~-3!"
	CALL mod-predunc1-in.bat !pname!
    CALL jrow2vec control-pred-err-pre-cal-jac.jco prediction_!pname! pred_!pname!.vec>NULL
	CALL predunc1<predunc!npoop:~-3!.in>predunc-!npoop:~-3!.out
)