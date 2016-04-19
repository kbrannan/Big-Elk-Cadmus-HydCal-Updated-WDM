rem ###########################################################
rem Delete an existing record file.
rem ###########################################################

del /P record_recalib.dat
echo  > record_recalib.dat
pause

rem ###########################################################
rem Do all the PEST runs.
rem ###########################################################


for /L %%i in (1,1,500) do (
del temp.pst
del temp.rec
c:\PEST\parrep parset%%i.par uncert.pst temp.pst
c:\PEST\pest temp /i < response_calib.in
echo ' '  >> record_recalib.dat
echo  PARAMETER SET %%i >> record_recalib.dat
find /I "ie phi" temp.rec >> record_recalib.dat
find /I "total model calls: " temp.rec >> record_recalib.dat
copy temp.par adjparset%%i.par
)
