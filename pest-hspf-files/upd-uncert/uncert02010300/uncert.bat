rem ###########################################################
rem Delete an existing record file.
rem ###########################################################

del /P record_recalib.dat
echo  > record_recalib.dat

rem ###########################################################
rem Do all the PEST runs.
rem ###########################################################


for /L %%i in (201,1,300) do (

pest uncert%%i.pst /i < response_calib.in

echo ' '  >> record_recalib.dat
echo  PARAMETER SET %%i >> record_recalib.dat
find /I "ie phi" uncert%%i.rec >> record_recalib.dat
find /I "total model calls: " uncert%%i.rec >> record_recalib.dat
)
