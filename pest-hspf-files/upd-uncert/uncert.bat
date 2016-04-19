rem ###########################################################
rem Delete an existing record file.
rem ###########################################################

del /P record_recalib.dat
echo  > record_recalib.dat
rem pause

rem ###########################################################
rem Do all the PEST runs.
rem ###########################################################


for /L %%i in (1,1,1000) do (
del temp.pst
del tempreg.pst
del tempreg.rec
parrep uncupd%%i.par calib.pst temp.pst
addreg1 temp.pst tempreg.pst

pest tempreg.pst /i < response_calib.in
rem pest temp
echo ' '  >> record_recalib.dat
echo  PARAMETER SET %%i >> record_recalib.dat
find /I "ie phi" tempreg.rec >> record_recalib.dat
find /I "total model calls: " tempreg.rec >> record_recalib.dat
copy tempreg.par adjparset%%i.par
copy tempreg.rec adjparset%%i.rec
)
