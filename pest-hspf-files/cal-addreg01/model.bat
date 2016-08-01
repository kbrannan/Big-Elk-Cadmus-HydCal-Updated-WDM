@echo off
del modflow.out
C:\PEST\par2par p2p_bigelk.dat > nul
C:\BASINS41\models\HSPF\bin\WinHspfLt.exe -1 -1 bigelk.uci
call rscript.bat