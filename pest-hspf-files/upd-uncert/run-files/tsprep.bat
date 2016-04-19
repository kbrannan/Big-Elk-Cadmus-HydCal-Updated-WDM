@echo off
E:\PEST\par2par %~dp0p2p_bigelk.dat
C:\BASINS41\models\HSPF\bin\WinHspfLt.exe -1 -1 %~dp0bigelk.uci
E:\PEST\surface_water_utilities\tsproc < %~dp0tsprep.in
