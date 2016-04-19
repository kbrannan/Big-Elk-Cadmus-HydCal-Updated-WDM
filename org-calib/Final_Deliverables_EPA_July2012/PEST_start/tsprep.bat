@echo off
C:\PEST\par2par p2p_bigelk.dat
C:\BASINS\models\HSPF\bin\WinHspfLt.exe -1 -1 C:\HSPF\PEST\bigelk.uci
C:\PEST\swutils\tsproc < tsproc.in
