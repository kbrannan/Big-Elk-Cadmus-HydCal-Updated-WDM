@echo off
E:\PEST\par2par p2p_bigelk.dat > nul
C:\BASINS41\models\HSPF\bin\WinHspfLt.exe -1 -1 M:\Models\Bacteria\HSPF\Big-Elk-Cadmus-HydCal-Updated-WDM\org-calib\Final_Deliverables_EPA_July2012\PEST_end\bigelk.uci
E:\PEST\surface_water_utilities\tsproc < tsproc.in > nul
