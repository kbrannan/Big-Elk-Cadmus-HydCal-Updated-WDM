@echo off
del M:\Models\Bacteria\HSPF\Big-Elk-Cadmus-HydCal-Updated-WDM\pest-hspf-files\pred-unc-pre-cal\modflow.out
REM del M:\Models\Bacteria\HSPF\Big-Elk-Cadmus-HydCal-Updated-WDM\pest-hspf-files\model.out
C:\PEST\par2par M:\Models\Bacteria\HSPF\Big-Elk-Cadmus-HydCal-Updated-WDM\pest-hspf-files\pred-unc-pre-cal\p2p_bigelk.dat > nul
C:\BASINS41\models\HSPF\bin\WinHspfLt.exe -1 -1 M:\Models\Bacteria\HSPF\Big-Elk-Cadmus-HydCal-Updated-WDM\pest-hspf-files\pred-unc-pre-cal\bigelk.uci
REM del M:\Models\Bacteria\HSPF\Big-Elk-Cadmus-HydCal-Updated-WDM\pest-hspf-files\pred-unc-pre-cal\model.out
REM del M:\Models\Bacteria\HSPF\Big-Elk-Cadmus-HydCal-Updated-WDM\pest-hspf-files\pred-unc-pre-cal\model.ins
"C:\Program Files\R\R-3.1.3\bin\x64\RScript.exe" M:\Models\Bacteria\HSPF\Big-Elk-Cadmus-HydCal-Updated-WDM\r-files\proc-mod-flow-for-pest-pre-cal-pred-error.R