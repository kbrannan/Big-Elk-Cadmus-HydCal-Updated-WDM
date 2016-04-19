cd randpar-files
randpar < c:\temp\upd-uncert\randpar.in
pause
cd ..
echo start parrep-addreg loop
pause
@echo off
for /L %%i in (1,1,1000) do (
parrep c:\temp\upd-uncert\randpar-files\rpar%%i.par M:\Models\Bacteria\HSPF\Big-Elk-Cadmus-HydCal-Updated-WDM\pest-hspf-files\upd-calib\cal-new-lims.pst c:\temp\upd-uncert\parrep-files\parrep%%i.pst
addreg1 c:\temp\upd-uncert\parrep-files\parrep%%i.pst c:\temp\upd-uncert\addreg-files\addreg1-%%i.pst
echo file %%i completed
@echo off
)
echo start replace-philim R-script
pause
"C:\Program Files\R\R-3.1.3\bin\x64\Rscript.exe" --vanilla c:\temp\uncert-upd\replace-phimlim.R
