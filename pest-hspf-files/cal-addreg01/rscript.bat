@echo off
REM R-executable
set Rscript="C:\Program Files\R\R-3.1.3\bin\x64\RScript.exe"
REM r-script file for processing HSPF output
set rfile="M:\Models\Bacteria\HSPF\Big-Elk-Cadmus-HydCal-Updated-WDM\r-files\proc-mod-flow-for-pest-args-in.R"
REM promary path for simulation
set dirPrime="M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM"
REM path extemsion from primary for current run
set dirCur="/pest-hspf-files/cal-addreg01"
REM put variables together to get the command
%Rscript% %rfile% %dirPrime% %dirCur%