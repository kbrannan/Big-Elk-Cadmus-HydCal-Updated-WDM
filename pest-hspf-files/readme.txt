7/23/2012

Folder Contents: 

-Base HSPF and PEST files used for model calibration and uncertainty analysis. Files in the "PEST_end" folder were generated from the contents of this folder using the method described in pest_method.docx
-PEST & HSPF files are configured to run from the C:\HSPF\PEST directory.
-PEST & HSPF files are configured to look for WDM files in the C:\HSPF directory.
-tsprep.bat & model.bat are configured to look for PEST executables in the C:\PEST directory.
-tsprep.bat & model.bat are configured to look for the WinHSPF executable in the C:\BASINS\models\HSPF\bin directory.

To run PEST from scratch:

1) Create directory c:\HSPF
2) Paste bigelk_in.WDM, bigelk_out.WDM, and bigelk_PLSout.wdm into c:\HSPF
3) Create directory c:\HSPF\PEST
4) Paste contents of PEST_start into C:\HSPF\PEST
5) Follow directions in pest_method.docx