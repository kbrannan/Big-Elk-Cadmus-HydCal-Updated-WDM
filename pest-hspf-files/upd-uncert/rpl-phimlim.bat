set /a ln=1
for /F "tokens=*" %%i in (addreg1-1.pst) do (
if !ln! == "13840" (
echo "  6.9000000E+03  1.0500000E-10  0.1000000    " >> junk.txt
)
else (
if 
echo %%i >> junk.txt
echo 