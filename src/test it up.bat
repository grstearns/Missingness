REM DON'T FORGET TO CTRL H FOR mi

start /D "c:\missingness\" cmd /K ""C:\Program Files\R\R-2.13.0\bin\x64\R.exe" --no-save --args 1 1 lwd RHOME=c:\missingness TMPDIR=c:\missingness\tmp < c:\missingness\test2.r> c:\missingness\results\mi-1.rdata"
