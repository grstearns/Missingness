REM DON'T FORGET TO CTRL H FOR mi

start /D "c:\missingness\" cmd /C ""C:\Program Files\R\R-2.13.0\bin\x64\R.exe" --no-save --args 2 1 lwd RHOME=c:\missingness TMPDIR=c:\missingness\tmp < c:\missingness\run.models.r > c:\missingness\results\mi-1.rdata"
start /D "c:\missingness\" cmd /C ""C:\Program Files\R\R-2.13.0\bin\x64\R.exe" --no-save --args 2 2 lwd RHOME=c:\missingness TMPDIR=c:\missingness\tmp < c:\missingness\run.models.r > c:\missingness\results\mi-2.rdata"
