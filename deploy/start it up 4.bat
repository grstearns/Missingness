REM DON'T FORGET TO CTRL H FOR mi

start /D "c:\missingness\" cmd /C ""C:\Program Files\R\R-2.13.0\bin\x64\Rterm.exe" --no-save --args 4 1 mi RHOME=c:\missingness TMPDIR=c:\missingness\tmp < c:\missingness\run.models.r > c:\missingness\results\mi-1.rdata"
start /D "c:\missingness\" cmd /C ""C:\Program Files\R\R-2.13.0\bin\x64\Rterm.exe" --no-save --args 4 2 mi RHOME=c:\missingness TMPDIR=c:\missingness\tmp < c:\missingness\run.models.r > c:\missingness\results\mi-2.rdata"
start /D "c:\missingness\" cmd /C ""C:\Program Files\R\R-2.13.0\bin\x64\Rterm.exe" --no-save --args 4 3 mi RHOME=c:\missingness TMPDIR=c:\missingness\tmp < c:\missingness\run.models.r > c:\missingness\results\mi-3.rdata"
start /D "c:\missingness\" cmd /C ""C:\Program Files\R\R-2.13.0\bin\x64\Rterm.exe" --no-save --args 4 4 mi RHOME=c:\missingness TMPDIR=c:\missingness\tmp < c:\missingness\run.models.r > c:\missingness\results\mi-4.rdata"
