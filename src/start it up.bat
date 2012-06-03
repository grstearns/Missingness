REM DON'T FORGET TO CTRL H FOR lwd

mkdir Results

start /D "c:\missingness\" cmd /C ""C:\Program Files\R\R-2.13.0\bin\x64\Rterm.exe" --no-save --args 1 1 lwd RHOME=c:\missingness < c:\missingness\run.models.r > c:\missingness\results\lwd-1.rdata"
