for /L %%i in (1,1,500) do (
del temp.pst
del temp.res
c:\pest\parrep adjparset%%i.par adjusted.pst temp.pst
c:\pest\pest temp
copy temp.res adjparset%%i.res)
