for /L %%i in (1, 1, 1000) do (
del tmp.pst
del.temp.res
parrep pre%%i.par control-pred-err-pre-cal-0.pst temp.pst
pest temp.pst
copy temp.res pre%%i.res)