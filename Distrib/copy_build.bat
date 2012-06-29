xcopy /d /y ..\source\x64\*.dll x64
xcopy /d /y ..\source\x64\*.exe x64
xcopy /d /y ..\source\x86\*.dll x86
xcopy /d /y ..\source\x86\*.exe x86

md installer\Examples
md installer\EPRITestCircuits
md installer\IEEETestCases

xcopy /d /s Examples\*.dss installer\Examples
xcopy /d /s Examples\*.txt installer\Examples
xcopy /d /s Examples\*.py  installer\Examples
xcopy /d /s Examples\*.m   installer\Examples
xcopy /d /s Examples\*.csv installer\Examples
xcopy /d /s Examples\*.doc installer\Examples
xcopy /d /s Examples\*.pdf installer\Examples
xcopy /d /s Examples\*.xls installer\Examples
xcopy /d /s Examples\*.ocx installer\Examples
xcopy /d /s Examples\*.ppt installer\Examples
xcopy /d /s Examples\*.zip installer\Examples
xcopy /d /s Examples\*.bat installer\Examples

xcopy /d /s EPRITestCircuits\*.dss installer\EPRITestCircuits
xcopy /d /s EPRITestCircuits\*.csv installer\EPRITestCircuits
xcopy /d /s EPRITestCircuits\*.txt installer\EPRITestCircuits

xcopy /d /s IEEETestCases\*.dss installer\IEEETestCases
xcopy /d /s IEEETestCases\*.jpg installer\IEEETestCases
xcopy /d /s IEEETestCases\*.tif installer\IEEETestCases
xcopy /d /s IEEETestCases\*.csv installer\IEEETestCases
xcopy /d /s IEEETestCases\*.txt installer\IEEETestCases
xcopy /d /s IEEETestCases\*.xlsm installer\IEEETestCases
xcopy /d /s IEEETestCases\*.xlsx installer\IEEETestCases

