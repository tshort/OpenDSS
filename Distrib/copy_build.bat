xcopy /d /y ..\source\x64\*.dll x64
xcopy /d /y ..\source\x64\*.exe x64
xcopy /d /y ..\source\x86\*.dll x86
xcopy /d /y ..\source\x86\*.exe x86

md installer\Examples
md installer\EPRITestCircuits
md installer\IEEETestCases

xcopy /d /s /y Examples\*.dss installer\Examples
xcopy /d /s /y Examples\*.txt installer\Examples
xcopy /d /s /y Examples\*.m   installer\Examples
xcopy /d /s /y Examples\*.doc installer\Examples
xcopy /d /s /y Examples\*.xls installer\Examples
xcopy /d /s /y Examples\*.ocx installer\Examples
rem xcopy /d /s /y Examples\*.py  installer\Examples
rem xcopy /d /s /y Examples\*.csv installer\Examples
rem xcopy /d /s /y Examples\*.pdf installer\Examples
rem xcopy /d /s /y Examples\*.ppt installer\Examples
rem xcopy /d /s /y Examples\*.zip installer\Examples
rem xcopy /d /s /y Examples\*.bat installer\Examples

xcopy /d /s /y EPRITestCircuits\*.dss installer\EPRITestCircuits
xcopy /d /s /y EPRITestCircuits\*.csv installer\EPRITestCircuits
xcopy /d /s /y EPRITestCircuits\*.txt installer\EPRITestCircuits

xcopy /d /s /y IEEETestCases\*.dss installer\IEEETestCases
xcopy /d /s /y IEEETestCases\*.jpg installer\IEEETestCases
xcopy /d /s /y IEEETestCases\*.tif installer\IEEETestCases
xcopy /d /s /y IEEETestCases\*.csv installer\IEEETestCases
xcopy /d /s /y IEEETestCases\*.txt installer\IEEETestCases
xcopy /d /s /y IEEETestCases\*.xlsm installer\IEEETestCases
xcopy /d /s /y IEEETestCases\*.xlsx installer\IEEETestCases

