FOR /F "tokens=5 delims= " %%P IN ('netstat -a -n -o | findstr :5570') DO TaskKill.exe /PID %%P /F /T
