@echo off
chcp 65001 >nul 2>&1
setlocal enabledelayedexpansion
cd /d "%~dp0"

echo.
echo ==========================================================
echo   个性化分析助手
echo ==========================================================
echo.

rem ---- 找 R ----
set "RSCRIPT="
for %%R in (
  "%ProgramFiles%\R\R-4.1.2\bin\Rscript.exe"
  "D:\soft\R412\bin\Rscript.exe"
) do (
  if exist "%%~R" if not defined RSCRIPT set "RSCRIPT=%%~R"
)
if not defined RSCRIPT (
  where Rscript.exe >nul 2>&1 && for /f "delims=" %%i in ('where Rscript.exe') do (
    if not defined RSCRIPT set "RSCRIPT=%%i"
  )
)
if not defined RSCRIPT (
  for /d %%D in ("%ProgramFiles%\R\R-*") do (
    if exist "%%~D\bin\Rscript.exe" if not defined RSCRIPT set "RSCRIPT=%%~D\bin\Rscript.exe"
  )
)

if not defined RSCRIPT (
  echo  [X] 没有找到 R。
  echo.
  echo      这个助手需要 R 才能运行。请先安装 R：
  echo      https://cran.r-project.org/bin/windows/base/
  echo.
  echo      如果你已经装了 R，但是在别的位置，改一下这个
  echo      bat 文件里的 RSCRIPT 那一段，或者设一个环境变量：
  echo        set RSCRIPT_PATH=你的\Rscript.exe 路径
  echo.
  pause
  exit /b 1
)

if defined RSCRIPT_PATH set "RSCRIPT=%RSCRIPT_PATH%"

echo  [1/2] 找到 R： %RSCRIPT%
echo  [2/2] 正在启动本地服务，浏览器会自动打开…
echo.

echo  关掉这个窗口就是停止服务。分析结果不会因为这个窗口关掉而丢失。
echo.

"%RSCRIPT%" --vanilla "%~dp0serve.R" --port 18980
if errorlevel 1 (
  echo.
  echo  [X] 服务起不来了。上面有报错信息。
  echo      常见原因：18980 端口被别的程序占了。
  echo      试试：把 serve.R 后面的 --port 18980 改成 --port 18981
  echo.
  pause
)
endlocal
