@echo off
set HOME_PATH=%~dp0
set HOME=%HOME_PATH:~0,-1%
set LIB="%HOME%\lib"
set CP=
set CLASS=ucesoft.mac.ui.MacGUI
set OPT=-server -Xms64M -Xmx64M --add-opens java.desktop/com.sun.media.sound=ALL-UNNAMED

if defined JAVA_HOME goto :setJavaHome
set JAVAW=javaw
set JAVA=java
goto :Check
:setJavaHome
set JAVAW=%JAVA_HOME%\bin\javaw
set JAVA=%JAVA_HOME%\bin\java
:Check
if "%1" == "--shell" goto shift_shell
if "%1" == "--help" goto help_shell
start %JAVAW% %OPT% -cp %CP% -DsmacHome="%HOME%" %CLASS% %*
goto end
:shift_shell

set Args=
:Parse
shift
set First=%1
if not defined First goto :EndParse
  set Args=%Args% %First%
  goto :Parse
:EndParse
goto shell

:help_shell
set Args=%*

:shell
%JAVA% %OPT% -cp %CP% -DsmacHome="%HOME%" %CLASS% %Args%
:end
