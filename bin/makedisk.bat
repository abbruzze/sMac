@echo off
set HOME_PATH=%~dp0
set HOME=%HOME_PATH:~0,-1%
set LIB="%HOME%\lib"
set CP=
set CLASS=ucesoft.mac.util.EmptyDiskMaker

if defined JAVA_HOME goto :setJavaHome
set JAVA=java
goto :Check
:setJavaHome
set JAVA=%JAVA_HOME%\bin\java
:Check
%JAVA% -cp %CP% %CLASS% %*

