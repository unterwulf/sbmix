@echo off
set PROG=sbmix
if "%1"=="" goto build
for %%t in (build clean dist) do if "%1"=="%%t" goto %%t
echo Available targets: build (default), dist and clean
goto end

:build
	nasm -f bin -O2 -o %PROG%.com -l %PROG%.lst %PROG%.asm
	goto end

:dist
	if not exist %PROG%.com call make build
	pkzip %PROG%.zip %PROG%.com
	goto end

:clean
	del %PROG%.lst
	del %PROG%.map
	del %PROG%.com
	del %PROG%.zip

:end
