all: link
.PHONY: static
static:
	rc /v ./static/static.rc
	cvtres /machine:ix86 ./static/static.RES
compile: static
	ml /c /I "D:\masm32\include" main.asm
link: compile
	link /subsystem:windows /libpath:D:\masm32\lib main.obj ./static/static.obj
clean:
	rm static/static.res
	rm static/static.obj
	rm main.obj
	rm main.exe