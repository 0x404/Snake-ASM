all: link

.PHONY: static
static:
	rc /v ./static/static.rc
	cvtres /machine:ix86 ./static/static.RES

.PHONY: compile
compile: static
	ml /c /I "D:\masm32\include" main.asm

.PHONY: link
link: compile
	link /subsystem:windows /libpath:D:\masm32\lib main.obj ./static/static.obj

.PHONY: clean
clean:
	rm static/static.res
	rm static/static.obj
	rm main.obj
	rm main.exe