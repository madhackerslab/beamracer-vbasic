CA65=ca65
CL65=cl65
LD65=ld65
HOST_OS := $(shell uname)

basic: basic.s vlib/vasyl.s c64-asm.cfg about_string.i
	$(CL65) -C ./c64-asm.cfg -u __EXEHDR__ $<

about_string.i: FORCE
	@printf ".byte \"revision: " >$@
	@git show -s --format=%h|xargs echo -n >>$@
	@printf " " >>$@
ifeq ($(HOST_OS), Darwin)
	@date -ujf "%Y-%m-%d %H:%M:%S %z" "`git show -s --format=%ci`" +"%Y-%m-%d %H:%M:%S"|xargs echo -n >>$@
else
	@date -u -d "`git show -s --format=%ci`" +"%Y-%m-%d %H:%M:%S"|xargs echo -n >>$@
endif
	@printf "\", 13\n.byte \"built: " >>$@
	@date -u "+%Y-%m-%d %H:%M:%S"|xargs echo -n >>$@
	@git diff-index --quiet HEAD -- || (printf "\", 13\n.byte \"with changes") >>$@
	@printf "\", 13" >>$@
FORCE:

clean:
	rm -f basic.o basic about_string.i
