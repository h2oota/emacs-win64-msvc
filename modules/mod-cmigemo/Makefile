ROOT = ../..
EMACS = $(ROOT)/src/emacs


CMIGEMO=cmigemo
UTF8=utf8.d
MIGEMO_DICT=$(CMIGEMO)/dict
DICT_SRC=$(MIGEMO_DICT)/migemo-dict $(MIGEMO_DICT)/zen2han.dat $(MIGEMO_DICT)/han2zen.dat $(MIGEMO_DICT)/hira2kata.dat $(MIGEMO_DICT)/roma2hira.dat
DICT_FILES=$(DICT_SRC:$(MIGEMO_DICT)=$(UTF8))


srcdir=$(CMIGEMO)/src/
ojbdir=
O=obj
VPATH=$(srcdir)

CC=cl -nologo
CFLAGS=-MD  -D_CRT_SECURE_NO_WARNINGS -I$(srcdir) -I$(ROOT)/src
LIBS=cmigemo.lib

all: utf8.d/dict-stamp mod-cmigemo.dll cmigemo.elc cmigemo.el.gz

include $(srcdir)depend.mak

mod-cmigemo.dll:  $(LIBS)

cmigemo.lib: $(OBJ)
	lib -nologo -out:$@ $(OBJ)

$(MIGEMO_DICT)/migemo-dict:
	make -C $(MIGEMO_DICT)  base-dict
	mv $(MIGEMO_DICT)/base-dict $(MIGEMO_DICT)/migemo-dict

utf8.d/dict-stamp: $(DICT_SRC)
	@$(EMACS) -batch -l to-utf8.el $(DICT_SRC)
	@touch $@

cmigemo.el.gz: cmigemo.el
	gzip -fk $<
cmigemo.elc: cmigemo.el
	$(EMACS) -batch -f batch-byte-compile $<

check: mod-cmigemo.dll dict-stamp
	$(EMACS) -batch -l ert -l test.el -f ert-run-tests-batch-and-exit

.SUFFIXES: .obj .dll
.c.obj:
	$(CC) -c $(CFLAGS) $<
.c.dll:
	$(CC) $(CFLAGS) -Fe:$@ $< -link -dll -manifest $(LIBS) && \
	mt -nologo -manifest $@.manifest "-outputresource:$@;#2"
clean:
	rm cmigemo.lib $(OBJ) cmigemo.elc cmigemo.el.gz mod-cmigemo.dll
	rm -rf utf8.d
