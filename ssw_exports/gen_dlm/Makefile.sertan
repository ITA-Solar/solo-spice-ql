DLMS = fmedian
EXPORT_DIR = $(HOME)/sb/ftp-area/dlm

all:
	@for i in $(DLMS) ; do make -C $$i ; done

install:
	@for i in $(DLMS) ; do make -C $$i install ; done

rounds: clean all install

clean:
	@for i in $(DLMS) ; do make -C $$i clean ; done

neat:
	rm -f *~ \#*
	@for i in $(DLMS) ; do make -C $$i neat ; done

export: neat
	rm -rf $(EXPORT_DIR) ; mkdir $(EXPORT_DIR)
	cp -p Makefile* $(EXPORT_DIR)
	@for i in $(DLMS) ; do (echo "cp -rp $$i $(EXPORT_DIR)" ; cp -rp $$i $(EXPORT_DIR)) ; done
	tar cvzf $(EXPORT_DIR)/gen_dlm.tar.gz Makefile* $(DLMS)

test:
	echo "$(MAKEFLAGS)"
