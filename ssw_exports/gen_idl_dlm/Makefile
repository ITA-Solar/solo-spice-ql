DLMS = fmedian cfit

all:
	@for i in $(DLMS) ; do make -C $$i ; done

install:
	@for i in $(DLMS) ; do make -C $$i install ; done

rounds: clean all install

clean:
	rm -f *~ \#* Makefile.bak
	@for i in $(DLMS) ; do make -C $$i clean ; done

neat:
	rm -f *~ \#*
	@for i in $(DLMS) ; do make -C $$i neat ; done
