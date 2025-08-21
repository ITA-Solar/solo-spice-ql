include ../Makefile.etc

# Implicit rules in Makefile.etc makes it unnecessary to 
# specify how compilations are to be done

all: fmedian2_dlm.$(SO_EXT)

install: fmedian2_dlm.$(SO_EXT) fmedian2_dlm.dlm
	mkdir -p $(SSW_DLM_TOP)/$(SSW_BINARY_TYPE)
	cp -p $^ $(SSW_DLM_TOP)/$(SSW_BINARY_TYPE)/
