include ../Makefile.etc

# Implicit rules in Makefile.etc makes it unnecessary to 
# specify how compilations are to be done

all: cfit.$(SO_EXT)

install: cfit.$(SO_EXT) cfit.dlm
	mkdir -p $(SSW_DLM_TOP)/$(SSW_BINARY_TYPE)
	cp -p $^ $(SSW_DLM_TOP)/$(SSW_BINARY_TYPE)/
