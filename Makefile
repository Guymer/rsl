# Find executables ...
CUT   := $(shell which cut    2> /dev/null || echo "ERROR")
EGREP := $(shell which egrep  2> /dev/null || echo "ERROR")
FC    := $(shell which mpif90 2> /dev/null || echo "ERROR")
RM    := $(shell which rm     2> /dev/null || echo "ERROR")

# Set defaults ...
DEBUG  ?= false
FTNLIB ?= fortranlib

# ******************************************************************************

# Set compiler flags ...
DEBG_OPTS := -g -fcheck=all
LANG_OPTS := -ffree-form -ffree-line-length-none -frecursive -fno-unsafe-math-optimizations -frounding-math -fsignaling-nans -fPIC
WARN_OPTS := -Wall -Wextra -Waliasing -Wcharacter-truncation -Wconversion-extra -Wimplicit-interface -Wimplicit-procedure -Wunderflow -Wtabs
OPTM_OPTS := -O2
MACH_OPTS := -march=native -m64

# If the user wants debugging then append the debugging flags to the language
# flags ...
ifeq ($(DEBUG), true)
	LANG_OPTS += $(DEBG_OPTS)
endif

# ******************************************************************************

# "gmake [all]"   = "make compile" (default)
all:				compile

# "gmake clean"   = removes the compiled code
clean:				$(RM)
	$(RM) -f convertBINtoPPM createFlood *.mod *.o
	$(MAKE) -r -C $(FTNLIB) clean

# "gmake compile" = compiles the code
compile:			mod_funcs.o													\
					convertBINtoPPM												\
					createFlood

# "gmake help"    = print this help
help:				$(EGREP)													\
					$(CUT)
	echo "These are the available options:"
	$(EGREP) "^# \"gmake " Makefile | $(CUT) -c 2-

# ******************************************************************************

.SILENT: help

# ******************************************************************************

# NOTE: As of 01/Nov/2019 there is still a bug in "gcc9" from MacPorts which
#       results in it being unable to find some system libraries. Below are
#       links to the MacPorts ticket and the GCC ticket as well as the reference
#       for my chosen (hopefully temporary) workaround.
#         * https://trac.macports.org/ticket/59113
#         * https://gcc.gnu.org/bugzilla/show_bug.cgi?id=90835
#         * https://stackoverflow.com/a/58081934

$(FTNLIB)/%.mod																	\
$(FTNLIB)/%.o:		$(FTNLIB)/Makefile											\
					$(FTNLIB)/%.F90
	$(MAKE) -r -C $(FTNLIB) DEBUG=$(DEBUG) $*.o

mod_funcs.mod																	\
mod_funcs.o:		$(FC)														\
					$(FTNLIB)/mod_safe.mod										\
					mod_funcs.F90
	$(FC) -c $(LANG_OPTS) $(WARN_OPTS) $(OPTM_OPTS) $(MACH_OPTS) -I$(FTNLIB) mod_funcs.F90

convertBINtoPPM.o:	$(FC)														\
					$(FTNLIB)/mod_safe.mod										\
					convertBINtoPPM.F90
	$(FC) -c $(LANG_OPTS) $(WARN_OPTS) $(OPTM_OPTS) $(MACH_OPTS) -I$(FTNLIB) convertBINtoPPM.F90 -o $@

createFlood.o:		$(FC)														\
					$(FTNLIB)/mod_safe.mod										\
					mod_funcs.mod												\
					createFlood.F90
	$(FC) -c -fopenmp $(LANG_OPTS) $(WARN_OPTS) $(OPTM_OPTS) $(MACH_OPTS) -I$(FTNLIB) createFlood.F90 -o $@

convertBINtoPPM:	$(FC)														\
					$(FTNLIB)/mod_safe.o										\
					convertBINtoPPM.o
	$(FC) $(LANG_OPTS) $(WARN_OPTS) $(OPTM_OPTS) $(MACH_OPTS) convertBINtoPPM.o $(FTNLIB)/mod_safe.o -L/usr/lib -o $@

createFlood:		$(FC)														\
					$(FTNLIB)/mod_safe.o										\
					mod_funcs.o													\
					createFlood.o
	$(FC) -fopenmp $(LANG_OPTS) $(WARN_OPTS) $(OPTM_OPTS) $(MACH_OPTS) createFlood.o $(FTNLIB)/mod_safe.o mod_funcs.o -L/usr/lib -o $@
