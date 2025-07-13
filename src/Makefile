# ******************************************************************************
# *                                 VARIABLES                                  *
# ******************************************************************************

COVERAGE  ?= false
DEBUG     ?= false
FTNLIB    ?= $(abspath ../fortranlib)
LANG_OPTS := -fopenmp -ffree-form -ffree-line-length-none -frecursive -fno-unsafe-math-optimizations -frounding-math -fsignaling-nans -fPIC
WARN_OPTS := -Wall -Wextra -Waliasing -Wcharacter-truncation -Wconversion-extra -Wimplicit-interface -Wimplicit-procedure -Wunderflow -Wtabs
MACH_OPTS := -march=native -m64

# ******************************************************************************
# *                                  BINARIES                                  *
# ******************************************************************************

AR      := $(shell which ar             2> /dev/null || echo "ERROR")
CUT     := $(shell which cut            2> /dev/null || echo "ERROR")
FC      := $(shell which gfortran-mp-14 2> /dev/null || echo "ERROR")
GREP    := $(shell which grep           2> /dev/null || echo "ERROR")
PYTHON3 := $(shell which python3.12     2> /dev/null || echo "ERROR")
RM      := $(shell which rm             2> /dev/null || echo "ERROR")

# ******************************************************************************
# *                             DYNAMIC VARIABLES                              *
# ******************************************************************************

ifeq ($(COVERAGE), true)
	LANG_OPTS += -g -O0 -fprofile-abs-path --coverage
else ifeq ($(DEBUG), true)
	LANG_OPTS += -g -fcheck=all
else
	LANG_OPTS += -O2
endif

# ******************************************************************************
# *                               CHECK BINARIES                               *
# ******************************************************************************

ifeq ($(AR),ERROR)
    $(error The binary "ar" is not installed)
endif
ifeq ($(CUT),ERROR)
    $(error The binary "cut" is not installed)
endif
ifeq ($(FC),ERROR)
    $(error The binary "fc" is not installed)
endif
ifeq ($(GREP),ERROR)
    $(error The binary "grep" is not installed)
endif
ifeq ($(PYTHON3),ERROR)
    $(error The binary "python3" is not installed)
endif
ifeq ($(RM),ERROR)
    $(error The binary "rm" is not installed)
endif

# ******************************************************************************
# *                            CHECK PYTHON MODULES                            *
# ******************************************************************************

# ifneq ($(shell $(PYTHON3) -c "import numpy; print(0)" 2> /dev/null),0)
#     $(error The Python module "numpy" is not installed)
# endif

# ******************************************************************************
# *                             DERIVED VARIABLES                              *
# ******************************************************************************

# SUFFIX ?= $(shell $(PYTHON3) -c "import sysconfig; print(sysconfig.get_config_var(\"EXT_SUFFIX\"))")

# ******************************************************************************
# *                           USER-SPECIFIED TARGETS                           *
# ******************************************************************************

# "gmake -r [all]"   = "gmake -r compile" (default)
all:				compile

# "gmake -r clean"   = removes the compiled code
clean:
	$(RM) -f createFlood downscale *.a *.gcda *.gcno *.mod *.o *.so
	$(MAKE) -r -C $(FTNLIB) COVERAGE=$(COVERAGE) DEBUG=$(DEBUG) FC=$(FC) PYTHON3=$(PYTHON3) clean

# "gmake -r compile" = compiles the code
compile:			createFlood													\
					downscale

# "gmake -r help"    = print this help
help:
	echo "These are the available options:"
	$(GREP) -E "^# \"gmake -r " Makefile | $(CUT) -c 2-

# ******************************************************************************
# *                            ENVIRONMENT SETTINGS                            *
# ******************************************************************************

.SILENT: help

# ******************************************************************************
# *                        INTERNALLY-SPECIFIED TARGETS                        *
# ******************************************************************************

# NOTE: There was a bug in NumPy (using "meson" to build) where "f2py" would
#       copy the file to a build folder and, therefore, the relative paths to
#       external libraries would break. To work around this I prepend the
#       current directory to the library path to make it an absolute path. See:
#         * https://github.com/numpy/numpy/issues/25344

# NOTE: See https://numpy.org/doc/stable/f2py/buildtools/distutils-to-meson.html

# NOTE: How GCC embeds the RPATH has changed, which results in binaries which do
#       not work on MacOS 15.5 using GFortran 14.2 and Python 3.12 (as of
#       21/Jun/2025). Here are two examples of other people having the same
#       problem:
#         * https://github.com/mesonbuild/meson/issues/10711
#         * https://stackoverflow.com/questions/52430325/f2py-compilation-failed-cannot-find-library-gomp
#       After hours of investigation, I have settled on a work around using
#       "install_name_tool -add_rpath" on Darwin-based operating systems.

$(FTNLIB)/lib%.a																\
$(FTNLIB)/%.mod																	\
$(FTNLIB)/%.o &:	$(FTNLIB)/%.F90
	$(MAKE) -r -C $(FTNLIB) COVERAGE=$(COVERAGE) DEBUG=$(DEBUG) FC=$(FC) PYTHON3=$(PYTHON3) $*.o

libmod_funcs.a																	\
mod_funcs.mod																	\
mod_funcs.o &:		$(FTNLIB)/mod_safe.mod										\
					mod_funcs.F90
	$(FC) -c $(LANG_OPTS) $(WARN_OPTS) $(MACH_OPTS) -I$(FTNLIB) mod_funcs.F90
	$(AR) -r libmod_funcs.a mod_funcs.o

createFlood.o:		$(FTNLIB)/mod_safe.mod										\
					mod_funcs.mod												\
					createFlood.F90
	$(FC) -c $(LANG_OPTS) $(WARN_OPTS) $(MACH_OPTS) -I$(FTNLIB) createFlood.F90 -o $@

downscale.o:		$(FTNLIB)/mod_safe.mod										\
					downscale.F90
	$(FC) -c $(LANG_OPTS) $(WARN_OPTS) $(MACH_OPTS) -I$(FTNLIB) downscale.F90 -o $@

createFlood:		$(FTNLIB)/mod_safe.o										\
					mod_funcs.o													\
					createFlood.o
	$(FC) $(LANG_OPTS) $(WARN_OPTS) $(MACH_OPTS) createFlood.o $(FTNLIB)/mod_safe.o mod_funcs.o -o $@

downscale:			$(FTNLIB)/mod_safe.o										\
					downscale.o
	$(FC) $(LANG_OPTS) $(WARN_OPTS) $(MACH_OPTS) downscale.o $(FTNLIB)/mod_safe.o -o $@
