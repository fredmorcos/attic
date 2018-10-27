# Haskell source files, including symlinks like Tac.hs and Tail.hs
srcs = Cat.hs Tac.hs Echo.hs Yes.hs True.hs False.hs Head.hs Tail.hs Uniq.hs

# Binaries based on the list of source files by lowercasing and
# removing .hs postfix. Used in the make default/all and clean targets
bins = $(call lowercase,$(srcs:.hs=))

# Lint targets based on the list of binaries by prefixing with
# lint-$(binary-name). Used in the lint target
lnts = $(foreach bin,$(bins),lint-$(bin))

# Haskell interface and object files based on the source files by
# replacing .hs postfix. Used in the clean target
hifs = $(srcs:.hs=.hi)
objs = $(srcs:.hs=.o)

# Haskell Compiler and compiler flags
HC ?= ghc
HC_FLAGS ?= -Wall --make

# Lint tool and lint tool flags
LINT = hlint
LINT_FLAGS = -x

# Special-case flags for lint tool
# Used in respective lint targets
tac-lint-flags = --cpp-define=TAC
tail-lint-flags = --cpp-define=TAIL

# Special-case flags for building
# Used in respective $(binary-name) targets
tac-build-flags = -DTAC
tail-build-flags = -DTAIL

.PHONY: default all clean lint $(lnts)
default: all
all: $(bins)
lint: $(lnts)

# String lowercasing helper function
define lowercase
$(shell echo $(1) | tr A-Z a-z)
endef

# Target template for lint-$(binary-name) targets
define lint-template
$(1): $(2)
	$(LINT) $(LINT_FLAGS) $(2) $(call $(1)-lint-flags)
endef

# Go over each source file and meta-create a lint target for it using
# the lint-template
$(foreach src,$(srcs), \
  $(eval $(call lint-template, \
    lint-$(call lowercase,$(src:.hs=)),$(src))))

# Target template for $(binary-name) targets
define build-template
$(1): $(2)
	$(HC) $(HC_FLAGS) $(2) -o $(1) $(call $(1)-build-flags)
endef

# Go over each source file and meta-create a build target for it using
# the build-template
$(foreach src,$(srcs), \
  $(eval $(call build-template, \
     $(call lowercase,$(src:.hs=)),$(src))))

# Cleanup
clean:
	rm -rf $(bins) $(hifs) $(objs)
