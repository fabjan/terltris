PROJECT = terltris
PROG = $(PROJECT)
DEPS = esdl2

# This branch is hacked to make builds with newer clang to work
# see https://github.com/ninenines/nif_helpers/issues/2
dep_esdl2 = git https://github.com/fabjan/esdl2/ fabian

ERLC_OPTS = -Werror +debug_info +warn_export_vars +warn_shadow_vars +warn_obsolete_guard

# TODO temp while converting
ERLC_OPTS += -Ddebugging=1
ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))

# TODO package as release? relx.config + make rel
$(PROG): all
	cp script/run.escript $@
	chmod a+x $@

include erlang.mk
