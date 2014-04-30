PROJECT = switchboard

ERLC_OPTS = -I include +'{parse_transform, lager_transform}' \
            +debug_info +warn_export_all +warn_shadow_vars +warn_obsolete_guard

DEPS            = lager gproc
dep_lager	= https://github.com/basho/lager.git 2.0.3
dep_gproc       = https://github.com/uwiger/gproc 0.3

include erlang.mk

console:
	erl -pa ebin -pa deps/*/ebin -s lager -s switchboard
