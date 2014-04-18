PROJECT = imapswitchboard

ERLC_OPTS = +debug_info +warn_export_all +warn_shadow_vars +warn_obsolete_guard

DEPS            = gproc
dep_gproc       = https://github.com/uwiger/gproc 0.3

include erlang.mk

console:
	erl -pa ebin -pa deps/*/ebin -s lager -s imapswitchboard
