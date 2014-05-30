PROJECT = switchboard
PROFILE = dev

ERLC_OPTS = -I include +'{parse_transform, lager_transform}' \
            +debug_info +warn_export_all +warn_shadow_vars +warn_obsolete_guard

EDOC_OPTS = no_packages, {subpackages, false}, {preprocess, true}

ifeq ($(PROFILE), dev)
	RELX_OPTS = --dev-mode
endif

DEPS            = lager gproc cowboy jsx
dep_lager	= https://github.com/basho/lager.git 2.0.3
dep_gproc	= https://github.com/uwiger/gproc 0.3
dep_cowboy	= https://github.com/extend/cowboy 0.9.0
dep_jsx		= https://github.com/talentdeficit/jsx v1.4.5

include erlang.mk

S3CMD_CONF	= .s3cmd
S3CMD		= s3cmd -c $(S3CMD_CONF)

serveclient: PORT = 8001
serveclient:
	(cd client && python -m SimpleHTTPServer $(PORT))


switchboard.tar.gz: _rel
	tar -s/_rel/switchboard/ -cvf $@ $<


$(S3CMD_CONF):
	$(S3CMD) --configure

push: switchboard.tar.gz $(S3CMD_CONF)
	$(S3CMD) push
