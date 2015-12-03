PROJECT = canillita

CONFIG ?= test/test.config

DEPS = sumo_rest lasse katana
SHELL_DEPS = sync
TEST_DEPS = shotgun mixer
LOCAL_DEPS = tools compiler syntax_tools common_test inets test_server dialyzer wx

dep_sumo_rest = git https://github.com/inaka/sumo_rest.git 0.1.0
dep_lasse = git https://github.com/inaka/lasse.git 1.0.1
dep_sync = git https://github.com/rustyio/sync.git 9c78e7b
dep_katana = git https://github.com/inaka/erlang-katana.git 07efe94
dep_shotgun = git https://github.com/inaka/shotgun.git 0.1.12

include erlang.mk

DIALYZER_DIRS := ebin/ test/
DIALYZER_OPTS := --verbose --statistics -Wunmatched_returns

TEST_ERLC_OPTS += +debug_info
CT_OPTS = -cover test/canillita.coverspec -erl_args -config ${CONFIG}

SHELL_OPTS = -s sync -config ${CONFIG}

quicktests: app
	@$(MAKE) --no-print-directory app-build test-dir ERLC_OPTS="$(TEST_ERLC_OPTS)"
	$(verbose) mkdir -p $(CURDIR)/logs/
	$(gen_verbose) $(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES)) $(CT_OPTS)

test-build-plt: ERLC_OPTS=$(TEST_ERLC_OPTS)
test-build-plt:
	@$(MAKE) --no-print-directory test-dir ERLC_OPTS="$(TEST_ERLC_OPTS)"
	$(gen_verbose) touch ebin/test

plt-all: PLT_APPS := $(ALL_TEST_DEPS_DIRS)
plt-all: test-deps test-build-plt plt

dialyze-all: app test-build-plt dialyze
