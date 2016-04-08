PROJECT = canillita

CONFIG ?= test/test.config

DEPS = sumo_rest lasse katana swagger sumo_db trails
SHELL_DEPS = sync
TEST_DEPS = shotgun mixer katana_test
BUILD_DEPS = inaka_mk
LOCAL_DEPS = tools compiler syntax_tools common_test inets test_server
LOCAL_DEPS += dialyzer wx

dep_sumo_rest = git https://github.com/inaka/sumo_rest.git 0.1.2
dep_lasse = git https://github.com/inaka/lasse.git 1.0.1
dep_sync = git https://github.com/rustyio/sync.git 9c78e7b
dep_katana = git https://github.com/inaka/erlang-katana.git 0.2.23
dep_shotgun = git https://github.com/inaka/shotgun.git 0.2.3
dep_mixer = git https://github.com/inaka/mixer.git 0.1.5
dep_swagger = git https://github.com/inaka/cowboy-swagger.git 1.0.2
dep_sumo_db = git https://github.com/inaka/sumo_db.git 0.4.0
dep_trails = git https://github.com/inaka/cowboy-trails.git 0.1.1
dep_inaka_mk = git https://github.com/inaka/inaka.mk.git 1.0.0
dep_katana_test = git https://github.com/inaka/katana-test.git 0.0.6

DEP_PLUGINS = inaka_mk

include erlang.mk

CT_OPTS = -cover test/canillita.coverspec -erl_args -config ${CONFIG}

SHELL_OPTS = -s sync -config ${CONFIG} -s lager
