NODE ?= canillita
REBAR ?= "./rebar"
CONFIG ?= "priv/app.config"
RUN := erl -pa ebin -pa deps/*/ebin -smp enable -s lager -boot start_sasl -config ${CONFIG} ${ERL_ARGS}

all:
	${REBAR} get-deps compile

quick:
	${REBAR} skip_deps=true compile

clean:
	${REBAR} clean

quick_clean:
	${REBAR} skip_deps=true clean

run: quick
	if [ -n "${NODE}" ]; then ${RUN} -name ${NODE}@`hostname` -s canillita; \
	else ${RUN} -s canillita; \
	fi
