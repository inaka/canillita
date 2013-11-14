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

scale:
	tsung -f `pwd`/priv/tsung.xml -l log/tsung/ -F start
	cd log/tsung/`ls log/tsung/ | sort -r | head -n1` && \
	/usr/local/lib/tsung/bin/tsung_stats.pl && \
	open report.html && \
	cd -
