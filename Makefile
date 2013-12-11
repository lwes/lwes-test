base-dir=$(shell pwd)
deps-dir=${base-dir}/deps
mvn-repo=${deps-dir}/mvn-repo

all: check

clean:
		rm -fr ${deps-dir}
		(cd erlang; rebar clean)

get-deps: clean
		(mkdir deps; cd deps; git clone https://github.com/lwes/lwes-java;\
			 cd lwes-java; git checkout feature/queasy-support)
		(mkdir ${mvn-repo})
		(cd erlang; rebar get-deps)

build-deps: get-deps
	(cd deps; \
	 cd lwes-java; \
	 mvn -Dmaven.repo.local=${mvn-repo} clean package install \
	 -Dmaven.test.skip=true)

build: build-deps
	cd erlang; rebar compile
	cd java; mvn -Dmaven.repo.local=${mvn-repo} -DskipTests=true clean package

check: build
	(mkdir -p log; python bin/run.py)
