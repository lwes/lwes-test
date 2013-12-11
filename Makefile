base-dir := `pwd`
deps-dir := ${base-dir}/deps
mvn-repo := ${deps-dir}/mvn-repo

clean:
		rm -fr ${deps-dir}

get-deps: clean
		(mkdir deps; cd deps; git clone https://github.com/lwes/lwes-java;\
			 cd lwes-java; git checkout feature/queasy-support)
		(mkdir ${mvn-repo})
		(cd erlang; rebar get-deps)

build-deps: get-deps
		(cd deps; cd lwes-java; mvn -Dmaven.repo.local=../mvn-repo -DskipTests=true clean package install)

build: build-deps
		cd erlang; rebar compile
		cd java; mvn -Dmaven.repo.local=../deps/mvn-repo -DskipTests=true clean package

check: build
		(mkdir -p log; python bin/run.py)
