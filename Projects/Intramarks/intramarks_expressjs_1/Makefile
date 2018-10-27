.PHONY: rundb npm init watch run

default: init

watch:
	./watchers.sh

run:
	./run.sh

npm:
	npm install

db:
	mkdir db/

init: npm db

rundb: init
	mongod --smallfiles --dbpath db/

clean:
	rm -rf dist/

deepclean: clean
	rm -rf db/

superclean: deepclean
	rm -rf node_modules/
