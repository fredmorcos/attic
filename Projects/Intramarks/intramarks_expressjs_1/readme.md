# intramarks

Intramarks is a bookmark sharing web application suitable for
individuals, groups and organizations alike.

## global dependencies

- node.js and the node package manager `npm`
- `npm install -g express     # global install`
- `npm install -g supervisor  # nodeJS watcher`
- `npm install -g grunt       # JS build system`
- `gem install sass           # css preprocessor`
- mongoDB

## development setup

- `make init  # will install project dependencies`

## run

Note that each of the following commands does not release the shell
until terminated.

- `make rundb  # will run the DB server`
- `make watch  # on-the-fly reload of server code`
- `make run    # start project web-server`
- visit [http://localhost:3000/](http://localhost:3000/)
