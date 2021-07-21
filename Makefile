all: build

.PHONY: install
install: node_modules

.PHONY: build
build: install
	npm run build

.PHONY: start
start: install
	npm start

node_modules: package.json
	npm install
	touch $@
