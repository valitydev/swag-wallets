all: build

.PHONY: install
install: node_modules

.PHONY: build
build: install
	npm run build

.PHONY: start
start: install
	npm start

.PHONY: dev
dev:
	wercker dev --publish 3000 --direct-mount

node_modules: package.json
	npm install
	touch $@
