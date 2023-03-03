.DEFAULT_GOAL = build

export SHELL := /bin/sh
export NIX_REMOTE = daemon
export GOOGLE_APPLICATION_CREDENTIALS ?= tmp/lpgprj-gss-p-ctrlog-gl-01-c0096aaa9469.json

PROJECT ?= ${HOME}/${LOGNAME}/proj/deid
OPT     ?=#
PWD     := $(shell pwd)
PORT    ?= 9200

BIN ?= deid

build: clean ## build (default)
	cabal $(OPT) build --minimize-conflict-set --jobs='$$ncpus' | source-highlight --src-lang=haskell --out-format=esc

buildc: clean ## build continuously
	@cabal build 2>&1 | source-highlight --src-lang=haskell --out-format=esc
	@fswatcher --path $(PWD) --include "\.hs$$|\.cabal$$" --throttle 8 cabal -- $(OPT) build 2>&1 \
	| source-highlight --src-lang=haskell --out-format=esc

install: # install binary
	rm -rf bin/* && mkdir -p bin
	cabal build --verbose
	cabal install --overwrite-policy=always --install-method=copy --installdir=bin

dev: ## nix develop
	nix develop

test: ## test
	cabal $(OPT) test

lint: ## lint
	hlint app src

clean: ## clean
	-cabal clean

clobber: clean ## cleanpq
	rm -rf dist-newstyle
	rm -rf tmp/*

# make activate KEY_FILE=... first
run: export GOOGLE_APPLICATION_CREDENTIALS ?= tmp/lpgprj-gss-p-ctrlog-gl-01-c0096aaa9469.json
run: ## run BIN, e.g. make run BIN=<binary>
	cabal run $(BIN) -- $(ARG)

#repl: export GOOGLE_APPLICATION_CREDENTIALS ?= /Users/milee/.zulu/lpgprj-gss-p-ctrlog-gl-01-c0096aaa9469.json
repl: ## repl
	cabal repl

update: ## update nix and cabal project dependencies
update: nix-update-all cabal-update

cabal-update: ## update cabal project depedencies
	nix develop \
	&& cabal update \
	&& exit

flake-update: ## update nix and project dependencies
	nix flake update

gcp-login: ## login to GCP
	gcloud auth login
	gcloud auth application-default login

# KEY_FILE=~milee/.zulu/lpgprj-gss-p-ctrlog-gl-01-5be472e42700.json
gcp-activate:  ## activate service account--copy and paste "dlp-api" service-account key, KEY_FILE=<dlp-api>.json
	gcloud auth activate-service-account --key-file ${KEY_FILE}

deid-csv: MAX_DOCS ?= 10000
deid-csv: TIMESTAMP ?= $(shell date +'%Y-%m-%d-%H%M')
deid-csv: CSV ?= /var/tmp/deid-$(TIMESTAMP).csv
deid-csv: ## write /var/tmp/deid-<timestamp>.csv
	cabal run deid -- --verbose --max=$(MAX_DOCS) | tee $(CSV)

cb-deid: ## find PII data in CB logs in GCP
cb-deid: install cb-ps2csv cb-csv2deid
	wc -l /var/tmp/deid-cb.csv

cb-ps2csv: ## CB pubsub to CSV
	./bin/pubsub2csv --max 1000 | tee /var/tmp/pubsub.csv

cb-csv2deid: ## CB csv to deid (PII)
	cat /var/tmp/pubsub.csv | ./bin/csv2deid | tee /var/tmp/deid-cb.csv

help: ## help
	-@grep --extended-regexp '^[0-9A-Za-z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	  | sed 's/^Makefile://1' \
	  | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-18s\033[0m %s\n", $$1, $$2}'
	-@ghc --version
	-@cabal --version
	-@hlint --version

nix-build: ## build with nix
	nix --print-build-logs build --impure

nix-install: ## install to profile
	nix profile install

nix-clean: ## clean up /nix
	nix-collect-garbage --delete-old

nix-clobber: ## clean up everything: https://nixos.org/guides/nix-pills/garbage-collector.html
	sudo rm -f /nix/var/nix/gcroots/auto/*
	nix-collect-garbage --delete-old

nix-update-all: ## init/update nix globally
	nix-channel --add https://nixos.org/channels/nixpkgs-unstable unstable
	nix-channel --update
	sudo nix profile upgrade '.*'
	nix flake update

docker-shell: ## run docker shell
	docker run \
	  --privileged \
	  --interactive \
	  --tty \
	  --mount type=bind,src=$(PWD),dst=/root/src \
	  --mount type=bind,src=$(PWD)/tmp/cabal,dst=/root/cabal \
	  nix-builder:latest

es-va-proxy: ESC = svor-esc101
es-va-proxy: ## ssh tunnel to an esc server in VA
	ssh -L $(PORT):$(ESC):9200 svvr-mng77

owner-service-pairs: ## distinct pairs of (lp_owner, service_name)
	cabal run os-pairs -- --max=9999 | tee tmp/owner-service.pairs
	sort tmp/owner-service.pairs | uniq | tee tmp/distinct-owner-service.pairs
	cp tmp/distinct-owner-service.pairs etc/owner-service.csv

owner-service-deid: ## run DEID on etc/owner-service.csv
	IFS=$$'\r\n' && for i in $$(sort etc/owner-service-1.csv); do \
		o=$$(echo $$i | awk -F, '{print $$1}'); \
		s=$$(echo $$i | awk -F, '{print $$2}'); \
		echo bin/os-deid --max 1000 --owner $$o --service $$s; \
		bin/os-deid --max 1000 --owner $$o --service $$s | tee -a os-deid.csv; \
	done

.PHONY: test
