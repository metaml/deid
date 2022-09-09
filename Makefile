.DEFAULT_GOAL = build

export SHELL := /bin/sh
export NIX_REMOTE = daemon

PROJECT ?= ${HOME}/${LOGNAME}/proj/deid
OPT     ?=#
PWD      = $(shell pwd)
PORT    ?= 9200

build: clean ## build
	cabal $(OPT) build --minimize-conflict-set --jobs='$$ncpus' | source-highlight --src-lang=haskell --out-format=esc

buildc: clean ## build continuously
	@cabal build 2>&1 | source-highlight --src-lang=haskell --out-format=esc
	@fswatcher --path . --include "\.hs$$|\.cabal$$" --throttle 31 cabal -- $(OPT) build 2>&1 \
	| source-highlight --src-lang=haskell --out-format=esc

install: # install binary
	mkdir -p bin
	cabal build --verbose
	cabal install --overwrite-policy=always --install-method=copy --installdir=bin

dev: ## nix develop
	nix develop

test: ## test
	cabal $(OPT) test

lint: ## lint
	hlint app src

clean: ## clean
	cabal clean

clobber: clean ## cleanpq
	rm -rf dist-newstyle
	rm -rf tmp/*

# make activate KEY_FILE=... first
run: BIN ?= deid
run: export GOOGLE_APPLICATION_CREDENTIALS ?= /Users/milee/.zulu/lpgprj-gss-p-ctrlog-gl-01-5be472e42700.json
run: ## run BIN, e.g. make run BIN=<binary>
	cabal $(OPT) run $(BIN) -- $(ARG)

repl: ## repl
	cabal $(OPT) repl

update: ## update project depedencies
	cabal update

gcp-login: ## login to GCP
	gcloud auth login
	gcloud auth application-default login

# KEY_FILE=~milee/.zulu/lpgprj-gss-p-ctrlog-gl-01-5be472e42700.json
gcp-activate:  ## activate service account--copy and paste "dlp-api" service-account key, KEY_FILE=<dlp-api>.json
	gcloud auth activate-service-account --key-file ${KEY_FILE}

help: ## help
	-@grep --extended-regexp '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
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
	rm -f /nix/var/nix/gcroots/auto/*
	nix-collect-garbage --delete-old

nix-update: ## init/update nix globally
	nix-channel --add https://nixos.org/channels/nixpkgs-unstable unstable
	nix-channel --update
	nix profile upgrade '.*'
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
