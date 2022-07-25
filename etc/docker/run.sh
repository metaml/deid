#!/bin/sh

set -m

nix-daemon >/dev/null 2>&1 &

/bin/sh
