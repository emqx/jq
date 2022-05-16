#!/usr/bin/env bash

set -euo pipefail

PKGNAME="$(./pkgname.sh)"
if [ -z "$PKGNAME" ]; then
    # unable to resolve pkgname-version
    # compile from source
    exit 1
fi

TAG="$(git describe --tags --exact-match 2>/dev/null | head -1)"
URL="https://github.com/emqx/jq/releases/download/$TAG/$PKGNAME"

mkdir -p _packages
if [ ! -f "_packages/${PKGNAME}" ]; then
    curl -f -L -o "${PKGNAME}" "${URL}"
fi

if [ ! -f "_packages/${PKGNAME}.sha256" ]; then
    curl -f -L -o "${PKGNAME}.sha256" "${URL}.sha256"
fi

echo "$(cat "_packages/${PKGNAME}.sha256") _packages/${PKGNAME}" | sha256sum -c || exit 1

tar -xzf "_packages/${PKGNAME}" -C .
