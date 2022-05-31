#!/usr/bin/env bash

set -euo pipefail

PKGNAME="${1:-}"

if [ -z "$PKGNAME" ]; then
    # unable to resolve pkgname-version
    # compile from source
    exit 1
fi

TAG="$(git describe --tags --exact-match 2>/dev/null | head -1)"
URL="https://github.com/emqx/jq/releases/download/$TAG/$PKGNAME"

mkdir -p _packages
PKGFILE="_packages/${PKGNAME}"
if [ ! -f "$PKGFILE" ]; then
    curl -f -L -o "${PKGFILE}" "${URL}"
fi

if [ ! -f "${PKGFILE}.sha256" ]; then
    curl -f -L -o "${PKGFILE}.sha256" "${URL}.sha256"
fi

echo "$(cat "${PKGFILE}.sha256") $PKGFILE" | sha256sum -c || exit 1

tar -xzf "${PKGFILE}" -C .
