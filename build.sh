#!/usr/bin/env bash

set -euo pipefail

PKGNAME="$(./pkgname.sh)"

if [ "${BUILD_RELEASE:-}" != 1 ] && [ -n "$PKGNAME" ]; then
    if ./download.sh "$PKGNAME"; then
        echo "JQ: done_dowloading_jq_build_cache"
        exit 0
    else
        echo "JQ: failed_to_download_jq_build_cache, continue to build from source"
    fi
fi

make -C c_src

if [ "${BUILD_RELEASE:-}" = 1 ]; then
    mkdir -p _packages
    TARGET="_packages/${PKGNAME}"
    cp c_src/libs/jqc/COPYING priv/
    tar -czf "$TARGET" ./priv
    # use openssl but not sha256sum command because in some macos env it does not exist
    if command -v openssl; then
        openssl dgst -sha256 "${TARGET}" | cut -d ' ' -f 2  > "${TARGET}.sha256"
    else
        sha256sum "${TARGET}" | cut -d ' ' -f 1 > "${TARGET}.sha256"
    fi
fi
