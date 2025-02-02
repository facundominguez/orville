#!/bin/sh

case "$1" in
  prepare-candidate)
    docker-compose run --rm dev cabal sdist
    docker-compose run --rm dev \
      sh -c \
      'cabal update && \
         cabal \
         v2-haddock \
         --builddir="dist-newstyle/docs" \
         --haddock-for-hackage \
         --enable-doc'
    ;;

  upload-candidate)
    version="$2"

    if [ "$version" = "" ]; then
      echo "Please specify the version number to upload."
      exit 1
    else
      docker-compose run --rm dev \
        sh -c \
        "cabal upload dist-newstyle/sdist/orville-postgresql-$version.tar.gz && \
         cabal upload -d dist-newstyle/docs/orville-postgresql-$version-docs.tar.gz"
    fi

    ;;

  *)
    echo "Unrecognized command: $1"
    exit 1
esac
