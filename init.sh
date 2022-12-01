#/bin/bash

if [ "$#" -ne 1 ]
then
    echo "Usage: $0 <day number>"
    exit 1
fi

DIR="d$1"
mkdir -p $DIR

pushd $DIR

cabal init --non-interactive --minimal --author="thiel" --email=""
rm CHANGELOG.md
