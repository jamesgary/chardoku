#!/bin/bash
echo "Make sure elm-live isn't running or it may override main.js while deploying!"

# TODO optimize when 0.19 releases
elm-make src/Main.elm --output=public/js/main.js

# make fresh gh-pages branch
git branch -D gh-pages
git checkout --orphan gh-pages
git add -f public/js/main.js
git commit -m "Deploy!"
git push origin :gh-pages && git subtree push --prefix public origin gh-pages
git checkout master
