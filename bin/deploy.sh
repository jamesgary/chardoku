#!/bin/bash
git checkout gh-pages
git add public/js/main.js
git commit -m "Deploy!"
git push origin :gh-pages && git subtree push --prefix public origin gh-pages
git checkout master
