git branch -D gh-pages
git push origin --delete gh-pages
git branch gh-pages
git checkout gh-pages

elm make src/Main.elm --optimize --output=elm.js
uglifyjs elm.js --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output=elm.min.js

git add index.html
git add slidelendar.css
git add elm.min.js -f
git add web_assets

git commit -m "deploy"

git push --set-upstream origin gh-pages

git checkout master
