JSTARGETS := dist/assets/js/iridescence.js dist/assets/js/iridescence.min.js

.PHONY: clean build rebuild deploy

dist/assets/js/iridescence.js:
	elm make src/Main.elm --output=dist/assets/js/iridescence.js --optimize

dist/assets/js/iridescence.min.js: dist/assets/js/iridescence.js
	uglifyjs dist/assets/js/iridescence.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=dist/assets/js/iridescence.min.js

prodindex: dist/index.html
	sed -i 's/iridescence.js/iridescence.min.js/' dist/index.html

debugindex: dist/index.html
	sed -i 's/iridescence.min.js/iridescence.js/' dist/index.html

build: dist/assets/js/iridescence.min.js prodindex
	@-rm -f dist/assets/js/iridescence.js

rebuild: clean build

manifest:
	cd manifester; cargo run --release; cd ..

serve: prodindex
	elm-live src/Main.elm -d dist --open -- --output=dist/assets/js/iridescence.js --optimize

debug: debugindex
	elm-live src/Main.elm -d dist --open -- --output=dist/assets/js/iridescence.js --debug

clean:
	@-rm -f $(JSTARGETS)

deploy: build
	rsync -avr --chown=www-data:www-data --checksum --delete -e ssh dist/ AkashaR:odyssey/sandbox/
