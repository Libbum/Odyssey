JSTARGETS := iridescence.js iridescence.min.js

.PHONY: clean build rebuild

iridescence.js:
	elm make src/Main.elm --output=iridescence.js --optimize

iridescence.min.js: iridescence.js
	uglifyjs iridescence.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=iridescence.min.js

build: iridescence.min.js

rebuild: clean build

manifest:
	cd manifester; cargo run --release; cd ..

serve:
	elm-live src/Main.elm --open -- --output=iridescence.js --optimize

debug:
	elm-live src/Main.elm --open -- --output=iridescence.js --debug

clean:
	@-rm -f $(JSTARGETS)
