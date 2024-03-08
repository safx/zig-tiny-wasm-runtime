test:
	./run_spec_tests.sh

setup:
	git clone https://github.com/WebAssembly/testsuite
	curl --output-dir testsuite -sLO "https://raw.githubusercontent.com/WebAssembly/testsuite/da56298dddb441d1af38492ee98fe001e625d156/if.wast"
	curl --output-dir testsuite -sLO "https://raw.githubusercontent.com/WebAssembly/testsuite/da56298dddb441d1af38492ee98fe001e625d156/comments.wast"
	find testsuite -depth 1 \( -iname '*.wast' -and -not -iname 'simd*.wast' \) -print0 | xargs -0 -n 1 wast2json
	find . \( -iname '*.wat' \) -delete
	curl -sLO 'https://raw.githubusercontent.com/r1ru/tiny-wasm-runtime/main/test/spectest.wat'
	wat2wasm spectest.wat

clean:
	rm -fr testsuite/
	find . \( -iname '*.json' -or -iname '*.wasm' -or -iname '*.wat' \) -delete
