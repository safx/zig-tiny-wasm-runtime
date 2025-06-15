test:
	python3 ./run_spectec_tests.py wasm_tests/

build:
	zig build

build-spec-test:
	zig build spec_test

setup-spectec:
	# Clone spectec repository for WebAssembly 2.0 test suite
	@if [ ! -d "spectec" ]; then \
		echo "Cloning spectec repository..."; \
		git clone https://github.com/Wasm-DSL/spectec.git; \
	else \
		echo "Updating spectec repository..."; \
		cd spectec && git pull; \
	fi

setup-tests: setup-spectec
	# Create test directory
	mkdir -p wasm_tests
	# Copy WebAssembly 2.0 core test files (keeping .wast format for our new test runner)
	# Include all WebAssembly 2.0 features including reference types, relaxed-simd, multi-memory, tail calls, exception handling, memory64
	# Only exclude garbage collection which requires advanced runtime support
	find spectec/test/core -name '*.wast' -type f \
		-not -path '*/gc/*' \
		-exec cp {} wasm_tests/ \;
	@echo "Test files copied to wasm_tests/ directory"
	@echo "Run 'make test' to execute the test suite"

clean-tests:
	rm -rf wasm_tests/

clean: clean-tests
	rm -rf zig-out/ .zig-cache/
	find . -name '*.json' -o -name '*.wasm' -type f -delete

help:
	@echo "Available targets:"
	@echo "  build           - Build the WebAssembly interpreter"
	@echo "  build-spec-test - Build the spec test runner"
	@echo "  setup-spectec   - Clone/update spectec repository"
	@echo "  setup-tests     - Setup WebAssembly 2.0 test files"
	@echo "  test            - Run WebAssembly spec tests"
	@echo "  clean           - Clean build artifacts and test files"
	@echo "  clean-tests     - Remove only test files"
	@echo "  help            - Show this help message"

.PHONY: test build build-spec-test setup-spectec setup-tests clean-tests clean help
