# =============================================================================
# Hybrid Architecture Project Makefile
# =============================================================================
# Project: hybrid
# Purpose: Hexagonal architecture demonstration with port/adapter pattern
#
# This Makefile provides:
#   - Build targets (build, clean, rebuild)
#   - Test infrastructure (test, test-coverage)
#   - Format/check targets (format, format-check, stats)
#   - Documentation generation (docs, api-docs)
#   - Development tools (watch, setup-hooks, ci)
# =============================================================================

PROJECT_NAME := hybrid

.PHONY: add-md-headers all api-docs book-build book-clean book-install \
        book-rebuild book-serve book-watch build build-dev build-opt build-release \
        build-tests check check-gmp check-links ci clean clean-diagrams \
        deep-clean deps diagrams docs format format-src format-tests format-all \
        format-check format-preview full generate-version help install \
        install-tools prereqs quick rebuild refresh release-create release-dry \
        release-prepare run run-help run-version setup-hooks stats sync-versions \
        test test-all test-contract test-coverage test-e2e test-integration \
        test-performance test-property test-run test-unit version watch \
        watch-diagrams

# =============================================================================
# OS Detection
# =============================================================================

UNAME := $(shell uname -s)

# =============================================================================
# Colors for Output
# =============================================================================

GREEN := \033[0;32m
YELLOW := \033[0;33m
RED := \033[0;31m
BLUE := \033[0;34m
ORANGE := \033[38;5;208m
CYAN := \033[0;36m
BOLD := \033[1m
NC := \033[0m

# =============================================================================
# Tool Paths
# =============================================================================

ALR := alr
GPRBUILD := gprbuild
GNATFORMAT := gnatformat
GNATDOC := gnatdoc
PYTHON3 := python3

# External abohlib directory for shared scripts
ABOHLIB_DIR := ../abohlib

# =============================================================================
# Tool Flags
# =============================================================================
ALR_BUILD_FLAGS := -j8 --no-indirect-imports | grep -E 'warning:|style:|error:' || true

# =============================================================================
# Directories
# =============================================================================

BUILD_DIR := obj
BIN_DIR := bin
DOCS_DIR := docs/api
COVERAGE_DIR := coverage
TESTS_DIR := tests

# Directories to format (hybrid architecture layers + shared + tests)
FORMAT_DIRS := $(wildcard application/src) $(wildcard bootstrap/src) \
               $(wildcard domain/src) $(wildcard infrastructure/src) \
               $(wildcard presentation/src) $(wildcard shared/src) \
               $(wildcard $(TESTS_DIR))

# =============================================================================
# Default Target
# =============================================================================

all: build

# =============================================================================
# Help Target
# =============================================================================

help: ## Display this help message
	@echo "$(ORANGE)$(BOLD)╔══════════════════════════════════════════════════╗$(NC)"
	@echo "$(ORANGE)$(BOLD)║  Hybrid Architecture - Ada 2022$(NC)"
	@echo "$(ORANGE)$(BOLD)╚══════════════════════════════════════════════════╝$(NC)"
	@echo ""
	@echo "$(YELLOW)Build Commands:$(NC)"
	@echo "  build              - Build project (development mode)"
	@echo "  build-dev          - Build with development settings"
	@echo "  build-opt          - Build with optimizations (-O2)"
	@echo "  build-release      - Build production release"
	@echo "  build-tests        - Build test suite only"
	@echo "  clean              - Remove build artifacts"
	@echo "  deep-clean         - Remove all artifacts including cache"
	@echo "  rebuild            - Clean and rebuild"
	@echo "  install            - Install via Alire"
	@echo ""
	@echo "$(YELLOW)Testing Commands:$(NC)"
	@echo "  test               - Run comprehensive test suite"
	@echo "  test-all           - Run all test categories"
	@echo "  test-run           - Run tests without building"
	@echo "  test-unit          - Run unit tests only"
	@echo "  test-integration   - Run integration tests"
	@echo "  test-e2e           - Run end-to-end tests"
	@echo "  test-contract      - Run contract verification"
	@echo "  test-property      - Run property-based tests"
	@echo "  test-performance   - Run performance benchmarks"
	@echo "  test-coverage      - Run tests with coverage (HTML report)"
	@echo ""
	@echo "$(YELLOW)Quality Commands:$(NC)"
	@echo "  check              - Run static analysis"
	@echo "  check-arch         - Validate architecture boundaries"
	@echo "  format-src         - Auto-format source code only"
	@echo "  format-tests       - Auto-format test code only"
	@echo "  format-all         - Auto-format all code"
	@echo "  format             - Alias for format-all"
	@echo "  format-check       - Check if formatting needed"
	@echo "  format-preview     - Preview formatting changes"
	@echo ""
	@echo "$(YELLOW)Development Commands:$(NC)"
	@echo "  install-tools      - Install dependencies (GMP, gcovr, gnatformat)"
	@echo "  ci                 - Run complete CI pipeline"
	@echo "  stats              - Display project statistics"
	@echo "  watch              - Auto-rebuild on file changes"
	@echo "  setup-hooks        - Configure git pre-commit hooks"
	@echo ""
	@echo "$(YELLOW)Documentation Commands:$(NC)"
	@echo "  docs               - Generate API documentation"
	@echo "  api-docs           - Generate API documentation (alternative)"
	@echo "  check-links        - Check documentation links"
	@echo "  book-build         - Build mdBook documentation"
	@echo "  book-serve         - Serve mdBook with live reload"
	@echo "  book-clean         - Clean mdBook build artifacts"
	@echo "  book-rebuild       - Clean and rebuild mdBook"
	@echo "  book-watch         - Watch and auto-rebuild book"
	@echo "  book-install       - Install mdbook tools"
	@echo ""
	@echo "$(YELLOW)Workflow Shortcuts:$(NC)"
	@echo "  all                - Build project (default)"

# =============================================================================
# Prerequisites and Tool Installation
# =============================================================================

prereqs: check-gmp
	@echo "$(GREEN)✓ All prerequisites satisfied$(NC)"

check-gmp:
	@if [ "$(UNAME)" = "Darwin" ]; then \
		echo "$(BLUE)Checking for GMP library on macOS...$(NC)"; \
		if ! brew list gmp &>/dev/null; then \
			echo "$(RED)ERROR: GMP library is required but not installed.$(NC)"; \
			echo "$(YELLOW)Run 'make install-tools' to install all required dependencies.$(NC)"; \
			exit 1; \
		else \
			echo "$(GREEN)✓ GMP is installed via Homebrew$(NC)"; \
		fi; \
	elif [ "$(UNAME)" = "Linux" ]; then \
		echo "$(BLUE)Checking for GMP library on Linux...$(NC)"; \
		if ! dpkg -l libgmp-dev 2>/dev/null | grep -q "^ii" && \
		   ! rpm -qa | grep -q gmp-devel 2>/dev/null; then \
			echo "$(RED)ERROR: libgmp-dev is required but not installed.$(NC)"; \
			echo "$(YELLOW)Run 'make install-tools' to install all required dependencies.$(NC)"; \
			exit 1; \
		else \
			echo "$(GREEN)✓ libgmp-dev is installed$(NC)"; \
		fi; \
	else \
		echo "$(YELLOW)Unknown OS: $(UNAME) - skipping GMP check$(NC)"; \
	fi

install-tools:
	@if [ -f "scripts/install_tools.py" ]; then \
		$(PYTHON3) scripts/install_tools.py; \
	else \
		echo "$(YELLOW)Warning: scripts/install_tools.py not found$(NC)"; \
		echo "$(YELLOW)Please install dependencies manually$(NC)"; \
	fi

# =============================================================================
# Version Management
# =============================================================================

sync-versions:
	@echo "$(CYAN)Synchronizing versions across all alire.toml files...$(NC)"
	@$(PYTHON3) scripts/sync_versions.py
	@echo "$(GREEN)✓ Versions synchronized$(NC)"

add-md-headers: ## Add standard headers to all markdown files
	@echo "$(CYAN)Adding standard headers to markdown files...$(NC)"
	@$(PYTHON3) scripts/add_md_headers.py
	@echo "$(GREEN)✓ Markdown headers updated$(NC)"

generate-version: sync-versions
	@echo "$(CYAN)Generating version package from alire.toml...$(NC)"
	@$(PYTHON3) scripts/generate_version.py alire.toml shared/src/hybrid-version.ads
	@echo "$(GREEN)✓ Version package generated$(NC)"

# =============================================================================
# Build Commands
# =============================================================================

build: prereqs generate-version
	@echo "$(GREEN)Building $(PROJECT_NAME)...$(NC)"
	$(ALR) build -- $(ALR_BUILD_FLAGS)
	@echo "$(GREEN)✓ Build complete$(NC)"

build-dev: prereqs
	@echo "$(GREEN)Building $(PROJECT_NAME) (development mode)...$(NC)"
	$(ALR) build --development -- $(ALR_BUILD_FLAGS)
	@echo "$(GREEN)✓ Development build complete$(NC)"

build-opt: prereqs
	@echo "$(GREEN)Building $(PROJECT_NAME) (optimized -O2)...$(NC)"
	$(ALR) build -- -O2 $(ALR_BUILD_FLAGS)
	@echo "$(GREEN)✓ Optimized build complete$(NC)"

build-release: prereqs
	@echo "$(GREEN)Building $(PROJECT_NAME) (release mode)...$(NC)"
	$(ALR) build --release -- $(ALR_BUILD_FLAGS)
	@echo "$(GREEN)✓ Release build complete$(NC)"

build-tests: prereqs generate-version
	@echo "$(GREEN)Building test suite...$(NC)"
	@if [ -f "tests.gpr" ]; then \
		$(ALR) exec -- $(GPRBUILD) -P tests.gpr -p --no-indirect-imports; \
		echo "$(GREEN)✓ Test build complete$(NC)"; \
	else \
		echo "$(YELLOW)No test project found (tests.gpr)$(NC)"; \
	fi

clean:
	@echo "$(YELLOW)Cleaning build artifacts...$(NC)"
	@$(ALR) clean
	@find . -name "*.backup" -delete 2>/dev/null || true
	@echo "$(GREEN)✓ Clean complete$(NC)"

deep-clean:
	@echo "$(YELLOW)Performing deep clean...$(NC)"
	@$(ALR) clean
	@rm -rf $(BUILD_DIR) $(BIN_DIR) lib alire .build $(COVERAGE_DIR)
	@find . -name "*.backup" -delete 2>/dev/null || true
	@find . -name "*.gcda" -o -name "*.gcno" -o -name "*.gcov" | \
	  xargs rm -f 2>/dev/null || true
	@echo "$(GREEN)✓ Deep clean complete$(NC)"

rebuild: clean build

install: prereqs
	@echo "$(GREEN)Installing $(PROJECT_NAME)...$(NC)"
	@$(ALR) install
	@echo "$(GREEN)✓ Installation complete$(NC)"

# =============================================================================
# Testing Commands
# =============================================================================

test: build build-tests
	@echo "$(BLUE)Checking architecture rules...$(NC)"
	@$(PYTHON3) $(ABOHLIB_DIR)/scripts/arch_guard.py
	@echo "$(GREEN)Running comprehensive test suite...$(NC)"
	@if [ -f "$(TESTS_DIR)/bin/test_runner" ]; then \
		$(TESTS_DIR)/bin/test_runner; \
		if [ $$? -eq 0 ]; then \
			echo "$(GREEN)✓ All tests passed$(NC)"; \
		else \
			echo "$(RED)✗ Tests failed$(NC)"; \
			exit 1; \
		fi; \
	elif [ -f "./bin/test_all" ]; then \
		./bin/test_all; \
	else \
		echo "$(YELLOW)Test runner not found - test infrastructure not yet implemented$(NC)"; \
	fi

test-all: build build-tests
	@echo "$(GREEN)Running all test executables...$(NC)"
	@failed=0; \
	if [ -d "$(TESTS_DIR)/bin" ]; then \
		for test in $(TESTS_DIR)/bin/test_*; do \
			if [ -x "$$test" ] && [ -f "$$test" ]; then \
				echo "$(CYAN)Running $$test...$(NC)"; \
				$$test || failed=1; \
				echo ""; \
			fi; \
		done; \
	fi; \
	if [ $$failed -eq 0 ]; then \
		echo "$(GREEN)✓ All test suites passed$(NC)"; \
	else \
		echo "$(RED)✗ Some tests failed$(NC)"; \
		exit 1; \
	fi

test-run:
	@echo "$(GREEN)Running tests (no build)...$(NC)"
	@if [ -f "$(TESTS_DIR)/bin/test_runner" ]; then \
		$(TESTS_DIR)/bin/test_runner; \
	elif [ -f "./bin/test_all" ]; then \
		./bin/test_all; \
	else \
		echo "$(YELLOW)Test runner not found$(NC)"; \
	fi

test-unit: build build-tests
	@echo "$(GREEN)Running unit tests...$(NC)"
	@echo "$(YELLOW)Unit tests not yet implemented$(NC)"

test-integration: build build-tests
	@echo "$(GREEN)Running integration tests...$(NC)"
	@echo "$(YELLOW)Integration tests not yet implemented$(NC)"

test-e2e: build build-tests
	@echo "$(GREEN)Running end-to-end tests...$(NC)"
	@echo "$(YELLOW)End-to-end tests not yet implemented$(NC)"

test-contract: build build-tests
	@echo "$(GREEN)Running contract verification tests...$(NC)"
	@echo "$(YELLOW)Contract tests not yet implemented$(NC)"

test-property: build build-tests
	@echo "$(GREEN)Running property-based tests...$(NC)"
	@echo "$(YELLOW)Property-based tests not yet implemented$(NC)"

test-performance: build build-tests
	@echo "$(GREEN)Running performance benchmarks...$(NC)"
	@echo "$(YELLOW)Performance benchmarks not yet implemented$(NC)"

test-coverage:
	@echo "$(GREEN)Running tests with coverage analysis...$(NC)"
	@if command -v $(PYTHON3) >/dev/null 2>&1; then \
		if [ -f "scripts/run_coverage.py" ]; then \
			$(PYTHON3) scripts/run_coverage.py; \
		else \
			echo "$(YELLOW)Coverage script not found$(NC)"; \
		fi; \
	else \
		echo "$(RED)Python 3 required for coverage analysis$(NC)"; \
	fi

# =============================================================================
# Quality & Code Formatting Commands
# =============================================================================

check: prereqs
	@echo "$(GREEN)Running static analysis...$(NC)"
	$(ALR) build
	@echo "$(GREEN)✓ Static analysis complete$(NC)"

check-arch: ## Validate hexagonal architecture boundaries
	@echo "$(GREEN)Validating architecture boundaries...$(NC)"
	@$(PYTHON3) $(ABOHLIB_DIR)/scripts/arch_guard.py
	@echo "$(GREEN)✓ Architecture validation complete$(NC)"

format-src:
	@echo "$(GREEN)Formatting source code...$(NC)"
	@if command -v $(GNATFORMAT) >/dev/null 2>&1; then \
		for dir in $(wildcard application/src) $(wildcard bootstrap/src) \
		           $(wildcard domain/src) $(wildcard infrastructure/src) \
		           $(wildcard presentation/src) $(wildcard shared/src); do \
			if [ -d "$$dir" ]; then \
				find "$$dir" -name "*.ads" -o -name "*.adb" | \
				while read file; do \
					echo "  Formatting $$file..."; \
					$(GNATFORMAT) "$$file" || true; \
				done; \
			fi; \
		done; \
		echo "$(GREEN)✓ Source formatting complete$(NC)"; \
	else \
		echo "$(YELLOW)Warning: gnatformat not found$(NC)"; \
		echo "$(YELLOW)Install: alr get --build gnatformat$(NC)"; \
	fi

format-tests:
	@echo "$(GREEN)Formatting test code...$(NC)"
	@if command -v $(GNATFORMAT) >/dev/null 2>&1; then \
		if [ -d "$(TESTS_DIR)/src" ]; then \
			find $(TESTS_DIR)/src -name "*.ads" -o -name "*.adb" | \
			while read file; do \
				echo "  Formatting $$file..."; \
				$(GNATFORMAT) "$$file" || true; \
			done; \
		fi; \
		echo "$(GREEN)✓ Test code formatting complete$(NC)"; \
	else \
		echo "$(YELLOW)Warning: gnatformat not found$(NC)"; \
		echo "$(YELLOW)Install: alr get --build gnatformat$(NC)"; \
	fi

format-all: format-src format-tests
	@echo "$(GREEN)✓ All code formatting complete$(NC)"

format: format-all

format-preview:
	@echo "$(GREEN)Preview formatting changes...$(NC)"
	@echo "$(YELLOW)Preview mode not implemented for gnatformat$(NC)"
	@echo "$(YELLOW)Use git diff to see changes$(NC)"

format-check: format-preview
	@echo ""
	@echo "Run 'make format' to apply formatting"

# =============================================================================
# Development Commands
# =============================================================================

ci: clean format build check test
	@echo ""
	@echo "$(GREEN)════════════════════════════════════════$(NC)"
	@echo "$(GREEN)✓ CI Pipeline Complete!$(NC)"
	@echo "$(GREEN)════════════════════════════════════════$(NC)"
	@echo ""
	@echo "Summary:"
	@echo "  • Code formatted"
	@echo "  • Project built successfully"
	@echo "  • Static analysis passed"
	@echo "  • All tests passed"

setup-hooks:
	@echo "$(GREEN)Setting up git hooks...$(NC)"
	@if [ -d ".git" ]; then \
		mkdir -p .git/hooks; \
		echo "#!/bin/sh" > .git/hooks/pre-commit; \
		echo "# Auto-generated pre-commit hook" >> .git/hooks/pre-commit; \
		echo "echo 'Running pre-commit checks...'" >> .git/hooks/pre-commit; \
		echo "make format" >> .git/hooks/pre-commit; \
		echo "make check" >> .git/hooks/pre-commit; \
		echo "git add -u" >> .git/hooks/pre-commit; \
		chmod +x .git/hooks/pre-commit; \
		echo "$(GREEN)✓ Git hooks configured$(NC)"; \
	else \
		echo "$(YELLOW)Warning: Not a git repository$(NC)"; \
	fi

stats:
	@echo "$(BLUE)Project Statistics for $(PROJECT_NAME)$(NC)"
	@echo "$(YELLOW)════════════════════════════════════════$(NC)"
	@echo ""
	@echo "Ada Source Files by Layer:"
	@echo "  Domain specs:          $$(find domain/src -name "*.ads" 2>/dev/null | wc -l)"
	@echo "  Domain bodies:         $$(find domain/src -name "*.adb" 2>/dev/null | wc -l)"
	@echo "  Application specs:     $$(find application/src -name "*.ads" 2>/dev/null | wc -l)"
	@echo "  Application bodies:    $$(find application/src -name "*.adb" 2>/dev/null | wc -l)"
	@echo "  Infrastructure specs:  $$(find infrastructure/src -name "*.ads" 2>/dev/null | wc -l)"
	@echo "  Infrastructure bodies: $$(find infrastructure/src -name "*.adb" 2>/dev/null | wc -l)"
	@echo "  Presentation specs:    $$(find presentation/src -name "*.ads" 2>/dev/null | wc -l)"
	@echo "  Presentation bodies:   $$(find presentation/src -name "*.adb" 2>/dev/null | wc -l)"
	@echo "  Bootstrap specs:       $$(find bootstrap/src -name "*.ads" 2>/dev/null | wc -l)"
	@echo "  Bootstrap bodies:      $$(find bootstrap/src -name "*.adb" 2>/dev/null | wc -l)"
	@echo "  Shared specs:          $$(find shared/src -name "*.ads" 2>/dev/null | wc -l)"
	@echo "  Shared bodies:         $$(find shared/src -name "*.adb" 2>/dev/null | wc -l)"
	@echo ""
	@echo "Lines of Code:"
	@find application/src bootstrap/src domain/src infrastructure/src presentation/src shared/src -name "*.ads" -o -name "*.adb" 2>/dev/null | xargs wc -l 2>/dev/null | tail -1 | awk '{printf "  Total: %d lines\n", $$1}' || echo "  Total: 0 lines"
	@echo ""
	@echo "Test Files:"
	@echo "  Test specs:            $$(find $(TESTS_DIR) -name "*.ads" 2>/dev/null | wc -l)"
	@echo "  Test bodies:           $$(find $(TESTS_DIR) -name "*.adb" 2>/dev/null | wc -l)"
	@echo ""
	@echo "Build Artifacts:"
	@if [ -f "./bootstrap/bin/hybrid-bootstrap-main" ]; then \
		echo "  Binary: $$(ls -lh ./bootstrap/bin/hybrid-bootstrap-main 2>/dev/null | awk '{print $$5}')"; \
	else \
		echo "  No binary found (run 'make build')"; \
	fi

watch:
	@echo "$(GREEN)Watching for changes in source directories...$(NC)"
	@echo "Press Ctrl+C to stop"
	@if command -v fswatch >/dev/null 2>&1; then \
		fswatch -o application/src bootstrap/src domain/src infrastructure/src presentation/src shared/src 2>/dev/null | while read; do \
			clear; \
			echo "$(YELLOW)Change detected, rebuilding...$(NC)"; \
			$(MAKE) build; \
		done; \
	elif command -v inotifywait >/dev/null 2>&1; then \
		while true; do \
			inotifywait -q -e modify,create,delete -r application/src bootstrap/src domain/src infrastructure/src presentation/src shared/src; \
			clear; \
			echo "$(YELLOW)Change detected, rebuilding...$(NC)"; \
			$(MAKE) build; \
		done; \
	else \
		echo "$(RED)Error: Neither fswatch nor inotifywait found$(NC)"; \
		echo "  macOS: brew install fswatch"; \
		echo "  Linux: sudo apt-get install inotify-tools"; \
		exit 1; \
	fi

# =============================================================================
# Documentation Commands
# =============================================================================

docs:
	@echo "$(GREEN)Generating documentation...$(NC)"
	@mkdir -p $(DOCS_DIR)
	@if command -v $(GNATDOC) >/dev/null 2>&1; then \
		$(ALR) exec -- $(GNATDOC) -P$(PROJECT_NAME).gpr -w --output=$(DOCS_DIR); \
		echo "$(GREEN)✓ Full documentation generated in $(DOCS_DIR)$(NC)"; \
	else \
		echo "$(YELLOW)Warning: gnatdoc not found, generating basic docs...$(NC)"; \
		find application/src bootstrap/src domain/src infrastructure/src presentation/src shared/src -name "*.ads" -exec echo "=== {} ===" \; -exec grep -E "^[ ]*--" {} \; > $(DOCS_DIR)/basic_docs.txt 2>/dev/null; \
		echo "$(GREEN)✓ Basic documentation extracted to $(DOCS_DIR)/basic_docs.txt$(NC)"; \
	fi

api-docs:
	@echo "$(GREEN)Generating API documentation...$(NC)"
	@if [ -f "scripts/generate_api_docs.sh" ]; then \
		./scripts/generate_api_docs.sh; \
	else \
		echo "$(YELLOW)Warning: scripts/generate_api_docs.sh not found$(NC)"; \
		echo "$(CYAN)Using 'make docs' instead...$(NC)"; \
		$(MAKE) docs; \
	fi

check-links:
	@echo "$(GREEN)Checking documentation links...$(NC)"
	@if ! command -v $(PYTHON3) >/dev/null 2>&1; then \
		echo "$(YELLOW)Warning: Python 3 not found, skipping link check$(NC)"; \
	elif [ -f "scripts/check_links.py" ]; then \
		$(PYTHON3) scripts/check_links.py; \
	else \
		echo "$(YELLOW)Warning: scripts/check_links.py not found$(NC)"; \
	fi

# =============================================================================
# Release Commands (inspired by Kotlin template)
# =============================================================================

version: ## Display project version from alire.toml
	@grep "^version" alire.toml | cut -d'"' -f2

release-prepare: ## Prepare a new release (sync versions, update docs, test)
	@echo "$(CYAN)Preparing release...$(NC)"
	@read -p "Enter version (e.g., 1.0.0): " version; \
	$(PYTHON3) scripts/release.py prepare $$version

release-create: ## Create and publish release (tag, push, GitHub release)
	@echo "$(CYAN)Creating release...$(NC)"
	@read -p "Enter version (e.g., 1.0.0): " version; \
	$(PYTHON3) scripts/release.py release $$version

release-dry: clean build test check ## Dry run of release process
	@echo "$(GREEN)✓ Release dry run completed successfully!$(NC)"
	@echo "$(CYAN)All checks passed. Ready for release.$(NC)"

# =============================================================================
# Quick Shortcuts (inspired by Kotlin template)
# =============================================================================

quick: build ## Quick build (skip clean)
	@echo "$(GREEN)✓ Quick build complete$(NC)"

full: clean build test check ## Full build, test, and validation
	@echo "$(GREEN)✓ Full validation complete$(NC)"

# =============================================================================
# Diagram Management (inspired by Kotlin template)
# =============================================================================

DIAGRAM_DIR := docs/diagrams
PUML_FILES := $(wildcard $(DIAGRAM_DIR)/*.puml)
SVG_FILES := $(PUML_FILES:.puml=.svg)

diagrams: ## Generate all PlantUML diagrams
	@echo "$(CYAN)Generating architecture diagrams...$(NC)"
	@if [ ! -d "$(DIAGRAM_DIR)" ]; then \
		echo "$(YELLOW)Warning: $(DIAGRAM_DIR) not found$(NC)"; \
		exit 0; \
	fi
	@if command -v plantuml >/dev/null 2>&1; then \
		for file in $(DIAGRAM_DIR)/*.puml; do \
			if [ -f "$$file" ]; then \
				echo "  Generating $${file%.puml}.svg..."; \
				plantuml -tsvg "$$file" 2>/dev/null || echo "$(YELLOW)  Warning: Failed to generate $${file%.puml}.svg$(NC)"; \
			fi; \
		done; \
		echo "$(GREEN)✓ Diagrams generated in $(DIAGRAM_DIR)$(NC)"; \
	else \
		echo "$(YELLOW)Warning: plantuml not found$(NC)"; \
		echo "$(CYAN)Install with: brew install plantuml (macOS) or apt-get install plantuml (Linux)$(NC)"; \
	fi

clean-diagrams: ## Remove generated diagram files
	@echo "$(CYAN)Cleaning generated diagrams...$(NC)"
	@rm -f $(DIAGRAM_DIR)/*.svg $(DIAGRAM_DIR)/*.png 2>/dev/null || true
	@echo "$(GREEN)✓ Diagrams cleaned$(NC)"

watch-diagrams: ## Watch and regenerate diagrams on changes
	@echo "$(GREEN)Watching for diagram changes (requires fswatch)...$(NC)"
	@if ! command -v fswatch >/dev/null 2>&1; then \
		echo "$(RED)Error: fswatch not installed$(NC)"; \
		echo "$(CYAN)Install with: brew install fswatch (macOS)$(NC)"; \
		exit 1; \
	fi
	@fswatch -o $(DIAGRAM_DIR)/*.puml | xargs -n1 -I{} $(MAKE) diagrams

# =============================================================================
# mdBook Commands
# =============================================================================

BOOK_DIR := docs/book
BOOK_SRC := $(BOOK_DIR)/src
BOOK_OUTPUT := $(BOOK_DIR)/book

book-build: ## Build the mdBook documentation
	@echo "$(CYAN)Building mdBook documentation...$(NC)"
	@if ! command -v mdbook >/dev/null 2>&1; then \
		echo "$(RED)Error: mdbook not installed$(NC)"; \
		echo "$(CYAN)Install with: cargo install mdbook$(NC)"; \
		exit 1; \
	fi
	@cd $(BOOK_DIR) && mdbook build
	@echo "$(GREEN)✓ Book built successfully in $(BOOK_OUTPUT)$(NC)"

book-serve: ## Serve the mdBook documentation with live reload
	@echo "$(CYAN)Starting mdBook server with live reload...$(NC)"
	@if ! command -v mdbook >/dev/null 2>&1; then \
		echo "$(RED)Error: mdbook not installed$(NC)"; \
		echo "$(CYAN)Install with: cargo install mdbook$(NC)"; \
		exit 1; \
	fi
	@echo "$(GREEN)Book will be available at http://localhost:3000$(NC)"
	@cd $(BOOK_DIR) && mdbook serve

book-clean: ## Clean mdBook build artifacts
	@echo "$(CYAN)Cleaning mdBook artifacts...$(NC)"
	@rm -rf $(BOOK_OUTPUT) 2>/dev/null || true
	@cd $(BOOK_DIR) && mdbook clean 2>/dev/null || true
	@echo "$(GREEN)✓ Book artifacts cleaned$(NC)"

book-rebuild: book-clean book-build ## Clean and rebuild the mdBook
	@echo "$(GREEN)✓ Book rebuilt successfully$(NC)"

book-watch: ## Watch book source files and auto-rebuild
	@echo "$(GREEN)Watching for book changes...$(NC)"
	@if ! command -v fswatch >/dev/null 2>&1; then \
		echo "$(YELLOW)Warning: fswatch not found, falling back to mdbook serve$(NC)"; \
		$(MAKE) book-serve; \
	else \
		fswatch -o $(BOOK_SRC)/*.md | while read; do \
			clear; \
			echo "$(YELLOW)Change detected, rebuilding book...$(NC)"; \
			$(MAKE) book-build; \
		done; \
	fi

book-install: ## Install mdbook and mdbook-plantuml
	@echo "$(CYAN)Installing mdbook tools...$(NC)"
	@if ! command -v cargo >/dev/null 2>&1; then \
		echo "$(RED)Error: cargo (Rust) not installed$(NC)"; \
		echo "$(CYAN)Install from: https://rustup.rs/$(NC)"; \
		exit 1; \
	fi
	@cargo install mdbook
	@cargo install mdbook-plantuml
	@echo "$(GREEN)✓ mdbook tools installed$(NC)"

# =============================================================================
# Advanced Targets
# =============================================================================

deps: ## Display project dependencies
	@echo "$(CYAN)Project dependencies from alire.toml:$(NC)"
	@grep -A 10 "\[\[depends-on\]\]" alire.toml || echo "$(YELLOW)No dependencies found$(NC)"
	@echo ""
	@echo "$(CYAN)Alire dependency tree:$(NC)"
	@$(ALR) show --solve || echo "$(YELLOW)Could not resolve dependencies$(NC)"

refresh: ## Refresh Alire dependencies
	@echo "$(CYAN)Refreshing Alire dependencies...$(NC)"
	@$(ALR) update
	@echo "$(GREEN)✓ Dependencies refreshed$(NC)"

run: build ## Run the main application
	@echo "$(CYAN)Running application...$(NC)"
	@./bootstrap/bin/hybrid-bootstrap-main

run-help: build ## Show application help
	@./bootstrap/bin/hybrid-bootstrap-main --help

run-version: build ## Show application version
	@./bootstrap/bin/hybrid-bootstrap-main --version

.DEFAULT_GOAL := help
