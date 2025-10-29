#!/usr/bin/env python3
# =============================================================================
# Comprehensive Architecture Guard - Hexagonal Architecture Validator
# =============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# See LICENSE file in the project root.
#
# Purpose:
#   Comprehensive validation of hexagonal architecture layer boundaries.
#   Ensures all layers only import their allowed dependencies.
#
# Architecture Rules:
#   Domain       → Can import: Shared only
#   Application  → Can import: Domain, Shared
#   Infrastructure → Can import: Domain, Application, Shared
#   Presentation → Can import: Application, Shared
#   Bootstrap    → Can import: All layers (composition root)
#
# Usage:
#   python3 scripts/arch_guard_comprehensive.py
#   python3 scripts/arch_guard_comprehensive.py --verbose
#
# Returns:
#   0 - No violations found
#   1 - Invalid arguments
#   2 - Architecture violations detected
# =============================================================================

import os
import re
import sys
from typing import Dict, List, Set, Tuple
from collections import defaultdict

# ANSI colors for terminal output
class Colors:
    GREEN = '\033[92m'
    RED = '\033[91m'
    YELLOW = '\033[93m'
    BLUE = '\033[94m'
    RESET = '\033[0m'
    BOLD = '\033[1m'

# Architecture rules: layer_name -> (allowed_imports, forbidden_imports)
ARCHITECTURE_RULES: Dict[str, Tuple[Set[str], Set[str]]] = {
    "domain": (
        {"Hybrid.Domain", "Hybrid.Shared", "Ada", "Interfaces", "System"},
        {"Hybrid.Application", "Hybrid.Infrastructure", "Hybrid.Presentation", "Hybrid.Bootstrap"}
    ),
    "application": (
        {"Hybrid.Application", "Hybrid.Domain", "Hybrid.Shared", "Ada", "Interfaces", "System"},
        {"Hybrid.Infrastructure", "Hybrid.Presentation", "Hybrid.Bootstrap"}
    ),
    "infrastructure": (
        {"Hybrid.Infrastructure", "Hybrid.Application", "Hybrid.Domain", "Hybrid.Shared",
         "Ada", "Interfaces", "System"},
        {"Hybrid.Presentation", "Hybrid.Bootstrap"}
    ),
    "presentation": (
        {"Hybrid.Presentation", "Hybrid.Application", "Hybrid.Shared", "Ada", "Interfaces", "System"},
        {"Hybrid.Domain", "Hybrid.Infrastructure", "Hybrid.Bootstrap"}
    ),
    "bootstrap": (
        # Bootstrap is composition root - can import anything
        {"Hybrid.Bootstrap", "Hybrid.Presentation", "Hybrid.Application",
         "Hybrid.Infrastructure", "Hybrid.Domain", "Hybrid.Shared",
         "Ada", "Interfaces", "System"},
        set()  # No forbidden imports
    ),
}

class ArchitectureViolation:
    """Represents a single architecture violation."""
    def __init__(self, file_path: str, layer: str, forbidden_import: str, line_num: int = 0):
        self.file_path = file_path
        self.layer = layer
        self.forbidden_import = forbidden_import
        self.line_num = line_num

    def __str__(self) -> str:
        location = f"{self.file_path}:{self.line_num}" if self.line_num else self.file_path
        return f"   {location}\n      └─> imports {self.forbidden_import}"

def get_project_root() -> str:
    """Get the project root directory."""
    return os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

def get_layer_directory(root: str, layer: str) -> str:
    """Get the directory path for a layer."""
    return os.path.join(root, layer)

def extract_imports(file_content: str) -> List[Tuple[str, int]]:
    """
    Extract all 'with' imports from Ada source file content.
    Returns list of (import_name, line_number) tuples.
    """
    imports = []
    lines = file_content.split('\n')

    for line_num, line in enumerate(lines, 1):
        # Match: with Package.Name;
        # Handles multiple packages: with A, B, C;
        match = re.search(r'^\s*with\s+([\w.]+(?:\s*,\s*[\w.]+)*)\s*;', line)
        if match:
            # Split comma-separated imports
            import_list = match.group(1).split(',')
            for imp in import_list:
                imports.append((imp.strip(), line_num))

    return imports

def is_import_allowed(import_name: str, layer: str) -> Tuple[bool, str]:
    """
    Check if an import is allowed for the given layer.
    Returns (is_allowed, violated_forbidden_pattern).
    """
    allowed, forbidden = ARCHITECTURE_RULES.get(layer, (set(), set()))

    # Check if import matches any allowed pattern
    is_allowed = any(import_name.startswith(pattern) for pattern in allowed)

    if not is_allowed:
        return False, "unknown"

    # Check if import matches any forbidden pattern
    for forbidden_pattern in forbidden:
        if import_name.startswith(forbidden_pattern):
            return False, forbidden_pattern

    return True, ""

def scan_layer(root: str, layer: str, verbose: bool = False) -> List[ArchitectureViolation]:
    """
    Scan all Ada files in a layer for architecture violations.
    Returns list of violations found.
    """
    violations = []
    layer_dir = get_layer_directory(root, layer)

    if not os.path.exists(layer_dir):
        if verbose:
            print(f"{Colors.YELLOW}⚠ Layer directory not found: {layer_dir}{Colors.RESET}")
        return violations

    file_count = 0
    for dirpath, _, filenames in os.walk(layer_dir):
        for filename in filenames:
            if not filename.endswith((".ads", ".adb")):
                continue

            file_path = os.path.join(dirpath, filename)
            file_count += 1

            try:
                with open(file_path, "r", encoding="utf-8", errors="ignore") as f:
                    content = f.read()
            except Exception as e:
                if verbose:
                    print(f"{Colors.YELLOW}⚠ Could not read {file_path}: {e}{Colors.RESET}")
                continue

            imports = extract_imports(content)
            for import_name, line_num in imports:
                is_allowed, violated_pattern = is_import_allowed(import_name, layer)

                if not is_allowed and violated_pattern != "unknown":
                    violations.append(
                        ArchitectureViolation(file_path, layer, import_name, line_num)
                    )

    if verbose:
        print(f"{Colors.BLUE}ℹ Scanned {file_count} files in {layer} layer{Colors.RESET}")

    return violations

def print_summary(violations_by_layer: Dict[str, List[ArchitectureViolation]], verbose: bool):
    """Print a summary of architecture validation results."""
    total_violations = sum(len(v) for v in violations_by_layer.values())

    print(f"\n{Colors.BOLD}{'='*70}{Colors.RESET}")
    print(f"{Colors.BOLD}Hexagonal Architecture Validation Report{Colors.RESET}")
    print(f"{Colors.BOLD}{'='*70}{Colors.RESET}\n")

    # Print layer-by-layer results
    for layer in ["domain", "application", "infrastructure", "presentation", "bootstrap"]:
        violations = violations_by_layer.get(layer, [])
        layer_name = layer.capitalize()

        if violations:
            print(f"{Colors.RED}✗ {layer_name}: {len(violations)} violation(s) found{Colors.RESET}")

            # Group violations by forbidden import pattern
            by_pattern = defaultdict(list)
            for v in violations:
                pattern = v.forbidden_import.split('.')[1] if '.' in v.forbidden_import else v.forbidden_import
                by_pattern[pattern].append(v)

            for pattern, viols in by_pattern.items():
                print(f"\n  {Colors.YELLOW}⚠ {layer_name} → {pattern} (forbidden){Colors.RESET}")
                for v in viols:
                    print(str(v))
            print()
        else:
            print(f"{Colors.GREEN}✓ {layer_name}: No violations{Colors.RESET}")

    print(f"\n{Colors.BOLD}{'='*70}{Colors.RESET}")

    if total_violations == 0:
        print(f"{Colors.GREEN}{Colors.BOLD}✅ SUCCESS: Architecture boundaries intact!{Colors.RESET}")
        print(f"{Colors.GREEN}All {len(ARCHITECTURE_RULES)} layers comply with hexagonal architecture rules.{Colors.RESET}\n")
    else:
        print(f"{Colors.RED}{Colors.BOLD}❌ FAILURE: {total_violations} architecture violation(s) detected!{Colors.RESET}")
        print(f"{Colors.YELLOW}Fix violations to maintain clean hexagonal architecture.{Colors.RESET}\n")

def main():
    """Main entry point."""
    verbose = "--verbose" in sys.argv or "-v" in sys.argv

    if "--help" in sys.argv or "-h" in sys.argv:
        print(__doc__)
        sys.exit(0)

    root = get_project_root()

    if verbose:
        print(f"{Colors.BLUE}ℹ Project root: {root}{Colors.RESET}")
        print(f"{Colors.BLUE}ℹ Validating {len(ARCHITECTURE_RULES)} layers...\n{Colors.RESET}")

    # Scan all layers
    violations_by_layer = {}
    for layer in ARCHITECTURE_RULES.keys():
        violations = scan_layer(root, layer, verbose)
        if violations:
            violations_by_layer[layer] = violations

    # Print results
    print_summary(violations_by_layer, verbose)

    # Exit with appropriate code
    total_violations = sum(len(v) for v in violations_by_layer.values())
    sys.exit(2 if total_violations > 0 else 0)

if __name__ == "__main__":
    main()
