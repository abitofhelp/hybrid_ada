#!/usr/bin/env python3
# =============================================================================
# Architecture Guard - Prevent Layer Violations
# =============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# See LICENSE file in the project root.
#
# Purpose:
#   Enforce hexagonal architecture rules by preventing invalid layer dependencies.
#   Specifically: Presentation layer must not import Domain layer directly.
#
# Usage:
#   python3 scripts/arch_guard.py
#
# Returns:
#   0 - No violations found
#   2 - Architecture violations detected
# =============================================================================

import os
import re
import sys

# Get project root (parent of scripts directory)
root = os.path.dirname(os.path.dirname(__file__))
bad = []

# Check all Ada source files in Presentation layer
presentation_dir = os.path.join(root, "presentation")
for dp, _, fns in os.walk(presentation_dir):
    for fn in fns:
        if fn.endswith((".ads", ".adb")):
            p = os.path.join(dp, fn)
            try:
                with open(p, "r", encoding="utf-8", errors="ignore") as f:
                    content = f.read()
            except Exception:
                continue

            # Check for direct Domain imports (violation)
            if re.search(r"\bwith\s+Hybrid\.Domain\b", content):
                bad.append(p)

if bad:
    print("❌ Presentation imports Domain (architecture violation):", *bad, sep="\n   ")
    sys.exit(2)

print("✅ OK: No Presentation→Domain imports found.")
sys.exit(0)
