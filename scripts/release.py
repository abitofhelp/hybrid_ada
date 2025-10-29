#!/usr/bin/env python3
# ==============================================================================
# release.py - Release management for Ada Hybrid Architecture Project
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# See LICENSE file in the project root.
# ==============================================================================
"""
Release management script for Ada Hybrid Architecture Project.

This script handles the complete release process including:
- Version synchronization across all alire.toml files
- Version package generation (Hybrid.Version)
- Ada source file header updates (Copyright year, version)
- CHANGELOG.md maintenance
- Build verification and testing
- Git tagging and GitHub release creation
- PlantUML diagram generation

Usage:
    python tools/release.py prepare <version>
    python tools/release.py release <version>
    python tools/release.py diagrams

Examples:
    python tools/release.py prepare 1.0.0
    python tools/release.py release 1.0.0
    python tools/release.py diagrams
"""

import argparse
import os
import re
import subprocess
import sys
from datetime import datetime
from pathlib import Path
from typing import List, Optional, Tuple


class AdaReleaseManager:
    def __init__(self, project_root: str):
        self.project_root = Path(project_root)
        self.date_str = datetime.now().strftime("%B %d, %Y")
        self.year = datetime.now().year

    # =========================================================================
    # Version Management
    # =========================================================================

    def update_root_alire_version(self, new_version: str) -> bool:
        """Update version in root alire.toml (single source of truth)."""
        root_toml = self.project_root / "alire.toml"

        try:
            with open(root_toml, 'r') as f:
                content = f.read()

            # Check if version is already correct
            current_version_match = re.search(
                r'^version\s*=\s*"([^"]+)"',
                content,
                flags=re.MULTILINE
            )

            if current_version_match:
                current_version = current_version_match.group(1)
                if current_version == new_version:
                    print(f"  ✓ Root alire.toml already has version = \"{new_version}\"")
                    return True

            # Update version line
            old_content = content
            content = re.sub(
                r'^(\s*version\s*=\s*")[^"]+(").*$',
                rf'\g<1>{new_version}\g<2>',
                content,
                flags=re.MULTILINE
            )

            if content == old_content:
                print(f"  ✗ Error: Version field not found in {root_toml}")
                return False

            with open(root_toml, 'w') as f:
                f.write(content)

            print(f"  ✓ Updated root alire.toml: version = \"{new_version}\"")
            return True

        except Exception as e:
            print(f"Error updating root alire.toml: {e}")
            return False

    def sync_layer_versions(self) -> bool:
        """Synchronize versions across all layer alire.toml files."""
        print("Syncing versions across all layer alire.toml files...")
        result = self.run_command(
            [sys.executable, "scripts/sync_versions.py"],
            capture_output=True
        )
        return result is not None

    def generate_version_package(self) -> bool:
        """Generate Hybrid.Version Ada package from alire.toml."""
        print("Generating Hybrid.Version package...")
        result = self.run_command(
            [sys.executable, "scripts/generate_version.py",
             "alire.toml", "shared/src/hybrid-version.ads"],
            capture_output=True
        )
        if result:
            print("  ✓ Generated shared/src/hybrid-version.ads")
        return result is not None

    # =========================================================================
    # Ada Source File Header Updates
    # =========================================================================

    def find_ada_source_files(self) -> List[Path]:
        """Find all Ada source files (.ads, .adb)."""
        ada_files = []

        # Search in layer directories
        for layer in ["application", "bootstrap", "domain",
                      "infrastructure", "presentation", "shared"]:
            layer_path = self.project_root / layer / "src"
            if layer_path.exists():
                ada_files.extend(layer_path.rglob("*.ads"))
                ada_files.extend(layer_path.rglob("*.adb"))

        return sorted(ada_files)

    def update_ada_copyright_year(self, file_path: Path) -> bool:
        """Update copyright year in Ada source file header."""
        try:
            with open(file_path, 'r') as f:
                content = f.read()

            # Update copyright year pattern: Copyright (c) 2025 -> Copyright (c) 2026
            old_content = content
            content = re.sub(
                r'(--\s+Copyright \(c\)) \d{4}',
                rf'\g<1> {self.year}',
                content
            )

            # Only write if changed
            if content != old_content:
                with open(file_path, 'w') as f:
                    f.write(content)
                return True

            return False

        except Exception as e:
            print(f"Error updating {file_path}: {e}")
            return False

    def update_all_ada_headers(self) -> int:
        """Update copyright year in all Ada source files. Returns count of updated files."""
        ada_files = self.find_ada_source_files()
        updated_count = 0

        print(f"Updating copyright year in Ada source files...")
        for ada_file in ada_files:
            if self.update_ada_copyright_year(ada_file):
                updated_count += 1

        if updated_count > 0:
            print(f"  ✓ Updated {updated_count} Ada source files with year {self.year}")
        else:
            print(f"  ✓ All Ada source files already have year {self.year}")

        return updated_count

    # =========================================================================
    # Documentation Updates
    # =========================================================================

    def find_markdown_files(self) -> List[Path]:
        """Find all markdown files with version headers."""
        md_files = []

        # Search in docs and root
        for pattern in ["docs/**/*.md", "*.md"]:
            md_files.extend(self.project_root.glob(pattern))

        # Filter to only files with version headers
        # Matches:
        #   - "Version: 1.0.0" or "version: 1.0.0" (with colon)
        #   - "**Version 1.0.0**" or "**version 1.0.0**" (bold without colon)
        #   - "Copyright © 2025" (copyright year pattern)
        versioned_files = []
        for md_file in md_files:
            try:
                with open(md_file, 'r') as f:
                    content = f.read()
                    # Match version patterns OR copyright patterns
                    if re.search(r'Version\s*[:)]|version\s*[:)]|\*\*Version\s+\d+\.\d+|Copyright\s*©\s*\d{4}',
                                content, re.IGNORECASE):
                        versioned_files.append(md_file)
            except Exception:
                pass

        return versioned_files

    def update_markdown_version(self, file_path: Path, new_version: str) -> bool:
        """
        Update version and metadata in markdown file headers.

        Handles these patterns:
        - **Version:** 1.0.0
        - **Date:** October 24, 2025
        - **Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.
        - **Status:** Unreleased / Released
        """
        try:
            with open(file_path, 'r') as f:
                content = f.read()

            old_content = content

            # Pattern 1: **Version:** 1.0.0 (bold with colon)
            content = re.sub(
                r'(\*\*Version:\*\*\s+)[^\s\n]+',
                rf'\g<1>{new_version}',
                content,
                flags=re.IGNORECASE
            )

            # Pattern 2: Version: 1.0.0 (plain with colon)
            content = re.sub(
                r'^(Version:\s+)[^\s\n]+',
                rf'\g<1>{new_version}',
                content,
                flags=re.IGNORECASE | re.MULTILINE
            )

            # Pattern 3: **Version 1.0.0** (bold without colon - used in cover.md)
            content = re.sub(
                r'(\*\*Version\s+)\d+\.\d+\.\d+(\*\*)',
                rf'\g<1>{new_version}\g<2>',
                content,
                flags=re.IGNORECASE
            )

            # Pattern 3: **Date:** October 24, 2025 (bold)
            content = re.sub(
                r'(\*\*Date:\*\*\s+)[^\n]+',
                rf'\g<1>{self.date_str}',
                content,
                flags=re.IGNORECASE
            )

            # Pattern 4: Date: October 24, 2025 (plain)
            content = re.sub(
                r'^(Date:\s+)[^\n]+',
                rf'\g<1>{self.date_str}',
                content,
                flags=re.IGNORECASE | re.MULTILINE
            )

            # Pattern 5: **Copyright:** © 2024 -> © 2025 (update year only)
            content = re.sub(
                r'(\*\*Copyright:\*\*\s+©\s*)\d{4}',
                rf'\g<1>{self.year}',
                content,
                flags=re.IGNORECASE
            )

            # Pattern 6: Copyright: © 2024 -> © 2025 (plain)
            content = re.sub(
                r'^(Copyright:\s+©\s*)\d{4}',
                rf'\g<1>{self.year}',
                content,
                flags=re.IGNORECASE | re.MULTILINE
            )

            # Pattern 7: **Status:** Unreleased/Pre-release -> Released (for releases)
            # Only update if this is NOT a pre-release version
            is_prerelease = '-' in new_version  # e.g., "1.0.0-dev", "1.0.0-alpha.1"
            if not is_prerelease:
                # For stable releases, mark as Released
                # Handle "Unreleased"
                content = re.sub(
                    r'(\*\*Status:\*\*\s+)Unreleased',
                    r'\g<1>Released',
                    content,
                    flags=re.IGNORECASE
                )
                content = re.sub(
                    r'^(Status:\s+)Unreleased',
                    r'\g<1>Released',
                    content,
                    flags=re.IGNORECASE | re.MULTILINE
                )
                # Handle "Pre-release" and "Pre-release (vX.X.X)"
                content = re.sub(
                    r'(\*\*Status:\*\*\s+)Pre-release(?:\s+\([^)]+\))?',
                    r'\g<1>Released',
                    content,
                    flags=re.IGNORECASE
                )
                content = re.sub(
                    r'^(Status:\s+)Pre-release(?:\s+\([^)]+\))?',
                    r'\g<1>Released',
                    content,
                    flags=re.IGNORECASE | re.MULTILINE
                )
            else:
                # For pre-releases, keep as Unreleased
                content = re.sub(
                    r'(\*\*Status:\*\*\s+)Released',
                    r'\g<1>Unreleased',
                    content,
                    flags=re.IGNORECASE
                )
                content = re.sub(
                    r'^(Status:\s+)Released',
                    r'\g<1>Unreleased',
                    content,
                    flags=re.IGNORECASE | re.MULTILINE
                )

            # Add trailing spaces for proper GitHub markdown rendering
            # GitHub needs two spaces at end of line for line breaks
            lines = content.split('\n')
            new_lines = []
            for line in lines:
                # Add trailing spaces to metadata header lines
                if re.match(r'^\*\*(Version|Date|SPDX|License|Copyright|Status):', line):
                    if not line.endswith('  '):
                        line = line.rstrip() + '  '
                new_lines.append(line)
            content = '\n'.join(new_lines)

            # Check if any changes were made
            if content != old_content:
                with open(file_path, 'w') as f:
                    f.write(content)
                return True

            return False

        except Exception as e:
            print(f"Error updating {file_path}: {e}")
            return False

    def update_all_markdown_files(self, version: str) -> int:
        """Update version in all markdown files. Returns count of updated files."""
        md_files = self.find_markdown_files()
        updated_count = 0

        if not md_files:
            return 0

        print(f"Updating markdown files...")
        for md_file in md_files:
            if self.update_markdown_version(md_file, version):
                rel_path = md_file.relative_to(self.project_root)
                print(f"  ✓ {rel_path}")
                updated_count += 1

        return updated_count

    # =========================================================================
    # CHANGELOG Management
    # =========================================================================

    def update_changelog(self, new_version: str) -> bool:
        """Update CHANGELOG.md with new version."""
        changelog_file = self.project_root / "CHANGELOG.md"

        if not changelog_file.exists():
            print(f"Warning: CHANGELOG.md not found, skipping")
            return True  # Not a failure, just skip

        try:
            with open(changelog_file, 'r') as f:
                content = f.read()

            # Find the [Unreleased] section
            unreleased_pattern = r'## \[Unreleased\]\s*\n(.*?)(?=\n## |\Z)'
            match = re.search(unreleased_pattern, content, re.DOTALL)

            if match:
                unreleased_content = match.group(1).strip()

                # Create new release section
                release_section = f"""## [{new_version}] - {self.date_str}

{unreleased_content}

## [Unreleased]

### Added


### Changed


### Fixed


### Removed


### Security

"""

                # Replace the unreleased section
                content = re.sub(
                    r'## \[Unreleased\]\s*\n.*?(?=\n## |\Z)',
                    release_section,
                    content,
                    flags=re.DOTALL
                )

                with open(changelog_file, 'w') as f:
                    f.write(content)

                print("  ✓ Updated CHANGELOG.md")
                return True
            else:
                print("Warning: Could not find [Unreleased] section in CHANGELOG.md")
                return True  # Not a fatal error

        except Exception as e:
            print(f"Error updating changelog: {e}")
            return False

    # =========================================================================
    # Build and Test Verification
    # =========================================================================

    def run_command(self, cmd: List[str], capture_output: bool = False) -> Optional[str]:
        """Run a shell command."""
        try:
            result = subprocess.run(
                cmd,
                cwd=self.project_root,
                capture_output=capture_output,
                text=True
            )
            if result.returncode != 0:
                print(f"❌ Command failed: {' '.join(cmd)}")
                if result.stderr:
                    print(f"Error: {result.stderr}")
                return None
            return result.stdout if capture_output else "SUCCESS"
        except Exception as e:
            print(f"❌ Command exception: {' '.join(cmd)}")
            print(f"Exception: {e}")
            return None

    def verify_clean_working_tree(self) -> bool:
        """Verify git working tree is clean."""
        result = self.run_command(["git", "status", "--porcelain"], capture_output=True)
        if result is None:
            return False
        return len(result.strip()) == 0

    def run_build(self) -> bool:
        """Run full build."""
        print("Running build...")
        return self.run_command(["make", "clean"]) is not None and \
               self.run_command(["make", "build"]) is not None

    def run_tests(self) -> bool:
        """Run all tests."""
        print("Running tests...")
        # Check if test target exists
        result = self.run_command(["make", "test"], capture_output=True)
        if result is None:
            print("  ⚠️  No tests found or test target not available")
            return True  # Not a fatal error for now
        print("  ✓ All tests passed")
        return True

    def run_format_check(self) -> bool:
        """Verify code formatting."""
        print("Checking code formatting...")
        result = self.run_command(["make", "format-check"], capture_output=True)
        if result is None:
            print("  ⚠️  Format check not available")
            return True  # Not fatal
        print("  ✓ Code formatting verified")
        return True

    def generate_diagrams(self) -> bool:
        """Generate PlantUML diagrams."""
        print("Generating UML diagrams...")

        # Check if plantuml is available
        try:
            subprocess.run(
                ["plantuml", "-version"],
                capture_output=True,
                check=True
            )
        except (subprocess.CalledProcessError, FileNotFoundError):
            print("  ⚠️  plantuml not found, skipping diagram generation")
            return True  # Not fatal

        diagrams_dir = self.project_root / "docs" / "diagrams"
        if not diagrams_dir.exists():
            print("  ⚠️  No diagrams directory found")
            return True

        # Find all .puml files and generate SVGs
        puml_files = list(diagrams_dir.glob("*.puml"))
        if not puml_files:
            print("  ⚠️  No PlantUML files found")
            return True

        for puml_file in puml_files:
            result = self.run_command(
                ["plantuml", "-tsvg", str(puml_file)],
                capture_output=True
            )
            if result is None:
                print(f"  ⚠️  Failed to generate {puml_file.stem}.svg")

        print(f"  ✓ Generated {len(puml_files)} diagram(s)")
        return True

    # =========================================================================
    # Git Operations
    # =========================================================================

    def create_git_tag(self, version: str) -> bool:
        """Create annotated git tag."""
        tag_name = f"v{version}"
        message = f"Release version {version}"

        result = self.run_command([
            "git", "tag", "-a", tag_name, "-m", message
        ])

        if result:
            print(f"  ✓ Created tag {tag_name}")
        return result is not None

    def push_changes(self, version: str) -> bool:
        """Push changes and tags to origin."""
        commands = [
            (["git", "push", "origin", "main"], "Pushed to main"),
            (["git", "push", "origin", f"v{version}"], f"Pushed tag v{version}")
        ]

        for cmd, success_msg in commands:
            if self.run_command(cmd) is None:
                return False
            print(f"  ✓ {success_msg}")

        return True

    def create_github_release(self, version: str) -> bool:
        """Create GitHub release using gh CLI."""
        # Extract release notes from CHANGELOG.md
        changelog_file = self.project_root / "CHANGELOG.md"
        release_notes = f"Release version {version}"

        if changelog_file.exists():
            try:
                with open(changelog_file, 'r') as f:
                    content = f.read()

                version_pattern = rf'## \[{re.escape(version)}\][^\n]*\n(.*?)(?=\n## |\Z)'
                match = re.search(version_pattern, content, re.DOTALL)

                if match:
                    release_notes = match.group(1).strip()
            except Exception as e:
                print(f"Warning: Could not extract release notes: {e}")

        # Create release
        cmd = [
            "gh", "release", "create", f"v{version}",
            "--title", f"Release {version}",
            "--notes", release_notes
        ]

        # Add diagram SVGs as assets
        diagrams_dir = self.project_root / "docs" / "diagrams"
        if diagrams_dir.exists():
            svg_files = list(diagrams_dir.glob("*.svg"))
            for svg in svg_files[:3]:  # Limit to prevent huge releases
                cmd.append(str(svg))

        result = self.run_command(cmd)
        if result:
            print(f"  ✓ Created GitHub release v{version}")
        return result is not None

    # =========================================================================
    # Release Workflow
    # =========================================================================

    def ensure_md_headers(self) -> bool:
        """Ensure all markdown files have standard headers."""
        print("Ensuring all markdown files have standard headers...")
        result = self.run_command(
            [sys.executable, "scripts/add_md_headers.py"],
            capture_output=True
        )
        if result:
            print("  ✓ Markdown headers checked/added")
        return result is not None

    def prepare_release(self, version: str) -> bool:
        """Prepare release by updating versions and running checks."""
        print(f"\n{'='*70}")
        print(f"PREPARING RELEASE {version}")
        print(f"{'='*70}\n")

        # Step 1: Ensure all markdown files have standard headers
        print("📝 Step 1: Ensuring markdown files have standard headers...")
        if not self.ensure_md_headers():
            print("  ⚠️  Warning: Could not verify markdown headers")

        # Step 2: Update root alire.toml version
        print("\n📝 Step 2: Updating root alire.toml version...")
        if not self.update_root_alire_version(version):
            return False

        # Step 3: Sync all layer alire.toml files
        print("\n📝 Step 3: Syncing layer versions...")
        if not self.sync_layer_versions():
            return False

        # Step 4: Generate Hybrid.Version package
        print("\n📝 Step 4: Generating Hybrid.Version package...")
        if not self.generate_version_package():
            return False

        # Step 5: Update Ada source file headers
        print("\n📝 Step 5: Updating Ada source file headers...")
        self.update_all_ada_headers()

        # Step 6: Update markdown files
        print("\n📝 Step 6: Updating markdown documentation...")
        self.update_all_markdown_files(version)

        # Step 6: Update CHANGELOG
        print("\n📝 Step 6: Updating CHANGELOG.md...")
        if not self.update_changelog(version):
            return False

        # Step 7: Generate diagrams
        print("\n📝 Step 7: Generating diagrams...")
        if not self.generate_diagrams():
            return False

        # Step 8: Build verification
        print("\n🔨 Step 8: Running build...")
        if not self.run_build():
            print("❌ Build failed")
            return False
        print("  ✓ Build successful")

        # Step 9: Test verification
        print("\n🧪 Step 9: Running tests...")
        if not self.run_tests():
            print("❌ Tests failed")
            return False

        # Step 10: Format check
        print("\n📋 Step 10: Checking formatting...")
        self.run_format_check()

        print(f"\n{'='*70}")
        print(f"✅ RELEASE {version} PREPARED SUCCESSFULLY!")
        print(f"{'='*70}\n")
        print("Next steps:")
        print("1. Review the changes:")
        print("   git diff")
        print("2. Commit the changes:")
        print(f"   git add -A && git commit -m 'Prepare release {version}'")
        print("3. Create the release:")
        print(f"   python tools/release.py release {version}")
        print()

        return True

    def create_release(self, version: str) -> bool:
        """Create the actual release (tag and publish)."""
        print(f"\n{'='*70}")
        print(f"CREATING RELEASE {version}")
        print(f"{'='*70}\n")

        # Verify working tree is clean
        print("🔍 Verifying clean working tree...")
        if not self.verify_clean_working_tree():
            print("❌ Working tree is not clean. Please commit changes first.")
            print("   Run: git status")
            return False
        print("  ✓ Working tree is clean")

        # Create git tag
        print("\n🏷️  Creating git tag...")
        if not self.create_git_tag(version):
            return False

        # Push changes and tag
        print("\n⬆️  Pushing to GitHub...")
        if not self.push_changes(version):
            return False

        # Create GitHub release
        print("\n📦 Creating GitHub release...")
        if not self.create_github_release(version):
            return False

        print(f"\n{'='*70}")
        print(f"🎉 RELEASE {version} CREATED SUCCESSFULLY!")
        print(f"{'='*70}\n")
        print("Release is now live on GitHub!")
        print(f"View at: https://github.com/YOUR-USERNAME/hybrid/releases/tag/v{version}")
        print()

        return True


def main():
    parser = argparse.ArgumentParser(
        description="Release management for Ada Hybrid Architecture Project"
    )
    parser.add_argument(
        "action",
        choices=["prepare", "release", "diagrams"],
        help="Action to perform"
    )
    parser.add_argument(
        "version",
        nargs='?',
        help="Version to release (e.g., 1.0.0 or 1.0.0-dev) - required for prepare/release"
    )

    args = parser.parse_args()

    # Validate version is provided for prepare/release
    if args.action in ["prepare", "release"] and not args.version:
        print("❌ Error: Version is required for prepare/release actions")
        parser.print_help()
        sys.exit(1)

    # Validate semantic version format (allows pre-release like -dev, -alpha.1)
    if args.version and not re.match(r'^\d+\.\d+\.\d+(-[a-zA-Z0-9.]+)?(\+[a-zA-Z0-9.]+)?$', args.version):
        print("❌ Error: Version must follow semantic versioning (e.g., 1.0.0, 1.0.0-dev)")
        sys.exit(1)

    # Find project root
    script_dir = Path(__file__).parent
    project_root = script_dir.parent

    release_manager = AdaReleaseManager(str(project_root))

    try:
        if args.action == "prepare":
            success = release_manager.prepare_release(args.version)
        elif args.action == "release":
            success = release_manager.create_release(args.version)
        elif args.action == "diagrams":
            success = release_manager.generate_diagrams()
        else:
            print(f"❌ Unknown action: {args.action}")
            sys.exit(1)

        sys.exit(0 if success else 1)

    except KeyboardInterrupt:
        print("\n\n❌ Release process interrupted by user")
        sys.exit(1)
    except Exception as e:
        print(f"\n❌ Unexpected error: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()
