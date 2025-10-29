# Architecture Diagrams

**Version:** 1.0.0
**Date:** October 28, 2025
**SPDX-License-Identifier:** BSD-3-Clause
**License File:** See the LICENSE file in the project root.
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.
**Status:** Released


This directory contains PlantUML source files and generated SVG diagrams for the Hybrid Ada Architecture.

## Understanding the Generic Architecture (Start Here!)

If you're trying to understand how this project uses Ada generics instead of OOP interfaces, start with:

**[From OOP to Ada Generics Guide](../guides/oop-to-generics-guide.md)** - Complete guide to translating OOP concepts to generics

### Key Diagrams for Understanding Generics

1. **OOP vs Generic Port-Adapter Pattern** (`oop-vs-generic-comparison.svg`)
   - Side-by-side comparison of dynamic dispatch vs static dispatch
   - Shows how interfaces translate to generic formal parameters
   - **Start here if you know OOP and want to learn generics**

2. **Port-Adapter Pattern with Generics** (`port-adapter-generic-pattern.svg`)
   - Detailed breakdown of the port-adapter pattern using generics
   - Shows compile-time dependency injection
   - Explains structural matching vs explicit implementation

3. **Generic-Based Hybrid Architecture** (`generic-architecture-layers.svg`)
   - Complete layer architecture with generics
   - Shows how Domain, Application, Infrastructure, and Bootstrap layers connect
   - Illustrates instantiation points

4. **Generic Instantiation Flow** (`generic-instantiation-flow.svg`)
   - Sequence diagram showing compile-time composition
   - Demonstrates how generics are instantiated in Bootstrap
   - Shows static dispatch at runtime

## Original Architecture Diagrams

These diagrams show the overall system architecture:

- `architecture-layers.svg` - Hexagonal/Clean architecture layers
- `component-view.svg` - Component relationships
- `dependency-flow.svg` - Dependency inversion principle
- `domain-model.svg` - Domain entities and value objects
- `error-handling.svg` - Error handling strategy
- `package-structure.svg` - Package organization
- `sequence-greeting.svg` - Greeting use case sequence
- `use-case-flow.svg` - Use case execution flow

## Generating Diagrams

To regenerate all SVG files from PlantUML source:

```bash
cd docs/diagrams
plantuml -tsvg *.puml
```

Or for a specific diagram:

```bash
plantuml -tsvg oop-vs-generic-comparison.puml
```

## File Organization

- `*.puml` - PlantUML source files (version controlled)
- `*.svg` - Generated SVG diagrams (also version controlled for convenience)

## Tools Required

- PlantUML: `brew install plantuml` (macOS) or see https://plantuml.com/download
