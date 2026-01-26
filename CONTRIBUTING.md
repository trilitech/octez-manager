# Contributing to Octez Manager

Thank you for your interest in contributing to Octez Manager! This document provides guidelines and instructions for contributing.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Development Setup](#development-setup)
- [Making Changes](#making-changes)
- [Code Style](#code-style)
- [Testing](#testing)
- [Submitting Changes](#submitting-changes)
- [License](#license)

## Code of Conduct

We are committed to providing a welcoming and inclusive environment. Please be respectful and constructive in all interactions.

## Getting Started

1. **Fork the repository** and clone your fork locally
2. **Set up the development environment** (see below)
3. **Create a branch** for your changes
4. **Make your changes** following our guidelines
5. **Submit a pull request**

## Development Setup

### Prerequisites

- OCaml 5.1 or later (5.3.x recommended)
- opam 2.x
- dune >= 3.15
- systemd (for service management features)

### Installation

```bash
# Clone your fork
git clone https://github.com/YOUR_USERNAME/octez-manager.git
cd octez-manager

# Install dependencies
make deps

# Build the project
make build

# Run tests
make test
```

### Useful Commands

| Command | Description |
|---------|-------------|
| `make deps` | Install all dependencies |
| `make build` | Build the project |
| `make test` | Run the test suite |
| `make fmt` | Format code with ocamlformat |
| `make clean` | Clean build artifacts |

## Making Changes

### Branch Naming

Use descriptive branch names with a prefix:

- `feat/` - New features
- `fix/` - Bug fixes
- `docs/` - Documentation changes
- `refactor/` - Code refactoring
- `test/` - Test additions or fixes
- `chore/` - Maintenance tasks

Example: `feat/add-baker-monitoring`

### Commit Messages

We follow [Conventional Commits](https://www.conventionalcommits.org/). Format:

```
type(scope): description

[optional body]

[optional footer]
```

**Types:**
- `feat` - New feature
- `fix` - Bug fix
- `docs` - Documentation
- `style` - Formatting (no code change)
- `refactor` - Code restructuring
- `test` - Adding tests
- `chore` - Maintenance

**Examples:**
```
feat(installer): add network selection wizard

fix(service): prevent duplicate service starts

docs(readme): add systemd requirements
```

## Code Style

### OCaml Formatting

We use `ocamlformat`. Run before committing:

```bash
make fmt
```

The configuration is in `.ocamlformat`.

### File Headers

All `.ml` and `.mli` files must include the license header:

```ocaml
(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)
```

### Interface Files

Public modules should have corresponding `.mli` interface files with:
- Type definitions
- Function signatures with documentation comments
- Module documentation at the top

## Testing

### Unit Tests

```bash
# Run unit tests
make test

# Run specific test
dune exec -- test/unit_tests.exe
```

Unit tests use [Alcotest](https://github.com/mirage/alcotest) and live in `test/unit_tests.ml`.

### Integration Tests

Integration tests run in Docker containers with real systemd, testing actual service management.

```bash
# Run integration tests locally
cd test/integration
./run.sh

# Or use docker-compose directly
docker compose up --build --abort-on-container-exit
```

Integration tests are located in `test/integration/cli-tester/tests/` organized by role:

| Directory | Description |
|-----------|-------------|
| `node/` | Node installation, lifecycle, restart, cascading dependencies |
| `baker/` | Baker installation, DAL config, remote endpoints, dependencies |
| `accuser/` | Accuser installation and lifecycle |
| `dal/` | DAL node installation and dependencies |

**Test naming**: `XX-description.sh` where XX is a number for ordering.

**Writing integration tests**:
- Source `/tests/lib.sh` for helper functions
- Use `cleanup_instance` for setup/teardown
- Use `wait_for_service_active`, `wait_for_node_ready` for async operations
- Tests run with real systemd as PID 1 in Docker

## Submitting Changes

### Pull Request Process

1. **Ensure all tests pass**: `make test`
2. **Format your code**: `make fmt`
3. **Update documentation** if needed
4. **Create a pull request** with a clear description

### CI Labels

The following labels can be added to PRs to control CI behavior:

| Label | Effect | Use Case |
|-------|--------|----------|
| `skip-coverage` | Runs fast integration tests (2-3 min/shard) instead of instrumented tests (8-9 min/shard). No coverage report will be generated. | Use when iterating quickly and coverage data isn't needed. Remove before final merge to ensure coverage is collected. |

**Note:** Coverage tests always run on the main branch to maintain the coverage baseline, regardless of labels used in PRs.

### Pull Request Template

Your PR description should include:

- **Summary**: What does this PR do?
- **Motivation**: Why is this change needed?
- **Testing**: How was this tested?
- **Breaking Changes**: Any breaking changes?

### Review Process

- All PRs require at least one review
- CI must pass (build, tests, formatting)
- Address review feedback promptly

## Getting Help

- **Issues**: Open an issue for bugs or feature requests
- **Discussions**: Use GitHub Discussions for questions

## License

By contributing, you agree that your contributions will be licensed under the MIT License. All contributions must include the appropriate license headers.

---

Thank you for contributing to Octez Manager!
