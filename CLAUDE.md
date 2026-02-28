# ABAP Tree-Sitter LSP - Claude Code Instructions

## Workflow Rules

- The workflow is Brainstorm -> Discuss Trade-offs of different designs -> Plan -> Write unit tests -> Implement -> Fix Tests -> Commit -> Refactor.
- When brainstorming / planning, consider the follow parameters:
  - Whether there are any open source projects which perform similar functionality, so that you don't have to write new code for the task
  - The complexity of the implementation matters. Think of a good balance between absolute correctness and "good enough". If in doubt, prompt me for guidance.
- Once a design is finalised, document salient architectural decisions as a timestamped Architectural Decision Record in `docs/architectural-design-decisions.md`.
- After completing implementation tasks, always run the full test suite before committing. Do not commit code that hasn't passed all tests.
- When implementing plans that span many files, complete each logical unit fully before moving to the next. Do not start a new task until the current one is committed. If the session may end, prefer a committed partial result over an uncommitted complete attempt.

## Project Context

- Primary languages: Python (LSP server), TypeScript/JavaScript (VS Code extension), C (tree-sitter grammar).
- When editing Python, always run `black` formatting before committing. When test counts are mentioned (e.g., 'all 229 tests passing'), verify that count hasn't regressed.
- The tree-sitter grammar for ABAP is in the repo root (grammar.js, src/parser.c, src/scanner.c).
- The LSP server lives in `abap-lsp/` using pygls + tree-sitter Python bindings.
- The VS Code extension client lives in `editors/vscode/`.

## Common Mistakes to Avoid

- When working with LLM API calls or external APIs, start with small test inputs before processing large datasets.
- ABAP is case-insensitive — all identifier comparisons must use case-insensitive matching.
- The external scanner (src/scanner.c) must be compiled alongside src/parser.c into the shared library.

## Interaction Style

- When a user interrupts or cancels a task, do not ask clarifying questions — immediately proceed with the redirected instruction. Treat interruptions as implicit 'stop what you're doing and do this instead'.

## Build

- When asked to commit and push, always push to 'main' branch, unless otherwise instructed.
- Before committing anything, run `poetry run black` on every Python file touched in the change. The CI pipeline enforces Black formatting and will fail if this is skipped.
- Before committing anything, run all tests, fixing them if necessary. If test assertions are being removed, ask me to review them.

## Testing Patterns

- Use `pytest` with fixtures for test setup
- Do not patch with `unittest.mock.patch`. Use proper dependency injection, and then inject mock objects.
- Use `tmp_path` fixture for filesystem tests
- When fixing tests, do not blindly change test assertions to make the test pass. Only modify assertions once you are sure that the actual code output is actually valid according to the context.
- Always start from writing unit tests for the smallest feasible units of code. True unit tests (which do not exercise true I/O) should be in a `unit` directory under the test directory. Tests which exercise I/O should be in the `integration` directory under the test directory.
- Make sure you are not creating any special implementation behaviour just to get the tests to pass.

## Programming Patterns

- Use proper dependency injection for interfaces to external systems. Do not hardcode importing the concrete modules in these cases.
- Minimise and/or avoid mutation.
- Write your code in the Functional Programming style, but balance it with readability. Avoid for loops where list comprehensions, map, filter, reduce, etc. can be used.
- Minimise magic strings and numbers by refactoring them into constants.
- When writing `if` conditions, prefer early return. Use `if` conditions for checking and acting on exceptional cases.
- Parameters in functions, if they must have default values, must have those values as empty structures corresponding to the non-empty types (empty dictionaries, lists, etc.). Do not use None.
- If a function has a non-None return type, never return None.
- Prefer small, composable functions. Do not write massive functions.
- Add copious helpful logs to track progress of tasks, especially long-running ones.
- When importing, use fully qualified module names. Do not use relative imports.
- For variables which can only take a fixed set of values, use enums instead of strings.

## Dependencies

- Python 3.10+
- Poetry for dependency management
- tree-sitter >= 0.23
- pygls >= 1.3
- Node.js (for VS Code extension development)
