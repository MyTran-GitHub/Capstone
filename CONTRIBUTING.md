CONTRIBUTING.md

How to contribute

- Fork the repository and create a topic branch for your change.
- Keep commits small and focused; prefer descriptive commit messages.
- Add tests for bug fixes or new features when practical.

Coding standards
- R: use `styler` for formatting and `lintr` for static checks. Example:

  R -e "styler::style_dir()"
  R -e "lintr::lint_dir()"

- Python: use `black` and `ruff`/`flake8`:

  black .
  ruff check .

Pull request process
- Open a pull request against `main`. Use small, focused PRs. Include a short description and list of changed files.
- All tests and linters should pass before merging.

Reproducibility
- Update `REPRODUCE.md` when adding new steps or external dependencies.
- Record environment snapshots with `conda env export` and commit a lockfile when appropriate.
