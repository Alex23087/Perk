# Contributing to Perk

Welcome to the Perk contribution guide!

---

## Finding what to contribute on
A good starting point is to check [issues tagged with "good first issue"](https://github.com/Alex23087/Perk/issues?q=is%3Aissue%20state%3Aopen%20label%3A%22good%20first%20issue%22). These should be simple tasks that we left behind specifically for people who want to contribute but don't know where to start.

If you have issues with those issues, or want to contribute in a different way, you can either open a new issue or reach out to us on [Discord](https://discord.com/invite/DgDDV6xPQe).

---

## Pull Request Process
We don't have a strict pull request process, but here are some guidelines to follow:
1. Fork the repository and create your branch from `main`.
2. If you've added code that should be tested, add tests.
3. :warning: **Ensure the test suite passes.** :warning:
    - Ideally, all tests should pass before you submit your pull request. See [Tests](#tests) for more information. If you can't get them to pass, you can still submit your PR, but please explain why the tests are failing in the PR description. We will absolutely help fix the tests, but it will likely delay the merging of your PR.
5. Issue that pull request!

:warning: **If you open a pull request to change a big part of the language, such as adding an entirely new construct, or changing how something is compiled, please get in touch with us first, either by opening an issue or reaching out on Discord. This will help us ensure that your changes align with the project's goals and avoid unnecessary work.** :warning:

---

## Tests
:warning: All references to the static tests only currently apply to the [`sperkaster-patches`](https://github.com/Alex23087/Perk/tree/sperkaster-patches) branch, and will be merged to `main` in the (near) future. :warning:

### Directory structure
Our current test setup consists in a number of Perk programs that should test small parts of the language. These programs are located in the `tests/` directory. The structure of this directory is as follows:
```
tests/
├── pass/           # Should compile and run successfully (dynamic)
├── fail/           # Should fail to compile (dynamic)
├── pass_static/    # Should compile and run successfully (static)
├── fail_static/    # Should fail to compile (static)
└── future/         # Tests for future features (not run)
```
In more detail:
- `pass/` and `fail/` test features of the full language, with support to features that require dynamic memory allocation (or in general runtime support)
- `pass_static/` and `fail_static/` test the subset of the language that can be compiled to C without any runtime support (i.e. no heap allocation, no garbage collection, no standard library)
- `future/` contains tests for features that are not yet implemented, and are more of a concept for what we want to implement in the future. These tests are not run.

### File structure
Each test is a `.perk` file, with a name starting with an increasing number (for example `01-hello_world.perk`). This number is used to order the tests when they are run, and should be unique in each directory.

Every **passing** test (i.e., a test in `pass/` or `pass_static/`) also has a corresponding `.expected` file, which contains the expected output of the test. The make target checks that the output of the test matches the expected output. **If there is no `.expected` file, or the output differs, the test is considered to fail.**

### Running the tests
To run the tests, simply run:
```bash
make test
```
This will run all the tests in `pass/`, `fail/`, `pass_static/` and `fail_static/`

You can also run a specific set of tests by running one of:
```bash
make test_pass
make test_fail
make test_pass_static
make test_fail_static
```

All test commands support passing a specific test file as an argument. For example:
```bash
make test_pass FILE=42
```
will only run the test that matches `test/pass/42*.perk`.

All the tests should pass. If you add a new feature, please add tests for it in the appropriate directory.