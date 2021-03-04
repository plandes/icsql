# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to [Semantic Versioning](http://semver.org/).


## [Unreleased]


## [0.0.6] - 2021-03-04
### Changed
- Fix connection prompt when missing supplemental connections.


## [0.0.5] - 2021-03-03
### Changed
- Handle single JSON `icsql-connections-supplemental` entry.


## [0.0.4] - 2021-02-16
### Added
- A new method to read connections from a file, which can be used store secure
  logins, with customized variable `icsql-connections-supplemental`.
### Changed
- Added a port to the list of entries `icsql-connections`.  If you use this
  configuration, you'll need to a `nil` in the fourth location in your
  customize database.  Or you'll need to remove it and re-add it.


## [0.0.3] - 2021-01-14
### Changed
- Drop the `-none` at the end of entry buffer names.
- Version bump for [ciSQL] REPL upgrade.
- More per spec doc.


## [0.0.2] - 2020-12-18
### Changed
- Upgraded to Emacs [zenbuild].
- Compat with recent *flycheck* and `package-lint`.
- Updated dependencies.
- Switched from Travis to GitHub workflows.
- Switched to conforming tag versions to other Zensols Emacs repos.

## Added
- User input sends SQL to buffer via `buffer-manager` framework.


## [0.0.1] - 2019-07-09
### Added
- Automatically download [ciSQL] jar file.
- Changelog and contributing.
- Add [zenbuild].
- Customization documentation.

### Changed
- Retrofit to new dependent libraries.
- Upgrade travis build.


<!-- links -->
[Unreleased]: https://github.com/plandes/icsql/current/v0.0.6...HEAD
[0.0.6]: https://github.com/plandes/icsql/current/v0.0.5...v0.0.6
[0.0.5]: https://github.com/plandes/icsql/current/v0.0.4...v0.0.5
[0.0.4]: https://github.com/plandes/icsql/current/v0.0.3...v0.0.4
[0.0.3]: https://github.com/plandes/icsql/current/v0.0.2...v0.0.3
[0.0.2]: https://github.com/plandes/icsql/current/v0.0.1...v0.0.2
[0.0.1]: https://github.com/plandes/icsql/current/vc9545c1e6e09961519cfbe2cfec0fb21ffa16c37...v0.0.1

[ciSQL]: https://github.com/plandes/cisql
[zenbuild]: https://github.com/plandes/zenbuild
