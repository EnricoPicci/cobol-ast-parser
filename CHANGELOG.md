# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.8.0] - 2026-02-27

### Added
- COBOL SECTIONs now appear as first-class entries in `analyze_procedure_division()` inventory
- Section entries include child paragraph list and line ranges
- Standalone section statements extracted into perform/goto/call graphs and field references

### Changed
- **BREAKING:** `ParagraphInfo` dataclass renamed to `ProcedureEntry`
- **BREAKING:** `ProcedureDivisionResult.paragraphs` renamed to `.inventory`
- **BREAKING:** `ProcedureDivisionResult.for_paragraphs()` renamed to `.for_entries()`
- **BREAKING:** `to_dict()` output key changed from `"paragraphs"` to `"inventory"`
- `to_text()` header changed from `PARAGRAPHS:` to `INVENTORY:`


## [0.7.0] - 2026-02-26

### Changed
- (Add changes here)


## [0.6.0] - 2026-02-19

### Changed
- (Add changes here)


## [0.5.0] - 2026-02-19

### Changed
- (Add changes here)


## [0.4.0] - 2026-02-18

### Changed
- (Add changes here)


## [0.3.0] - 2026-02-18

### Changed
- (Add changes here)


## [0.2.5] - 2026-02-05

### Fixed
- Copybook resolution no longer fails completely when individual copybooks are not found. Instead, missing copybooks generate warnings and the COPY statement is left as-is in the source.

### Added
- `warnings` field on `AnalysisResult`, `DataDivisionTree`, and `CombinedResult` containing warning messages (e.g., copybooks not found)
- `warnings` property on `CopyResolver` to access warnings collected during resolution
- "Handling Warnings" section in API documentation


## [0.2.4] - 2026-02-03

### Changed
- (Add changes here)


## [0.2.3] - 2026-02-02

### Changed
- (Add changes here)


## [0.2.2] - 2026-02-02

### Changed
- (Add changes here)


## [0.2.1] - 2026-02-02

### Changed
- (Add changes here)


## [0.2.0] - 2026-02-02

### Added
- Initial release

