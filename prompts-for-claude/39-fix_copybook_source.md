# Fix for "COPYBOOK" Placeholder Bug in cobol-ast-parser

## Bug Description

When the COBOL parser resolves COPY statements and builds line mappings, it incorrectly assigns the literal string `"COPYBOOK"` as the `source_file` for some variables that originate from copybooks, instead of using the actual copybook name.

## Root Cause Analysis

### Location of Bug

The bug is in `preprocessor/copy_resolver.py`, specifically in the `_build_line_mapping()` method at **lines 284 and 292**:

```python
# Line 279-286:
if not found_match:
    # Line is from copybook
    self._line_mapping[resolved_line_num] = LineMapping(
        expanded_line=resolved_line_num,
        original_line=copybook_start_line if copybook_start_line else 1,
        source_file=current_copybook or "COPYBOOK",  # <-- BUG
        is_copybook=True,
    )

# Line 287-294:
else:
    # All remaining lines are from copybook
    self._line_mapping[resolved_line_num] = LineMapping(
        expanded_line=resolved_line_num,
        original_line=copybook_start_line if copybook_start_line else 1,
        source_file=current_copybook or "COPYBOOK",  # <-- BUG
        is_copybook=True,
    )
```

### Why `current_copybook` Becomes `None`

The algorithm tracks the current copybook context using `current_copybook` variable. However, this variable can incorrectly become `None` due to:

#### 1. Premature Reset (Lines 219-222)

When a resolved line matches an original source line, `current_copybook` is reset:

```python
if resolved_line.strip():
    current_copybook = None
```

This reset is too aggressive. If a copybook line happens to match an original source line (common COBOL patterns, FILLER statements, empty lines), the algorithm incorrectly thinks we've returned to the main source.

#### 2. False Positive Line Matching (Lines 227-277)

The lookahead/lookback matching logic tries to sync resolved lines with original source lines:

```python
# Look forward
for lookahead in range(1, lookahead_limit):
    check_line = original_lines[check_idx]
    if self._lines_match(resolved_line, check_line):
        # Found match - incorrectly assumes we're in original source
        current_copybook = None  # <-- Loses copybook context
        found_match = True
        break
```

When a copybook line coincidentally matches an original source line, the algorithm:
1. Sets `found_match = True`
2. Resets `current_copybook = None`
3. Subsequent lines from the same copybook lose their context

#### 3. Fallback to Literal "COPYBOOK"

After the erroneous reset, when subsequent copybook lines don't match any original source line, they fall into the fallback case:

```python
source_file=current_copybook or "COPYBOOK"
```

Since `current_copybook` is `None`, the literal string `"COPYBOOK"` is used.

## Detailed Fix

### Changes Required

The fix introduces a `last_known_copybook` variable to track the most recent valid copybook name, ensuring we never lose context completely.

### Modified Code

Replace the `_build_line_mapping` method (lines 138-296) with the following:

```python
def _build_line_mapping(self, original_source: str, resolved_source: str) -> None:
    """Build mapping from resolved line numbers to original line numbers.

    For lines that come from copybooks, maps to the COPY statement line in original.
    For original source lines, maps directly.

    Args:
        original_source: Original source before COPY resolution
        resolved_source: Source after COPY resolution
    """
    original_lines = original_source.splitlines()
    resolved_lines = resolved_source.splitlines()

    # Find all COPY statement locations in original source
    # Use a simpler pattern here that doesn't require the trailing period,
    # since multi-line COPY statements (with REPLACING) have the period
    # on a later line.
    copy_location_pattern = re.compile(r'\bCOPY\s+([A-Za-z0-9_-]+)', re.IGNORECASE)
    copy_locations: Dict[int, str] = {}  # original_line -> copybook_name
    for i, line in enumerate(original_lines, 1):
        match = copy_location_pattern.search(line)
        if match:
            copy_locations[i] = match.group(1).upper()

    # Track position in both sources
    orig_idx = 0
    resolved_idx = 0
    current_copybook: Optional[str] = None
    last_known_copybook: Optional[str] = None  # NEW: Track last valid copybook
    copybook_start_line: int = 0

    while resolved_idx < len(resolved_lines):
        resolved_line_num = resolved_idx + 1
        resolved_line = resolved_lines[resolved_idx]

        # Check if this is a COPY resolution comment
        # The marker "* COPY X resolved" appears BEFORE the copybook content,
        # so we use it to set the context for subsequent lines.
        if resolved_line.strip().startswith("* COPY") and "resolved" in resolved_line:
            # Extract copybook name from comment
            parts = resolved_line.strip().split()
            if len(parts) >= 3:
                new_copybook = parts[2]
                # Find which original COPY statement this corresponds to
                new_copybook_start_line = copybook_start_line
                for orig_line_num, copybook_name in copy_locations.items():
                    if copybook_name == new_copybook and orig_line_num > copybook_start_line:
                        new_copybook_start_line = orig_line_num
                        break

                current_copybook = new_copybook
                last_known_copybook = new_copybook  # NEW: Update last known
                copybook_start_line = new_copybook_start_line

            self._line_mapping[resolved_line_num] = LineMapping(
                expanded_line=resolved_line_num,
                original_line=copybook_start_line,
                source_file=current_copybook or self._main_source_name,
                is_copybook=True,
            )
            resolved_idx += 1
            continue

        # Check if we're back to original source
        if orig_idx < len(original_lines):
            orig_line = original_lines[orig_idx]
            # Skip COPY statements in original - they're replaced
            # Don't reset current_copybook here - we need to keep tracking
            # which copybook we're in for the expanded lines
            if self.COPY_PATTERN.search(orig_line):
                orig_idx += 1
                continue

            # If lines match (approximately), we're in original source
            if self._lines_match(resolved_line, orig_line):
                self._line_mapping[resolved_line_num] = LineMapping(
                    expanded_line=resolved_line_num,
                    original_line=orig_idx + 1,
                    source_file=self._main_source_name,
                    is_copybook=False,
                )
                orig_idx += 1
                # Only reset current_copybook when matching non-empty lines.
                # Empty lines can match between copybook content and original
                # source, causing incorrect state transitions.
                if resolved_line.strip():
                    current_copybook = None
                    # NOTE: Do NOT reset last_known_copybook here
            else:
                # Lines don't match - could be copybook content, or we're out of sync
                # Look both forward and backward in the original source to see if
                # the resolved line appears within a reasonable window
                found_match = False
                if resolved_line.strip():  # Only search for non-empty lines
                    # First look forward
                    lookahead_limit = min(50, len(original_lines) - orig_idx)
                    for lookahead in range(1, lookahead_limit):
                        check_idx = orig_idx + lookahead
                        if check_idx >= len(original_lines):
                            break
                        check_line = original_lines[check_idx]
                        # Skip COPY statements when looking ahead
                        if self.COPY_PATTERN.search(check_line):
                            continue
                        if self._lines_match(resolved_line, check_line):
                            # Found the line ahead - sync up and use this position
                            orig_idx = check_idx
                            self._line_mapping[resolved_line_num] = LineMapping(
                                expanded_line=resolved_line_num,
                                original_line=orig_idx + 1,
                                source_file=self._main_source_name,
                                is_copybook=False,
                            )
                            orig_idx += 1
                            current_copybook = None
                            # NOTE: Do NOT reset last_known_copybook here
                            found_match = True
                            break

                    # If not found forward, also look backward (orig_idx may have
                    # jumped ahead due to sync issues with copybook content)
                    if not found_match:
                        lookback_limit = min(100, orig_idx)
                        for lookback in range(1, lookback_limit):
                            check_idx = orig_idx - lookback
                            if check_idx < 0:
                                break
                            check_line = original_lines[check_idx]
                            # Skip COPY statements
                            if self.COPY_PATTERN.search(check_line):
                                continue
                            if self._lines_match(resolved_line, check_line):
                                # Found the line behind - use this position
                                # but don't change orig_idx (we might need to
                                # continue processing from current position)
                                self._line_mapping[resolved_line_num] = LineMapping(
                                    expanded_line=resolved_line_num,
                                    original_line=check_idx + 1,
                                    source_file=self._main_source_name,
                                    is_copybook=False,
                                )
                                current_copybook = None
                                # NOTE: Do NOT reset last_known_copybook here
                                found_match = True
                                break

                if not found_match:
                    # Line is from copybook
                    # FIX: Use last_known_copybook as fallback instead of "COPYBOOK"
                    effective_copybook = current_copybook or last_known_copybook
                    self._line_mapping[resolved_line_num] = LineMapping(
                        expanded_line=resolved_line_num,
                        original_line=copybook_start_line if copybook_start_line else 1,
                        source_file=effective_copybook or "<unknown_copybook>",
                        is_copybook=True,
                    )
        else:
            # All remaining lines are from copybook
            # FIX: Use last_known_copybook as fallback instead of "COPYBOOK"
            effective_copybook = current_copybook or last_known_copybook
            self._line_mapping[resolved_line_num] = LineMapping(
                expanded_line=resolved_line_num,
                original_line=copybook_start_line if copybook_start_line else 1,
                source_file=effective_copybook or "<unknown_copybook>",
                is_copybook=True,
            )

        resolved_idx += 1
```

### Summary of Changes

| Line | Change | Reason |
|------|--------|--------|
| 165 | Add `last_known_copybook: Optional[str] = None` | Track the most recent valid copybook name |
| 188 | Add `last_known_copybook = new_copybook` | Update tracking when entering a copybook |
| 222, 249, 276 | Remove reset of `last_known_copybook` | Preserve context even when temporarily matching original source |
| 284 | Change `current_copybook or "COPYBOOK"` to `effective_copybook or "<unknown_copybook>"` | Use tracked copybook name instead of hardcoded literal |
| 292 | Same as above | Consistent fix for remaining lines case |

### Alternative Fix (Minimal Change)

If a minimal change is preferred, only modify lines 284 and 292:

```python
# Before (line 284):
source_file=current_copybook or "COPYBOOK",

# After:
source_file=current_copybook or last_known_copybook or "<unknown_copybook>",
```

And add the `last_known_copybook` variable tracking (lines 165 and 188).

### Test Case 2: Nested COPY

```cobol
       COPY OUTER-COPY.
```

Where OUTER-COPY contains:
```cobol
       01  OUTER-VAR.
           COPY INNER-COPY.
```

**Expected:** Variables from INNER-COPY should have `copybook_source="INNER-COPY"`, not "OUTER-COPY" or "COPYBOOK"

### Test Case 3: Multiple COPY Statements

```cobol
       COPY COPYBOOK-A.
       01  LOCAL-VAR PIC X(10).
       COPY COPYBOOK-B.
```

**Expected:** Variables from COPYBOOK-A have `copybook_source="COPYBOOK-A"`, LOCAL-VAR has no copybook_source, variables from COPYBOOK-B have `copybook_source="COPYBOOK-B"`






### Examples from a complex COBOL program

In the variable tree UI:
- `WS-UPDATE-AREA-LENGTHS` shows "(COPYBOOK)"
- `WS-AREA-SIZE PIC S9(04)` shows "(COPYBOOK)"

These variables come from `COPY DATAGRP-A` statement, so they should show "(DATAGRP-A)".

## Testing

### Test Case 1: Basic COPY Resolution

```cobol
       WORKING-STORAGE SECTION.
       01  MAIN-LENGTH-TABLE.
         02  FILLER OCCURS 06 TIMES.
             COPY DATAGRP-A.
```

**Expected:** All variables from DATAGRP-A should have `copybook_source="DATAGRP-A"`