This is the only step that writes to the DoltLite history in
`data/cso.doltlite`. Each new CSO release of a table becomes one commit and one
tag, named `<TABLE>/<release timestamp>`.

**Release identity.** A release is identified by the `updated` timestamp in the
JSON-stat response. If that timestamp is already in the `releases` table, the
branch returns `already recorded` and writes nothing, so re-running the
pipeline is always safe.

**Why delete and re-insert.** The table's rows are replaced wholesale on each
release. DoltLite diffs by primary key `(table_id, series_key, period)`, so
unchanged rows produce no diff, and only real revisions, new periods and
removals show up in the history.

**Failed fetches** return status `fetch failed: ...` and leave the history
untouched; the other tables carry on.
