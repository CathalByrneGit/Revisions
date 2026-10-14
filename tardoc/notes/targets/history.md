Reads `dolt_diff_observations` over the whole commit graph: every row that was
added, modified or removed, with the commit that did it. Because each commit is
exactly one release of one table, joining commits to release tags tells us which
CSO release changed which value.

Classification happens downstream in `derive_revisions()`:

| Change in DoltLite | In the first release of a table | Later releases |
|---|---|---|
| added | `baseline` | `first_print` |
| modified | n/a | `revision` |
| removed | n/a | `removed` |

A "modified" row whose value went from missing to missing is ignored.
