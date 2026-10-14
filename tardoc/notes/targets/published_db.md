DoltLite stores data in a prolly-tree engine that stock SQLite cannot open, so
Datasette cannot read `data/cso.doltlite` directly. This target writes an
ordinary SQLite file derived from the history instead.

The file is rebuilt from scratch on every run, is not committed, and is
deployed to GitHub Pages along with `summary.json` (the landing page's numbers)
by the Update workflow.
