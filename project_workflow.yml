name: Generate Mermaid Diagrams
on:
  push:
    paths:
      - 'docs/*.mmd'
  pull_request:
    paths:
      - 'docs/*.mmd'

jobs:
  generate-diagrams:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Generate SVG from Mermaid
        uses: mikefarah/mermaid-action@v1
        with:
          mermaid-version: 9.4.3
          input: 'docs/workflow.mmd'
          output: 'docs/workflow.svg'
      - name: Commit generated files
        run: |
          git config --local user.email "action@github.com"
          git config --local user.name "GitHub Action"
          git add docs/workflow.svg
          git commit -m "Auto-generate workflow diagram" || exit 0
          git push
