---
output:
  md_document:
    variant: markdown_github
---

# isogasex 0.2.00

5/11/2019
Updated from package tdllicor, now runs on current versions of R.

# isogasex 0.2.01

6/6/2019
Resolve all R CHECK errors, warnings, notes.
Updated all documentation using Roxygen.
Modern package update complete.

# isogasex 0.2.02

6/9/2019
Two version of input parameter file: yaml and xls.
When copying decimal values from xls to yaml, copy full number from value bar (16 digits) instead of the 10 digits from the table cell.

# isogasex 0.2.03

6/14/2019
Uses new RLicor package to read in Licor 6400 and 6800 files.
Currently works with 6400 version.
Renamed read_Licor to get_Licor to reduce chance of conflict with RLicor::read_Licor().

# isogasex 0.2.04

7/2/2019
Works with Licor 6800 files.

# isogasex 0.2.05

7/15/2019
Updated warning message for Site mismatch between Template and TDL files.

