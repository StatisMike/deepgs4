
<!-- README.md is generated from README.Rmd. Please edit that file -->

# deepgs4

## Overview

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/StatisMike/deepgs4/branch/main/graph/badge.svg)](https://app.codecov.io/gh/StatisMike/deepgs4?branch=main)
<!-- badges: end -->

The goal of `deepgs4` is to provide a low-level access to Google Sheets
API, in opposition to
<a href="https://github.com/tidyverse/googlesheets4"
target="_blank">googlesheets4</a> package, which provides high-level
access. It basically means that `deepgs4` allows operations on
*cell-level*, and `googlesheets4` on *sheet-level*.

If you want to create great looking report in Google Sheet - and produce
them programatically, with custom formats, addition of formulas,
creation of charts, merge cells etc: the low-level access is an
inevitability. If you want to only read, write and append
*data.frame-like* data into and from Google Sheets the high-level access
would be enough.

## Installation

You can install the development version of deepgs4 from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("StatisMike/deepgs4")
```

## Currently available functionalities

Ultimate goal of this package is to bind every Google Sheets API object
on the R side to allow for as easy as possible request generation. There
is still a long way to go, but currently some functionalities are
provided:

-   `send_create_req()` allowing Spreadsheet creation
-   `send_get_req()` allowing retrieval of data by Sheets API
-   `send_batchUpdate_req()` allowing updating spreadsheets with
    lower-level `dgs4Req` objects:
    -   Sheet creation, update and deletion
    -   CellData creation allowing:
        -   entering numeric (and date/datetime with the help of
            `dgs4_serial_number()`), character, boolean and formula
            values
        -   formatting individual cells (or parts of the cells)
        -   coercing into *RowData* and *GridData*
        -   helpers: `to_RowData_from_df()` and `to_GridData_from_df()`
            are also available for coercing `data.frame` into these
            objects
    -   Updating and appending *RowData* to the sheet
    -   Modifying the columns and rows properties (eg. their width and
        height)
    -   Creation, modification and deletion of conditional format rules
    -   Creation, modification and deletion of BasicCharts
    -   Pasting data into sheets: either by cut-paste or copy-paste from
        the same spreadsheet or by providing delimited text or html
-   package oauth client available in `dgs4_auth()` is currently running
    in development mode, so getting your own token through `deepgs4`
    alone is not currently available. If you plan on contributing,
    contact me to add your google account into allowed emails.
    -   it is possible to use `googlesheets4` token with
        `dgs4_auth(token = googlesheets4::gs4_token())`
    -   or provide a service account with
        `dgs4_auth(path = "path_to_service_account_json.json")`
