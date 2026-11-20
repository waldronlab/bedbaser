# version 1.6.0

## SIGNIFICANT USER-VISIBLE CHANGES

- Update bedbase to 0.15.0 (#32, @jwokaty)

## NEW FEATURES

- Add a `host` argument to `BEDbase()` for using another API server, such
  as a development or test server (#5, @nsheff)
- Add a `show()` method for `BEDbase` objects (#31, @lwaldron)
- Add `after` to `bb_list_beds()` for paging by the id of the last record
  (#34, @jwokaty)

## BUG FIXES AND MINOR IMPROVEMENTS

- Fix `bb_list_beds()` failing on later pages, and report bedhost errors
  clearly instead of failing or requiring xml2 (#34, @jwokaty)

# version 1.3.5

- Incorporated test_request parameter
- Improved how bedbaser constructs BED file url

# version 1.3.1

- Update bedbase to 0.12.1
- Replace download.file with curl::curl_download
- Fix test for bb_bed_text_search

# version 1.1.1

- Update bedbase to 0.10.6
- Update examples
- Remove bigbed

# version 0.99.0

- Initial release
