# version 1.5.3

- Update bedbase to 0.15.0
- Add `after` to `bb_list_beds()` for paging by the id of the last record
- Fix `bb_list_beds()` failing on later pages, where bedhost no longer
  returns `count`
- `bb_list_beds()` reports bedhost errors, such as an `offset` over 10000,
  instead of failing or requiring xml2 to parse the response

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
