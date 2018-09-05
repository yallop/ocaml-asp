## PPM image format benchmark

#### Syntax:

The [PPM P3 image format][ppm-format] for colour images.

#### Benchmark

Parse and validate ppm files, checking that
  * the number of entries in the body matches the metadata in the header
  * the maximum colour value matches the metadata in the header

Return a single boolean to indicate validity.

[ppm-format]: https://en.wikipedia.org/wiki/Netpbm_format
