# IBM-8b-10b-encoder

This is an Ada95 library implementing the ![1983 IBM 8b/10b encoding scheme](https://en.wikipedia.org/wiki/8b/10b_encoding). 

The outputs can be checked against the table found at https://www.cs.cornell.edu/courses/cs3410/2013sp/lab/tables/dy_encoding_table.pdf

## Improvement Goals
This library only outputs to a fixed Endianness (both at the byte and bit level). Future versions will allow the user of the library to select at least the byte Endianness.
