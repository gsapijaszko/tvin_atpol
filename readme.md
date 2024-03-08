# Old data in 21 century
Grzegorz Sapijaszko

A couple of scripts to deal with a quite old (30+ years) data, made in
DOS/Win 3.1 programs.

### ATPOL extracts

Text files named `0123.LOC` which contains species name in header and
tabular data with ATPOL square, localization, author, year, longitude
(`Dl:`) and latitude (`Sz`) as three columns with degrees, minutes and
seconds. Below an example of such file.

    Raport rekordow gatunku: 200
    Atriplex roseum L.

    Liczba kwadratow: 22
    R173807  AD49    Kr�pa Ma�a, pow. Zielona G�ra,     1  Schube Th      1903    Dl:   0  0  0   Sz:   0  0  0
    R173813  AD57    Lipno, pow. Zielona G�ra,          1  Schube Th      1903    Dl:   0  0  0   Sz:   0  0  0
    R173817  AD59    Kr�pa, pow. Zielona G�ra,          1  Gruhl K        1929    Dl:   0  0  0   Sz:   0  0  0

The files uses [Mazovia](https://en.wikipedia.org/wiki/Mazovia_encoding)
encoding. The content of the file can be read with
`extract_data_from_old_atpol()` function from
[{atpolR}](https://github.com/gsapijaszko/atpolR) (Sapijaszko 2024)
package, like:

``` r
atpolR::extract_data_from_old_atpol(filename = system.file("extdata/0200.LOC", package = "atpolR"))
```

    # A tibble: 22 × 10
         lon   lat record_type record_number atpol_square description               
       <dbl> <dbl> <chr>               <dbl> <chr>        <chr>                     
     1     0     0 R                  173807 AD49         Krępa Mała, pow. Zielona …
     2     0     0 R                  173813 AD57         Lipno, pow. Zielona Góra  
     3     0     0 R                  173817 AD59         Krępa, pow. Zielona Góra  
     4     0     0 R                  173892 AE29         Bolesławiec               
     5     0     0 R                  173902 AE35         Zgorzelec                 
     6     0     0 R                  174013 BD50         Łaz, pow. Zielona Góra    
     7     0     0 R                  174018 BD51         Lubięcin, pow. Nowa Sól   
     8     0     0 R                  174029 BD60         Nowa Sól                  
     9     0     0 R                  174103 BE05         Krzelów, pow. Wołów       
    10     0     0 R                  174149 BE18         Prusice, pow. Milicz      
    # ℹ 12 more rows
    # ℹ 4 more variables: date <dttm>, number_of_entries <dbl>, author_name <chr>,
    #   species <chr>

It returns a tibble with coerced and formatted data.

### Bibliography

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-R-atpolR" class="csl-entry">

Sapijaszko, Grzegorz. 2024. *atpolR: ATPOL Grid Implementation*.
<https://github.com/gsapijaszko/atpolR>.

</div>

</div>
