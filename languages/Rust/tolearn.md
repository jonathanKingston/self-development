Trait Objects
Pointers
&str and str relationships (Stack and heap)
Generics (bound expressions)
Static vs dynamic despatch
impl trait - https://github.com/rust-lang/rfcs/blob/master/text/1522-conservative-impl-trait.md
Futures
- https://dev.to/mindflavor/rust-futures-an-uneducated-short-and-hopefully-not-boring-tutorial---part-1-3k3
  - Uses impl trait to load functions, also advises the use of lowercase functions instead of enum variants such as Ok.
- http://paulkernfeld.com/2018/01/20/future-by-example.html

Terminal applications with https://github.com/redox-os/termion used in Sodium
- Rope data structure used in both sodium and gxi

MIR - mid-level intermediate language: https://github.com/rust-lang/rfcs/blob/master/text/1211-mir.md
  - Removal of language suage intermediate used for compile checking
  - https://github.com/solson/miri is an experimental interpreter for MIR.