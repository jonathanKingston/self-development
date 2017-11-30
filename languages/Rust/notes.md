Box's go on the heap explicitly.
Logic race and deadlocks are a thing in Rust however data access race isn't an issue

Don't implement Send,Sync for things as it's unsagfe the compiler does it for you.

Arc is used for multiprocess Rc
  Arc useful for immutable access to a hash for example, when the count is 0 we can throw away of the hash

Crossbeam for managing threads that need a lifetime

libstd can be used instead of libcore with no malloc

futures similar to promises in JS however it blocks the threads unlike the event loop.

wargo - for wasm cargo.

--

mutable self called in futures is bad

https://rustbyexample.com/custom_types/constants.html

lifetimes are a way to define a relationship between two things.

Language ergo: https://github.com/rust-lang/rust-roadmap/issues/17
