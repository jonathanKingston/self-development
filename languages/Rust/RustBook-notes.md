# Rust Book https://doc.rust-lang.org/book/second-edition/ch02-00-guessing-game-tutorial.html

Notes of interesting facts whilst reading 'The rust book'.

## Chapter 2 https://doc.rust-lang.org/book/second-edition/ch02-00-guessing-game-tutorial.html

- Mutability isn't the default
- `String::new` isn't a static method despite representing one in other languages
- `new` isn't a reserved function name
- `&` is a reference to permit multiple reads without copying data
- Result is an enum with two enum variants
- `cargo update` 
- `cargo doc --open`
- I was interested in how the generics worked for `.parse()` by passing it in the assignment so:
  - "turbofish" `::<>`
- When looking at previous code I was pretty shocked that things like input yielded 
  the program inside a loop.
- Code comments can be bootstrapped but hidden for documentation using a `#`

## Chapter 3

- Constants can't be the return value of a function that could change at runtime
- Immutable variables can become mutable? (I assume through borrowing)
- Shadowing still seems suprising to me in a typed system
- I keep forgetting there is a char type; single not double quotes. Do multiple emoji's that are create one emoji count as a char?
- Match arms need to match in type
- Conditions in if expressions need to be bool or will be a compile error