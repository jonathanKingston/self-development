# [Rust Book](https://doc.rust-lang.org/book/second-edition/)

Notes of interesting facts whilst reading 'The rust book'.

## [Chapter 2](https://doc.rust-lang.org/book/second-edition/ch02-00-guessing-game-tutorial.html)

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

## [Chapter 3](https://doc.rust-lang.org/book/second-edition/ch03-00-common-programming-concepts.html)

- Constants can't be the return value of a function that could change at runtime
- Return in a block, returns out of the containing method.
- 5000i64 as i8 doesn't panic!? (TORAISE?)
- Immutable variables can become mutable? (I assume through borrowing)
- Shadowing still seems suprising to me in a typed system
- I keep forgetting there is a char type; single not double quotes. Do multiple emoji's that are create one emoji count as a char?
- Match arms need to match in type
- Conditions in if expressions need to be bool or will be a compile error


### Team questions from this week

- What is the difference between i32 and i64
- What is isize and usize
- String vs &str?
- Difference between heap and stack
- Lifetimes on a Struct construction

```
struct Person <'a> {
    name: &'a str
}

fn main() {
  create_person();
}

fn create_person<'b> -> Person<'b> {
  let name = "person";
  Person {
    name
  }
}
```

## [Chapter 4](https://doc.rust-lang.org/book/second-edition/ch04-00-understanding-ownership.html)

### [Ownership](https://doc.rust-lang.org/book/second-edition/ch04-01-what-is-ownership.html)
- The return reference of a function impacts the variable it assigns to:
```
  fn b() -> &mut Vec<i32> {
  }
  let c = b(); // c is a mutable reference even though not declared this way.
```
- Simple types are copied where as complex are moved by changing their pointer on the stack
- Copy trait is for stack data types
- Copy can't be implemented on something that implements the Drop trait or data within it has Drop
- Copy is called when assigning to a new variable, it's the same as clone given that theita isn't on the heap.
- Drop trait is for Heap data types and is used to clean up memory
- `clone` is a method that is used to deeply copy heap data and also create

### [References and borrowing](https://doc.rust-lang.org/book/second-edition/ch04-02-references-and-borrowing.html)
- Ampersands are references, they create another pointer on the stack to the stack value
- * dereferences
- References don't deallocate data like stack pointers to the heap would.
- References as function parameters are called borrowing
- When a borrow is dropped you hand it back to the owner.
- References are not mutable by default much like variables
- You can have multiple immutable references but only one mutable
- Dangling pointers are where a pointer still exists for memory that has been freed.
  - Rust prevents dangling pointers by requiring lifetimes to keep the memory around longer than it's normal dropping at the end of the scope.
- Returning a value from a function moves it out into the calling scope.

### [The Slice type](https://doc.rust-lang.org/book/second-edition/ch04-03-slices.html)

- Slices make an immutable borrow to the place on the heap
- String methods require a mutable borrow to change the data which ends up causing a compile time error
- String literals `let s = "me";` is a slice.
- Borrowing `&String` in a function is likely better written as borrowing a slice `&str`. This is because it can then be easily used for Strings and strs
- Slices aren't just for strings, slices can be for arrays too.
- Slices prevent the data changing to the range reference to the data on the heap. Slices utilise borrowing to prevent this from happening.

## [Chapter 5](https://doc.rust-lang.org/book/second-edition/ch05-00-structs.html)
- Structs key name + type are called fields.
- Creating an instance of the struct requires providing concrete values for the fields.
- Mutability isn't possible for just one field
- Destructuring from one struct to another using `..`
  - This appears to be how the `Default` trait works which defines a struct with default values and destructures that way.
- Tuple structs allow the naming of the type but not the fields. It also allows the definition of the types themselves `struct Color(i32, i32, i32)`
- Unit structs are possible too `struct Poop()`
- Structs that don't own their data like `&str` require lifetimes
- Short hands like JS and destructuring `..otherStruct` is also possible too
- `self`, `&self` or `&mut self` are the first argument of methods.
- Associated functions = static methods.

## [Chapter 6](https://doc.rust-lang.org/book/second-edition/ch06-00-enums.html)
### [Defining an Enum](https://doc.rust-lang.org/book/second-edition/ch06-01-defining-an-enum.html)
- Enums can contain anything, all variants can be different in the fields they accept
- Enums can have methods like structs
- `Option`s are enums and `Some` and `None`, are in the std prelude and are an alternative to null
   - https://doc.rust-lang.org/std/option/enum.Option.html

### [The `match` control flow operator](https://doc.rust-lang.org/book/second-edition/ch06-02-match.html)
- Match "arms" are a pattern and code.
- We can extract values in enum variants much like other code `match x { thing(c) => println!("{}", c); }`
- Match is exhustive always.
- `_` is used to match all other cases.

### [Concise Control Flow with `if let`](https://doc.rust-lang.org/book/second-edition/ch06-03-if-let.html)
- `if let Some(3) = some_u8_value {` can be used to remove boilerplate of a single match.
- `Option<T>` really helps reduce code errors that other languages would create.

## [Chapter 7 - Using Modules to Reuse and Organize Code](https://doc.rust-lang.org/book/second-edition/ch07-00-modules.html)

- `mod` declares a module in a block or points to a file.

### [`mod` and the Filesystem](https://doc.rust-lang.org/book/second-edition/ch07-01-mod-and-the-filesystem.html)

- Uses `lib.rs` instead of `main.rs`
- Multiple `mod` can be within the same file.
- Modules can be in `main.rs` and also `mod` inside another `mod`
- `mod modname;` expands a `modname.rs` file as if it were in a block.
  - If the mod has submodules the path should be `modname/mod.rs`

### [Controlling Visibility with `pub`](https://doc.rust-lang.org/book/second-edition/ch07-02-controlling-visibility-with-pub.html)
- `extern crate name` for use in `main.rs` imports in the local name lib.rs
- Using `pub` makes the unused method warnings go away. As external code might be using it at that point.
  - `mod` need to be `pub` too if it's not being used internally.
- Root mod's are accessible within the same file.

### [Referring to Names in Different Modules](https://doc.rust-lang.org/book/second-edition/ch07-03-importing-names-with-use.html)
- `use x::y::z;` exposes the `z` module to the current scope. `z::method_name()` is possible then.
  - `z::ab` isn't brought into the scope too. The code still needs to access it the same way with `z::ab::method()`
- use can import just a module with `use x::module_name;`
- enum variants are a form of namespace too, so can imported individually too. `use x::ip::ipv6;`
- `use x::*;` brings in all the `x` names (fns, vars, or variants) into scope.
- Leading colons mean the root namespace, `super` means the parent namespace.
- `use super::x;` can bring in parent namespaces, doing this in tests is helpful to reduce prefixing everything with `super`

## [Common Collections](https://doc.rust-lang.org/book/second-edition/ch08-00-common-collections.html)
- Collections represent multiple values and aren't fixed in size and stored on the heap.

### [Storing Lists of Values with Vectors](https://doc.rust-lang.org/book/second-edition/ch08-01-vectors.html)
- All items need to be the same type
- `vec![]` and `Vec::new()`
- Dropping the vector, drops it's items.
- `let third: &i32 = &v[2];` and `let third: Option<&i32> = v.get(2);` allow array access.
