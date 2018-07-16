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
- Shadowing still seems surprising to me in a typed system
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
- The return reference of a function impacts the variable it assigns to (giving ownership):
```
  fn b() -> &mut Vec<i32> {
  }
  let c = b(); // c is a mutable reference even though not declared this way.
```
- Simple types are copied where as complex are moved by changing their pointer on the stack
- Copy trait is for stack data types
- Copy can't be implemented on something that implements the Drop trait or data within it has Drop
- Copy is called when assigning to a new variable, it's the same as clone given that it isn't on the heap.
- Drop trait is for Heap data types and is used to clean up memory
- `clone` is a method that is used to deeply copy heap data and also create

#### Other resources
- [Ownership in Rust, Part 1](https://medium.com/@thomascountz/ownership-in-rust-part-1-112036b1126b)
- [Ownership in Rust, Part 2](https://medium.com/@thomascountz/ownership-in-rust-part-2-c3e1da89956e)

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
- Match is exhaustive always.
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
- Borrowing rules apply to owning items of a vector whilst trying to modify the vector.i
  - If you have a immutable reference to a vector item you can't modify the vector size.
  - This happens because sometimes the contents of a vector has to move when they grow.
- To change the items in a mutable vector `*` is needed to dereference the value.
- To get a vector containing different types, use an enum
  - Also a trait object but this will require runtime checking.

### [Storing UTF-8 Encoded Text with Strings](https://doc.rust-lang.org/stable/book/second-edition/ch08-02-strings.html)

- Strings in the core language are `str` known as string slices. They are usually found as a borrow `&str`. Literals `"stringy"` are string slices.
- The `String` type isn't in the core, it's in the standard lib.
- The `Display` trait provides a `to_string()` method.
- `+` and `format!` are both options for concatenating or `push_str`.
- `&String` with cooerce to `&str` by using a "deref coercion".
- `string1 + string2` will take ownership of string1 `add(self, &str)`.
- `format!` doesn't take ownership over any of it's values.
- Indexing into a string isn't permitted because of unicode characters taking more than one byte.
  - Indexing also isn't possible because it's expected that indexing is `O(1)` complexity but working out valid chars is more complex.
- Rust understands strings as u8 bytes, Chars and Graphemes.
  - Chars also contain other characters that in Graphemes would be overlayed ontop of other Chars.
				- `.chars()` `.bytes()` (grapheme iterators aren't possible in stdlib)
- Rust does allow ranges to create `str` from a String as it's a little more explicit about wanting a `str`
  - However Rust panics if the range isn't valid UTF8

### [Storing Keys with Associated Values in Hash Maps](https://doc.rust-lang.org/stable/book/second-edition/ch08-03-hash-maps.html)
- `HashMap<K, V>` are another collection type also called objects, hashes and many other names in other languages.
- To add values `insert(k, v)` is the method to call.
- Hash maps are 'homogeneous' like Vec in that all keys need to be of a single type and values need to be of a single type.
- vectors of tuples can use `collect` which will gather data and convert it into another type.
  - `vec![(k, v), (k2, v2)].collect();`
  - `HashMap<_, _>` means the compiler can infer from the data given.
#### [Hash Maps and OWnership](https://doc.rust-lang.org/stable/book/second-edition/ch08-03-hash-maps.html#hash-maps-and-ownership)
- Primitive types that implement `Copy` are copied into the hash map, owned types are moved into the hash map. In both cases it owns the data but `String` variables for example would be moved into the hash map.
- Owned typed can be passed in by reference but the lifetime of the type has to outlive the hash map.

#### [Only Inserting a Value If the Key Has No Value](https://doc.rust-lang.org/stable/book/second-edition/ch08-03-hash-maps.html#only-inserting-a-value-if-the-key-has-no-value)
- Using the `entry` api returns an `Entry` enum which makes it easier to only insert when empty.
  - `scores.entry("blue".to_string()).or_insert(50)`
  - `or_insert` provides a mutable reference to either the existing value in the hash map or inserts one and returns that.
    - This allows further modification of the value with a dref:
      ```
      let x = map.entry(b).or_insert(0);
      *x += 1;
      ```
#### [Hashing functions](https://doc.rust-lang.org/stable/book/second-edition/ch08-03-hash-maps.html#hashing-functions)
- The default hasing function is DoS resistant but isn't the fastest.
- `hasher` is a trait which provides the hashing function.

## [Error Handling](https://doc.rust-lang.org/stable/book/second-edition/ch09-00-error-handling.html)

### [Unrecoverable Errors with `panic!`](https://doc.rust-lang.org/stable/book/second-edition/ch09-01-unrecoverable-errors-with-panic.html)

- `[profile.release]\npanic = 'abort'` makes panic not unwind and cleanup which is faster. (perhaps useful for WASM?)

### [Recoverable Errors with `Result`](https://doc.rust-lang.org/stable/book/second-edition/ch09-02-recoverable-errors-with-result.html)

- `Result<T, E>` permits any `T` and `E` types for returning in a function.
  - `T` and `E` are known as Generic types, when they get used, they become concrete types.
  - This sets up the `Error(E)` and `Ok(T)` enum variants.
- As always Enums provide a way to require handling the variants.
- The variants are in the prelude so no importing required.
- Match guards: `Err(ref error) if error.kind() == ErrorKind::NotFound => {`
  - `ref` prevents error being owned by the "match guard"
  - `&` in the match expression, matches a reference to a value, where as `ref` returns a reference to a value.
- `unwrap()` returns the `Ok` variant and panics on a `Err`.
  - `expect()` allows you to specify the error.
- `?` is a shortcut for propagating errors from one function to it's caller.
  - Instead of having to write a match expression, it `return Err(E);` inside the expression.
  - `?` also go through `from()` from the `From` trait to convert into the return type of the function.
  - **note:** There is Nightly only support for `Option::None` to be treated like an `Err` by implementing the `impl From<NoneError> for YourErrorType`.
  - **note:** the book suggests `main` can't return `Result` and thus can't use `?` - https://github.com/rust-lang/rust/issues/43301 will allow this once stable.

### [To `panic!` or Not to `panic!`](https://doc.rust-lang.org/stable/book/second-edition/ch09-03-to-panic-or-not-to-panic.html)

- `Result` is a better default when it's not clear if the error isn't recoverable.
- `unwrap` and `expect` are useful for prototyping or example code.
  - Also useful in a test as it will fail the whole test.
- Hardcoded `unwrap` can be ok when it's clear it will never fail:
  - `let home: IpAddr = "127.0.0.1".parse().unwrap();`
  - If the IP address was user input then `unwrap` would be unsafe here.
- If code doesn't expect to be in the bad state occasionaly a panic seems advisable.
- Using types to guard a function is better than having runtime failures.
  - Creating a type that calls `panic!` on creation of the type is one possibility to reduce boiler plate checking of values.
  - Types need a private interface for their values if they use this mechanism to prevent manually creating the struct.
```
pub struct Guess {
    value: u32,
}

impl Guess {
    pub fn new(value: u32) -> Guess {
        if value < 1 || value > 100 {
            panic!("Guess value must be between 1 and 100, got {}.", value);
        }

        Guess {
            value
        }
    }

    pub fn value(&self) -> u32 {
        self.value
    }
}
```

## [Generic Types, Traits, and Lifetimes](https://doc.rust-lang.org/stable/book/second-edition/ch10-00-generics.html)

- Generics are similar to how functions can take parameters of any value, generics allow a function to act on different types.
- `[T]` in a function signature can be used, is it possible to use for anything other than vectors?

### [Generic Data Types](https://doc.rust-lang.org/stable/book/second-edition/ch10-01-syntax.html)

- The programmer names their own generic type names like `fn largest<T>(list: &[T]) -> T {` where `T` is the  generic.
  - `T` is also know as a "Type parameter"
  - Read as "`largest` is a generic over some type `T`"
- The generic type has to be specified such that any behaviour is possible with that generic.
  - The code sample uses `>` but the type definition didn't specify the `std::cmp::PartialOrd` trait.
- `struct` can also take Generics in a similar manner using `struct Point<T> {` where `T` can be used for the fields instead of concrete types.
- Generic parameters can take multiple generics just like function parameters.
  - `struct Point<T, U> {`
- `Enum`'s can also take generics like `Option<T>` and `Result<T, E>` do.
  - `fn b() -> Result<String, std::io::Error> {` turns the generic into concrete types.
- `impl` can accept traits also which make it clear that this is still a generic method.
  - `impl<T> Point<T> { ...` is an implementation for any of `T`
  - `impl Point<f32> {` can be used also just for a concrete implementation.
    - This permits the programmer to add specific methods just for that concrete type.
- Methods in structs can also have their own generic parameters:
```
impl<T, U> Point<T, U> {
    fn mixup<V, W>(self, other: Point<V, W>) -> Point<T, W> {
```
  - This means it can take a different point, with different concrete types and still cope with that.
- Rust uses `monomorphization` to turn generics into concrete at compile time so there isn't a cost.

### [Traits: Defining Shared Behavior](https://doc.rust-lang.org/stable/book/second-edition/ch10-02-traits.html)

- Traits allow types to share behaviour.
- Types that implement the same trait can be passed around easily and code will be able to interact with it in the same way.
- `pub trait Summary { fn summarize(&self) -> String; }`
   - Defines a 'Summary' trait that has a `summarize` method that returns a `String`.
   - We only need to declare the method signatures such that the concrete type can implement them.
   - Any type implementing this trait must implement all the methods.
- `impl TraitName for Type` is the way to specify this behaviour on the type.

- If another crate wants to implement a trait on their type they would need to `use other::Sumary;` to import the trait.
- Also the trait has to be public, so that other types can implement it.
  - Can a private trait be used in functions constraints?
- A trait or type need to be owned by a crate for it to be implemented.
  - Either trait or type need to be owned by the crate.
  - A programmer can add a trait to a standard type if they own the trait for example.
- We can't implement external traits on external types to keep *coherence* specifically the *orphan rule*.
  - External code can't break my code.
- Traits can have default implementations which are just method bodies that don't need defining in the concrete type.
```
pub trait Speak {
    fn shout() -> String {
        String::from(self.words.make_ascii_uppercase());
    }
}
```
- Default implementations can also call `self.other_method` where `other_method` comes from the concrete type.
  - The overriding implementation can't call the default.

#### [Trait bounds](https://doc.rust-lang.org/stable/book/second-edition/ch10-02-traits.html#trait-bounds)
- `pub fn shout<T: Speak>(beep: T) -> String {` is generic over type `T` that implements `Speak`
- Multiple trait bounds can be applied to a generic using `+`.
```
fn shout<T: Speak + Display, U: Clone + Debug>(beep: T, thing: U) -> String {
```
Or:
```
fn shout<T, U>(beep: T, thing: U) -> String
  where T: Speak + Display,
        U: Clone + Debug {
```
- `PartialOrd + Copy` trait bound restricts only types that are orderable and copyable (primitive types of known size)

- Trait bounds can be used in an impl block to only specify methods to types that satisfy the bound.
  - `impl<T: Display + PartialOrd> Pair<T> {` is an impl block for the Pair struct for when `T` is od `Display` and `PartialOrd`
  - The same can be used to implement any trait: `impl<T: Display> ToString for T {` implements a `ToString` trait on any type that also implements `Display`.
    - Known as a "blanket implementation".

### [Validating References with Lifetimes](https://doc.rust-lang.org/stable/book/second-edition/ch10-03-lifetime-syntax.html)

- Lifetimes guarantee with a generic that references last as long as we want them to live.
- Every reference has a lifetime, even if most are implied.
  - Rust requires annotation of lifetimes when they can't be inferred.
- Lifetimes are intended to prevent 'dangling references' where a reference points to different data it was supposed to.

- Rust doesn't permit reading a variable before it has a value assigned to it. 

- The *borrow checker* is what prevents variable access of dropped references. When a variable doesn't live long enough, it causes a compile time error.
  - The borrow checker checks the length of the reference lifetime to make sure if lives as long as the variable that is trying to use it.

- When a function takes borrowed values, it might be unclear when it returns a borrowed value what the lifetime of the values are.
- Lifetimes **don't** change how long the reference lives.
- `&'a mut i32` is an `a` lifetime annotation on a mutable i32
- Lifetimes explain how multiple references interact

- When a function has references from other code it's likely rust can't determine the lifetimes
- When a function is called, it will get concrete lifetimes passed in rather than generic annotations.
  - The shortest concrete lifetime between all concrete lifetimes that are given to the function.
```
    fn bigger<'a>(n: &'a i32, j: &'a i32) -> &'a i32 {
        if n > j {
            return n;
        }
        j
    }
    let x = 12;
    let y = 2;
    let b = bigger(&x, &y);
    println!("bigger number: {}", b);
```
- It's not clear to Rust how long the return is permitted to live if there is more than one reference, without the annotations above the code won't compile.


- "Lifetimes are descriptive, not prescriptive."

- When returning a reference from a function, the lifetime has to match the lifetime of one of the params.
- Lifetimes are about connecting the relationships of data lifetimes not trying to expand them.

- Structs can hold references too, they need to have annotations on every field.
  - Lifetimes here help anotate that our fields lifetime outlives the struct.
```
struct Boop<'a> {
    title: &'a str,
}
```

- The cases where Rust can infer the lifetimes are called *lifetime elision rules*
- Lifetimes on function or method params are called *input lifetimes* and *output lifetimes* are for return values.
- Three elision rules:
  - Each input parameter that is a reference gets it's own lifetime: `fn test<'a, 'b>(d: &'a i32, e: &'b i32)`
  - If there is only one input lifetime, that is assigned to an output that is a reference: `fn thing<'a>(b: &'a i32) -> &'a i32`
  - If one of the reference is `&self` or `&mut self`, then elision will choose that as the lifetime.

- If the elision rules resolve all reference lifetimes, then no lifetime annotations are needed to compile.
  - If there are some left, the compiler will ask for those to be filled.

- `&'static` is special in that it lives for the lifetime of the program.
  - String literals automatically have static lifetimes.
  - Suggestion to use static, is usually a lifetime error and people should think before using it.


## [Writing Automated Tests](https://doc.rust-lang.org/stable/book/second-edition/ch11-00-testing.html)
