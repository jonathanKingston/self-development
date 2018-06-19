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

## Chapter 4

### Ownership
- Simple types are copied where as complex are moved by changing their pointer on the stack
- Copy trait is for stack data types
- Copy can't be implemented on something that implements the Drop trait or data within it has Drop
- Copy is called when assigning to a new variable, it's the same as clone given that theita isn't on the heap.
- Drop trait is for Heap data types and is used to clean up memory
- `clone` is a method that is used to deeply copy heap data and also create

### References and borrowing
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

### The Slice type

- Slices make an immutable borrow to the place on the heap
- String methods require a mutable borrow to change the data which ends up causing a compile time error
- String literals `let s = "me";` is a slice.
- Borrowing `&String` in a function is likely better written as borrowing a slice `&str`. This is because it can then be easily used for Strings and strs
- Slices aren't just for strings, slices can be for arrays too.
- Slices prevent the data changing to the range reference to the data on the heap. Slices utilise borrowing to prevent this from happening.

## Chapter 5
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

## Chapter 6
### Defining an Enum
- Enums can contain anything, all variants can be different in the fields they accept
- Enums can have methods like structs
- `Option`s are enums and `Some` and `None`, are in the std prelude and are an alternative to null
   - https://doc.rust-lang.org/std/option/enum.Option.html

# The `match` control flow operator


