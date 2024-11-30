# named_func_args

![Rust](https://github.com/DoumanAsh/named_func_args/workflows/Rust/badge.svg?branch=master)
[![Crates.io](https://img.shields.io/crates/v/named_func_args.svg)](https://crates.io/crates/named_func_args)
[![Documentation](https://docs.rs/named_func_args/badge.svg)](https://docs.rs/crate/named_func_args/)

Proc macro to create boilerplate to simulate function with named arguments

Since we cannot have proper functional calls, we might as well use macro to implement it

## Limitations

- References without lifetime parameter will have explicit lifetime with name `'implicit_lifetime`
- Generics may require explicit `?Sized` as struct's generic parameters always `Sized`
- Const generics are attached to actual function struct, even when not used by arguments

## Why?

Just because language lacks basic UX feature, doesn't mean we should have it.

Even if you have tool X to substitute lack of this feature, it doesn't mean language has to suck.

Named arguments are important to avoid writing boilerplate code by hand

## Usage

```rust
 use named_func_args::named_args;

 #[named_args]
 fn my_func<'a, T, T2: Copy + 'a, T3>(arg: T, arg2: T2, arg3: T3, text: &'a str,) -> &'a str where T3: 'a + Copy {
     text
 }

 let result = my_func {
     arg: true,
     arg2: false,
     arg3: 10u8,
     text: "my_func_call"
 }.call();

 assert_eq!(result, "my_func_call");
 ```

## Examples

### Plain function

Just because why not

```rust
 use named_func_args::named_args;

 #[named_args]
 fn my_func() {
 }

 my_func.call();
```

### Multiple arguments of the same type

```rust
 use named_func_args::named_args;

 #[named_args]
 fn my_func<'a>(arg1: &'a str, arg2: &str) -> String {
     format!("{arg1}+{arg2}")
 }

 let result = my_func { arg1: "1", arg2: "2" }.call();
 assert_eq!(result, "1+2");
```

### Multiple generics of the same type

```rust
 use named_func_args::named_args;

 use core::fmt;

 #[named_args]
 fn my_func<'a, T: ?Sized + fmt::Display>(arg1: &'a T, arg2: &T) -> String {
     format!("{arg1}+{arg2}")
 }

 let result = my_func { arg1: "1", arg2: "2" }.call();
 assert_eq!(result, "1+2");
```

### Const generics

```rust
 use named_func_args::named_args;

 use core::fmt;

 #[named_args]
 fn my_func<'a, T: ?Sized + fmt::Display + fmt::Debug, const N: usize>(arg1: &'a T, arg2: &[&'a T; N]) -> String {
     format!("{arg1}+{:?}", arg2)
 }

 let result = my_func { arg1: "1", arg2: &["2"] }.call();
 assert_eq!(result, "1+[\"2\"]");
 ```
