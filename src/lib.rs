//!Proc macro to create boilerplate to simulate function with named arguments
//!
//!Since we cannot have proper functional calls, we might as well use macro to implement it
//!
//!## Limitations
//!
//!- References without lifetime parameter will have explicit lifetime with name `'implicit_lifetime`
//!- Generics may require explicit `?Sized` as struct's generic parameters always `Sized`
//!- Const generics are attached to actual function struct, even when not used by arguments
//!
//!## Why?
//!
//!Just because language lacks basic UX feature, doesn't mean we should have it.
//!
//!Even if you have tool X to substitute lack of this feature, it doesn't mean language has to suck.
//!
//!Named arguments are important to avoid writing boilerplate code by hand
//!
//!## Usage
//!
//!```rust
//! use named_func_args::named_args;
//!
//! #[named_args]
//! fn my_func<'a, T, T2: Copy + 'a, T3>(arg: T, arg2: T2, arg3: T3, text: &'a str,) -> &'a str where T3: 'a + Copy {
//!     text
//! }
//!
//! let result = my_func {
//!     arg: true,
//!     arg2: false,
//!     arg3: 10u8,
//!     text: "my_func_call"
//! }.call();
//!
//! assert_eq!(result, "my_func_call");
//! ```
//!
//!## Examples
//!
//!### Plain function
//!
//!Just because why not
//!
//!```rust
//! use named_func_args::named_args;
//!
//! #[named_args]
//! fn my_func() {
//! }
//!
//! my_func.call();
//!```
//!
//!### Multiple arguments of the same type
//!
//!```rust
//! use named_func_args::named_args;
//!
//! #[named_args]
//! fn my_func<'a>(arg1: &'a str, arg2: &str) -> String {
//!     format!("{arg1}+{arg2}")
//! }
//!
//! let result = my_func { arg1: "1", arg2: "2" }.call();
//! assert_eq!(result, "1+2");
//!```
//!
//!### Multiple generics of the same type
//!
//!```rust
//! use named_func_args::named_args;
//!
//! use core::fmt;
//!
//! #[named_args]
//! fn my_func<'a, T: ?Sized + fmt::Display>(arg1: &'a T, arg2: &T) -> String {
//!     format!("{arg1}+{arg2}")
//! }
//!
//! let result = my_func { arg1: "1", arg2: "2" }.call();
//! assert_eq!(result, "1+2");
//!```
//!
//!### Const generics
//!
//!```rust
//! use named_func_args::named_args;
//!
//! use core::fmt;
//!
//! #[named_args]
//! fn my_func<'a, T: ?Sized + fmt::Display + fmt::Debug, const N: usize>(arg1: &'a T, arg2: &[&'a T; N]) -> String {
//!     format!("{arg1}+{:?}", arg2)
//! }
//!
//! let result = my_func { arg1: "1", arg2: &["2"] }.call();
//! assert_eq!(result, "1+[\"2\"]");
//! ```


#![allow(clippy::style)]

extern crate proc_macro;

use proc_macro::{TokenStream, TokenTree, Span, Delimiter};

use core::fmt::{self, Write};
use std::collections::{HashMap, hash_map};

const TAB: &str = "    ";

#[cold]
#[inline(never)]
fn compile_error(span: Span, error: &str) -> TokenStream {
    if let Some(source) = span.source_text() {
        format!("compile_error!(\"{source}\n\n{error}\");").parse().unwrap()
    } else {
        format!("compile_error!(\"{error}\");").parse().unwrap()
    }
}

struct GenericProps {
    is_const: bool,
    bounds: Vec<TokenTree>
}

impl GenericProps {
    #[inline(always)]
    const fn new() -> Self {
        Self::with_bounds(Vec::new())
    }

    #[inline(always)]
    const fn with_bounds(bounds: Vec<TokenTree>) -> Self {
        Self {
            is_const: false,
            bounds
        }
    }

    #[inline(always)]
    const fn new_const() -> Self {
        Self {
            is_const: true,
            bounds: Vec::new()
        }
    }

    #[inline(always)]
    fn extend_bounds(&mut self, bounds: &mut Vec<TokenTree>) {
        self.bounds.append(bounds);
    }
}

struct GenericType<'a> {
    ident: &'a str,
    props: &'a GenericProps
}

impl fmt::Display for GenericType<'_> {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.props.is_const {
            fmt.write_str("const ")?;
        }

        fmt.write_str(self.ident)?;

        if !self.props.bounds.is_empty() {
            fmt.write_str(":")?;
            for token in self.props.bounds.iter() {
                fmt.write_fmt(format_args!("{token}"))?
            }
        }

        Ok(())
    }
}

struct ArgumentFmt<'a>(&'a [TokenTree]);

impl fmt::Display for ArgumentFmt<'_> {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut tokens = self.0.iter();
        while let Some(token) = tokens.next() {
            if let TokenTree::Punct(punct) = token {
                fmt.write_char(punct.as_char())?;
                if punct.as_char() == '\'' {
                    if let Some(token) = tokens.next() {
                        fmt.write_fmt(format_args!("{token} "))?
                    }
                }
            } else {
                fmt.write_fmt(format_args!("{token}"))?
            }
        }
        Ok(())
    }
}

#[proc_macro_attribute]
///Creates function wrapper struct that takes arguments with the same name as in function definition and implements method `call(self)`
pub fn named_args(_attr: TokenStream, input: TokenStream) -> TokenStream {
    let mut function_def = Vec::new();

    let mut has_implicit_lifetime = false;
    let mut fn_name = String::new();
    let mut code_block = None;
    let mut lifetimes = HashMap::new();
    let mut generics = HashMap::new();
    let mut arguments = Vec::new();
    let mut return_type = Vec::new();

    //Skip until `fn` ident
    let mut parts = input.into_iter();
    while let Some(part) = parts.next() {
        if let TokenTree::Ident(ident) = part {
            let ident_span = ident.span();
            let ident_name = ident.to_string();
            if ident_name == "async" {
                function_def.push(TokenTree::Ident(ident));
            } else if ident_name == "fn" {
                function_def.push(TokenTree::Ident(ident));
                match parts.next() {
                    Some(TokenTree::Ident(ident)) => {
                        fn_name = ident.to_string();
                        break;
                    },
                    Some(unexpected) => return compile_error(unexpected.span(), "Expected ident"),
                    None => return compile_error(ident_span, "No function name follows"),
                };
            } else {
                return compile_error(ident_span, "Unexpected ident: Should be function definition")
            }
        } else {
            return compile_error(part.span(), "Unexpected component: Should be ident")
        }
    }

    if fn_name.is_empty() {
        compile_error(Span::call_site(), "No function name found, please write valid code");
    }

    ///Parses argument token stream
    macro_rules! arguments_loop {
        ($parts:expr) => {
            let mut parts = $parts;
            'arguments: loop {
                match parts.next() {
                    Some(TokenTree::Ident(argument)) => {
                        match parts.next() {
                            Some(TokenTree::Punct(punct)) => if punct.as_char() == ':' {
                                let mut argument_type = Vec::new();
                                loop {
                                    match parts.next() {
                                        Some(TokenTree::Punct(punct)) => if punct.as_char() == ',' {
                                            arguments.push((argument, argument_type));
                                            continue 'arguments;
                                        } else if punct.as_char() == '&' {
                                            argument_type.push(TokenTree::Punct(punct));

                                            //Detect need for lifetime
                                            match parts.next() {
                                                Some(TokenTree::Punct(punct)) if punct.as_char() == '\'' => {
                                                    argument_type.push(TokenTree::Punct(punct));
                                                },
                                                Some(rest) => {
                                                    has_implicit_lifetime = true;
                                                    let punct = TokenTree::Punct(proc_macro::Punct::new('\'', proc_macro::Spacing::Alone));
                                                    let ident = TokenTree::Ident(proc_macro::Ident::new("implicit_lifetime", rest.span()));
                                                    argument_type.push(punct);
                                                    argument_type.push(ident);
                                                    argument_type.push(rest);
                                                },
                                                None => {
                                                    return compile_error(argument.span(), "Argument has incomplete type")
                                                }
                                            }
                                        } else {
                                            argument_type.push(TokenTree::Punct(punct));
                                        },
                                        Some(rest) => {
                                            argument_type.push(rest);
                                        }
                                        //Last argument (most likely)
                                        None => {
                                            arguments.push((argument, argument_type));
                                            break 'arguments;
                                        }
                                    }
                                }
                            } else {
                                return compile_error(argument.span(), "Expected : after parameter name")
                            },
                            Some(unexpected) => return compile_error(unexpected.span(), "Expected punctuation after parameter name"),
                            None => return compile_error(argument.span(), "Unexpected EOF in argument list"),
                        }
                    },
                    Some(TokenTree::Punct(punct)) if !arguments.is_empty() => if punct.as_char() == ',' {
                        continue 'arguments
                    } else {
                        return compile_error(punct.span(), "Expected parameter name")
                    },
                    Some(unexpected) => return compile_error(unexpected.span(), "Expected parameter name"),
                    None => break 'arguments,
                }
            }
        };
    }

    //Handles generic types and lifetimes
    macro_rules! handle_generics {
        () => {{
            'generics: while let Some(part) = parts.next() {
                match part {
                    TokenTree::Punct(punct) => if punct.as_char() == '>' {
                        break 'generics;
                    } else if punct.as_char() == '\'' {
                        //lifetime definition
                        let lifetime_ident = match parts.next() {
                            Some(TokenTree::Ident(lifetime_ident)) => lifetime_ident,
                            Some(unexpected) => return compile_error(unexpected.span(), "Expected lifetime ident but got nothing"),
                            None => return compile_error(punct.span(), "Expected lifetime ident but got nothing"),
                        };

                        match parts.next() {
                            Some(TokenTree::Punct(punct)) => if punct.as_char() == ',' {
                                if let hash_map::Entry::Vacant(vacant) = lifetimes.entry(format!("'{lifetime_ident}")) {
                                    vacant.insert(Vec::new());
                                }
                                continue 'generics;
                            } else if punct.as_char() == ':' {

                                let mut generics_lifetime_bounds = Vec::new();
                                macro_rules! finalize_lifetime_bounds {
                                    () => {{
                                        match lifetimes.entry(format!("'{lifetime_ident}")) {
                                            hash_map::Entry::Vacant(vacant) => {
                                                vacant.insert(generics_lifetime_bounds);
                                            }
                                            hash_map::Entry::Occupied(ref mut occupied) => {
                                                occupied.get_mut().extend_from_slice(&generics_lifetime_bounds);
                                            }
                                        }
                                    }};
                                }

                                loop {
                                    match parts.next() {
                                        Some(TokenTree::Punct(punct)) => if punct.as_char() == ',' {
                                            finalize_lifetime_bounds!();
                                            continue 'generics;
                                        } else if punct.as_char() == '>' {
                                            finalize_lifetime_bounds!();
                                            break 'generics;
                                        } else {
                                            generics_lifetime_bounds.push(TokenTree::Punct(punct));
                                            continue
                                        },
                                        Some(rest) => {
                                            generics_lifetime_bounds.push(rest);
                                        },
                                        None => {
                                            return compile_error(lifetime_ident.span(), "Unfinished lifetime definition");
                                        }
                                    }
                                }
                            //End of generics
                            } else if punct.as_char() == '>' {
                                if let hash_map::Entry::Vacant(vacant) = lifetimes.entry(format!("'{lifetime_ident}")) {
                                    vacant.insert(Vec::new());
                                }
                                break 'generics;
                            } else {
                                return compile_error(lifetime_ident.span(), &format!("Unexpected punct={} after lifetime", punct));
                            }
                            _ => return compile_error(lifetime_ident.span(), "Expected punctuation after lifetime but got nothing"),
                        }
                    } else {
                        return compile_error(punct.span(), "Unexpected punct: Generics ends with <");
                    }
                    //generic type handling
                    TokenTree::Ident(generic) => {
                        let mut generic_name = generic.to_string();
                        let mut generic_props = if generic_name == "const" {
                            match parts.next() {
                                Some(TokenTree::Ident(actual_generic)) => {
                                    generic_name = actual_generic.to_string();
                                    GenericProps::new_const()
                                }
                                _ => return compile_error(generic.span(), "Invalid const generic")
                            }
                        } else {
                            GenericProps::new()
                        };

                        match parts.next() {
                            Some(TokenTree::Punct(punct)) => {
                                if punct.as_char() == ',' {
                                    if let hash_map::Entry::Vacant(vacant) = generics.entry(generic_name) {
                                        vacant.insert(generic_props);
                                    }

                                    continue 'generics;
                                } else if punct.as_char() == ':' {
                                    let mut trait_bounds = Vec::new();
                                    macro_rules! finalize_generic {
                                        () => {
                                            if trait_bounds.is_empty() {
                                                if let hash_map::Entry::Vacant(vacant) = generics.entry(generic_name) {
                                                    vacant.insert(generic_props);
                                                }
                                            } else {
                                                match generics.entry(generic_name) {
                                                    hash_map::Entry::Vacant(vacant) => {
                                                        generic_props.bounds = trait_bounds;
                                                        vacant.insert(generic_props);
                                                    },
                                                    hash_map::Entry::Occupied(ref mut occupied) => {
                                                        occupied.get_mut().extend_bounds(&mut trait_bounds);
                                                    }
                                                }
                                            }
                                        };
                                    }

                                    //Trait bounds
                                    loop {
                                        match parts.next() {
                                            Some(TokenTree::Punct(punct)) => if punct.as_char() == ',' {
                                                finalize_generic!();
                                                continue 'generics;
                                            } else if punct.as_char() == '>' {
                                                finalize_generic!();
                                                break 'generics;
                                            } else {
                                                trait_bounds.push(TokenTree::Punct(punct));
                                            },
                                                Some(token) => {
                                                    trait_bounds.push(token);
                                                }
                                            None => return compile_error(punct.span(), "Unexpected EOF during trait bounds"),
                                        }
                                    }
                                } else if punct.as_char() == '>' {
                                    if let hash_map::Entry::Vacant(vacant) = generics.entry(generic_name) {
                                        vacant.insert(generic_props);
                                    }
                                    break 'generics;
                                } else {
                                    return compile_error(punct.span(), "Unexpected token after generic declaration")
                                }
                            },
                            Some(unexpected) => return compile_error(unexpected.span(), "Unexpected token after generic declaration"),
                            _ => return compile_error(generic.span(), "Unexpected EOF after generic declaration"),
                        }
                    },
                    unexpected => return compile_error(unexpected.span(), "Unexpected token in generic definition"),
                }
            }
        }}
    }

    ///Handles token blocks either arguments or code
    macro_rules! handle_group {
        ($label:tt <= $group:expr) => {{
            let group = $group;
            match group.delimiter() {
                //code block
                Delimiter::Brace => {
                    if code_block.is_some() {
                        return compile_error(group.span(), "Second code block")
                    }
                    code_block = Some(group.stream());
                    break $label;
                },
                //arguments
                Delimiter::Parenthesis => {
                    arguments_loop!(group.stream().into_iter());
                    continue $label;
                },
                _ => {
                    return compile_error(group.span(), "Unexpected block")
                }
            }
        }};
    }

    //Parses where clause generics
    //
    //Aggregates generics together with the one defined in bounds
    macro_rules! handle_where_clause {
        ($where:ident) => {
            'where_clause: loop {
                match parts.next() {
                    Some(TokenTree::Group(group)) => {
                        handle_group!('where_clause <= group);
                    },
                    //lifetimes
                    Some(TokenTree::Punct(punct)) => if punct.as_char() == '\'' {
                        //lifetime definition
                        let lifetime_ident = match parts.next() {
                            Some(TokenTree::Ident(lifetime_ident)) => lifetime_ident,
                            Some(unexpected) => return compile_error(unexpected.span(), "Expected lifetime ident but got nothing"),
                            None => return compile_error(punct.span(), "Expected lifetime ident but got nothing"),
                        };

                        match parts.next() {
                            Some(TokenTree::Punct(punct)) => if punct.as_char() == ',' {
                                if let hash_map::Entry::Vacant(vacant) = lifetimes.entry(lifetime_ident.to_string()) {
                                    vacant.insert(Vec::new());
                                }
                                continue 'where_clause;
                            } else if punct.as_char() == ':' {

                                let mut generics_lifetime_bounds = Vec::new();
                                macro_rules! finalize_lifetime_bounds {
                                    () => {{
                                        match lifetimes.entry(lifetime_ident.to_string()) {
                                            hash_map::Entry::Vacant(vacant) => {
                                                vacant.insert(generics_lifetime_bounds);
                                            }
                                            hash_map::Entry::Occupied(ref mut occupied) => {
                                                occupied.get_mut().extend_from_slice(&generics_lifetime_bounds);
                                            }
                                        }
                                    }};
                                }

                                loop {
                                    match parts.next() {
                                        Some(TokenTree::Punct(punct)) => if punct.as_char() == ',' {
                                            finalize_lifetime_bounds!();
                                            continue 'where_clause;
                                        } else {
                                            generics_lifetime_bounds.push(TokenTree::Punct(punct));
                                            continue
                                        },
                                        Some(rest) => {
                                            generics_lifetime_bounds.push(rest);
                                        },
                                        None => {
                                            return compile_error(lifetime_ident.span(), "Unfinished lifetime definition");
                                        }
                                    }
                                }
                            } else {
                                return compile_error(lifetime_ident.span(), "Unexpected punct after lifetime");
                            }
                            _ => return compile_error(lifetime_ident.span(), "Expected punctuation after lifetime but got nothing"),
                        }
                    } else {
                        return compile_error(punct.span(), "Unexpected token in where clause")
                    },
                    //generic type handling
                    Some(TokenTree::Ident(generic)) => match parts.next() {
                        Some(TokenTree::Punct(punct)) => {
                            if punct.as_char() == ',' {
                                if let hash_map::Entry::Vacant(vacant) = generics.entry(generic.to_string()) {
                                    vacant.insert(GenericProps::new());
                                }

                                continue 'where_clause;
                            } else if punct.as_char() == ':' {
                                let mut trait_bounds = Vec::new();
                                macro_rules! finalize_generic {
                                    () => {
                                        if trait_bounds.is_empty() {
                                            if let hash_map::Entry::Vacant(vacant) = generics.entry(generic.to_string()) {
                                                vacant.insert(GenericProps::new());
                                            }
                                        } else {
                                            match generics.entry(generic.to_string()) {
                                                hash_map::Entry::Vacant(vacant) => {
                                                    vacant.insert(GenericProps::with_bounds(trait_bounds));
                                                },
                                                hash_map::Entry::Occupied(ref mut occupied) => {
                                                    occupied.get_mut().extend_bounds(&mut trait_bounds);
                                                }
                                            }
                                        }
                                    };
                                }

                                //Trait bounds
                                loop {
                                    match parts.next() {
                                        Some(TokenTree::Punct(punct)) => if punct.as_char() == ',' {
                                            finalize_generic!();
                                            continue 'where_clause;
                                        } else {
                                            trait_bounds.push(TokenTree::Punct(punct));
                                        },
                                        Some(TokenTree::Group(group)) => {
                                            handle_group!('where_clause <= group);
                                        },
                                        Some(token) => {
                                            trait_bounds.push(token);
                                        }
                                        None => return compile_error(punct.span(), "Unexpected EOF during where's trait bounds"),
                                    }
                                }
                            } else {
                                return compile_error(punct.span(), "Unexpected token after generic definition")
                            }
                        },
                        Some(unexpected) => return compile_error(unexpected.span(), "Unexpected token after generic declaration"),
                        _ => return compile_error(generic.span(), "Unexpected EOF after generic declaration"),
                    },
                    Some(unexpected) => return compile_error(unexpected.span(), "Unexpected token in generic definition"),
                    None => return compile_error($where.span(), "Unexpected EOF in where block"),
                }
            }
        };
    }

    'main_loop: while let Some(part) = parts.next() {
        match part {
            TokenTree::Group(group) => handle_group!('main_loop <= group),
            TokenTree::Punct(punct) => {
                if punct.as_char() == '<' {
                    handle_generics!();
                }
                else if punct.as_char() == '-' {
                    //return type
                    return_type.push(TokenTree::Punct(punct.clone()));

                    match parts.next() {
                        Some(TokenTree::Punct(punct)) if punct.as_char() == '>' => {
                            return_type.push(TokenTree::Punct(punct.clone()));

                            'return_type: loop {
                                match parts.next() {
                                    Some(TokenTree::Ident(ident)) => {
                                        let ident_name = ident.to_string();
                                        if ident_name != "where" {
                                            return_type.push(TokenTree::Ident(ident));
                                            continue 'return_type
                                        } else {
                                            handle_where_clause!(ident);
                                            continue 'main_loop;
                                        }
                                    },
                                    Some(TokenTree::Group(group)) => handle_group!('main_loop <= group),
                                    Some(rest) => return_type.push(rest),
                                    None => return compile_error(punct.span(), "Unexpected EOF in return type")
                                }
                            }
                        }
                        Some(unexpected) => return compile_error(unexpected.span(), "Expected > but got"),
                        None => return compile_error(punct.span(), "Expected > but got EOF")
                    }
                } //return type
                else {
                    return compile_error(punct.span(), "Unexpected punct: Generics start with <");
                }
            },
            unexpected => return compile_error(unexpected.span(), "Unexpected part: Should be group or punctuation"),
        }
    }

    let code_block = match code_block {
        Some(code_block) => code_block,
        None => return compile_error(Span::call_site(), "Code block is missing. Did you apply macro to the function"),
    };

    if has_implicit_lifetime {
        lifetimes.insert("'implicit_lifetime".to_owned(), vec![]);
    }

    let mut output = String::new();
    //Declare struct
    let _ = write!(&mut output, "pub struct {fn_name}");
    if !lifetimes.is_empty() {
        output.push('<');
        for (generic, definition) in lifetimes.iter() {
            let _ = if definition.is_empty() {
                write!(&mut output, "{generic},")
            } else {
                write!(&mut output, "{generic}:{},", ArgumentFmt(definition))
            };
        }
    }

    if !generics.is_empty() {
        if lifetimes.is_empty() {
            output.push('<');
        }

        for (generic, props) in generics.iter() {
            let generic = GenericType {
                ident: &generic,
                props,
            };
            let _ = write!(&mut output, "{generic},");
        }

        output.pop();
        output.push('>');
    } else if !lifetimes.is_empty() {
        output.pop();
        output.push('>');
    }

    if !arguments.is_empty() {
        output.push(' ');
        output.push('{');
        output.push('\n');
        for (argument, typ) in arguments.iter() {
            let _ = write!(&mut output, "{TAB}{argument}:{},\n", ArgumentFmt(&typ));
        }
        output.push('}');
    } else {
        output.push(';');
    }

    output.push('\n');
    //Impl call()
    output.push_str("impl");

    if !lifetimes.is_empty() {
        output.push('<');
        for (generic, definition) in lifetimes.iter() {
            let _ = if definition.is_empty() {
                write!(&mut output, "{generic},")
            } else {
                write!(&mut output, "{generic}:{},", ArgumentFmt(definition))
            };
        }
    }

    if !generics.is_empty() {
        if lifetimes.is_empty() {
            output.push('<');
        }

        for (generic, props) in generics.iter() {
            let generic = GenericType {
                ident: &generic,
                props,
            };
            let _ = write!(&mut output, "{generic},");
        }

        output.pop();
        output.push('>');
    } else if !lifetimes.is_empty() {
        output.push('>');
    }

    output.push(' ');
    output.push_str(&fn_name);

    if !lifetimes.is_empty() {
        output.push('<');
        for (generic, _) in lifetimes.iter() {
            let _ = write!(&mut output, "{generic},");
        }
    }

    if !generics.is_empty() {
        if lifetimes.is_empty() {
            output.push('<');
        }

        for (generic, _) in generics.iter() {
            let _ = write!(&mut output, "{generic},");
        }

        output.pop();
        output.push('>');
    } else if !lifetimes.is_empty() {
        output.push('>');
    }

    output.push(' ');
    output.push('{');
    output.push('\n');

    output.push_str(TAB);
    //Define function call with original tokens
    for token in function_def.iter() {
        let _ = write!(&mut output, "{token} ");
    }
    output.push_str("call");
    output.push('(');
    output.push_str("self");
    output.push(')');

    output.push(' ');

    //Add explicit return type if any
    if !return_type.is_empty() {
        let mut tokens = return_type.iter();
        while let Some(token) = tokens.next() {
            if let TokenTree::Punct(punct) = token {
                output.push(punct.as_char());
                if punct.as_char() == '\'' {
                    if let Some(token) = tokens.next() {
                        let _ = write!(&mut output, "{token} ");
                    }
                }
            } else {
                let _ = write!(&mut output, "{token} ");
            }
        }
    }

    //Generate function call
    output.push('{');
    output.push('\n');

    //Expand arguments
    if !arguments.is_empty() {
        output.push_str(TAB);
        output.push_str(TAB);
        output.push_str("let Self { ");

        for (argument, _) in arguments.iter() {
            let _ = write!(&mut output, "{argument},");
        }

        output.push_str(" } = self;");
        output.push('\n');
    }

    //Code block
    let _ = write!(&mut output, "{TAB}{TAB}const __func__: &str = \"{fn_name}\";\n").expect("Cannot format");
    let _ = write!(&mut output, "{}\n", code_block).expect("Cannot format");

    output.push('\n');
    output.push_str(TAB);
    output.push('}');
    output.push('\n');
    output.push('}');

    output.parse().expect("generate correct code")
}
