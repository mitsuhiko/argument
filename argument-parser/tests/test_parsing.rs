use std::ffi::OsString;
use std::net::{IpAddr, Ipv4Addr};

use argument_parser::{Error, Flag, Param, Parser};

fn make_invalid_unicode_os_string() -> OsString {
    #[cfg(unix)]
    {
        use std::os::unix::ffi::OsStringExt;
        OsString::from_vec(vec![0xff, 0xff])
    }
    #[cfg(windows)]
    {
        use std::os::windows::ffi::OsStringExt;
        OsString::from_wide(&[0xD800]) // Invalid UTF-16 surrogate
    }
}

#[test]
fn test_basic() -> Result<(), Error> {
    let this = std::env::current_exe().unwrap();
    let mut parser = Parser::from_cmdline([this.to_str().unwrap(), "-n23", "-n42"].into_iter());
    let mut num = 0;

    assert!(!parser.finished());
    while let Some(param) = parser.param()? {
        assert!(!parser.finished());
        if param.is_short('n') {
            num = parser.value()?;
        } else {
            return Err(parser.unexpected());
        }
    }
    assert!(parser.finished());
    assert_eq!(
        parser.prog(),
        this.file_name()
            .and_then(|x| x.to_str())
            .unwrap_or_default()
    );
    assert_eq!(parser.raw_prog(), Some(this.as_os_str()));
    assert_eq!(num, 42);

    Ok(())
}

#[test]
fn test_default_flags() {
    let parser = Parser::from_args(["test"].into_iter());

    // Default enabled flags
    assert!(parser.get_flag(Flag::OptionsEnabled));
    assert!(parser.get_flag(Flag::HandleDoubleDash));

    // Default disabled flags
    assert!(!parser.get_flag(Flag::DisableNumericOptions));
    assert!(!parser.get_flag(Flag::DisableOptionsAfterArgs));
    assert!(!parser.get_flag(Flag::StripShortOptionEqualSign));
}

#[test]
fn test_value_parsing() -> Result<(), Error> {
    let mut parser = Parser::from_args(["a-string", "42", "true", "127.0.0.1"].into_iter());

    assert_eq!(parser.value::<String>()?, "a-string");
    assert_eq!(parser.value::<i32>()?, 42);
    assert!(parser.value::<bool>()?);
    assert_eq!(parser.value::<IpAddr>()?, IpAddr::V4(Ipv4Addr::LOCALHOST));
    assert!(parser.finished());

    Ok(())
}

#[test]
fn test_handle_double_dash() -> Result<(), Error> {
    let mut parser = Parser::from_args(["-n23", "--", "-n42", "meh"].into_iter());
    assert!(parser.get_flag(Flag::HandleDoubleDash));
    let mut num = 0;
    let mut args = Vec::<String>::new();
    while let Some(param) = parser.param()? {
        if param.is_short('n') {
            num = parser.value()?;
        } else if param.is_pos() {
            args.push(parser.value()?);
        } else {
            return Err(parser.unexpected());
        }
    }
    assert_eq!(num, 23);
    assert_eq!(args, vec!["-n42", "meh"]);

    let mut parser = Parser::from_args(["-n23", "--", "-n42", "meh"].into_iter());
    parser.set_flag(Flag::HandleDoubleDash, false);
    assert!(!parser.get_flag(Flag::HandleDoubleDash));
    let mut num = 0;
    let mut args = Vec::<String>::new();
    while let Some(param) = parser.param()? {
        if param.is_short('n') {
            num = parser.value()?;
        } else if param.is_pos() {
            args.push(parser.value()?);
        } else {
            return Err(parser.unexpected());
        }
    }
    assert_eq!(num, 42);
    assert_eq!(args, vec!["--", "meh"]);

    Ok(())
}

#[test]
fn test_arguments_disable_options() -> Result<(), Error> {
    let mut parser = Parser::from_args(["-n23", "foo", "-n42"].into_iter());
    assert!(!parser.get_flag(Flag::DisableOptionsAfterArgs));
    let mut num = 0;
    let mut args = Vec::<String>::new();
    while let Some(param) = parser.param()? {
        if param.is_short('n') {
            num = parser.value()?;
        } else if param.is_pos() {
            args.push(parser.value()?);
        } else {
            return Err(parser.unexpected());
        }
    }
    assert_eq!(num, 42);
    assert_eq!(args, vec!["foo"]);

    let mut parser = Parser::from_args(["-n23", "foo", "-n42"].into_iter());
    parser.set_flag(Flag::DisableOptionsAfterArgs, true);
    assert!(parser.get_flag(Flag::DisableOptionsAfterArgs));
    let mut num = 0;
    let mut args = Vec::<String>::new();
    while let Some(param) = parser.param()? {
        if param.is_short('n') {
            num = parser.value()?;
        } else if param.is_pos() {
            args.push(parser.value()?);
        } else {
            return Err(parser.unexpected());
        }
    }
    assert_eq!(num, 23);
    assert_eq!(args, vec!["foo", "-n42"]);
    Ok(())
}

#[test]
fn test_disallow_numeric_options() -> Result<(), Error> {
    let mut parser =
        Parser::from_args(["-n4", "1", "2", "3", "-", "-1", "-2", "-3", "x"].into_iter());
    assert!(!parser.get_flag(Flag::DisableNumericOptions));
    let mut num = 0;
    let mut known_numeric_shorts = 0;
    let mut args = Vec::<String>::new();
    while let Some(param) = parser.param()? {
        if param.is_short('n') {
            num = parser.value()?;
        } else if param.is_short('1') || param.is_short('2') || param.is_short('3') {
            known_numeric_shorts += 1;
        } else if param.is_pos() {
            args.push(parser.value()?);
        } else {
            return Err(parser.unexpected());
        }
    }
    assert_eq!(num, 4);
    assert_eq!(known_numeric_shorts, 3);
    assert_eq!(args, vec!["1", "2", "3", "-", "x"]);

    let mut parser =
        Parser::from_args(["-n4", "1", "2", "3", "-", "-1", "-2", "-3", "x"].into_iter());
    parser.set_flag(Flag::DisableNumericOptions, true);
    assert!(parser.get_flag(Flag::DisableNumericOptions));
    let mut num = 0;
    let mut known_numeric_shorts = 0;
    let mut args = Vec::<String>::new();
    while let Some(param) = parser.param()? {
        if param.is_short('n') {
            num = parser.value()?;
        } else if param.is_short('1') || param.is_short('2') || param.is_short('3') {
            known_numeric_shorts += 1;
        } else if param.is_pos() {
            args.push(parser.value()?);
        } else {
            return Err(parser.unexpected());
        }
    }
    assert_eq!(num, 4);
    assert_eq!(known_numeric_shorts, 0);
    assert_eq!(args, vec!["1", "2", "3", "-", "-1", "-2", "-3", "x"]);
    Ok(())
}

#[test]
fn test_looks_at_value() -> Result<(), Error> {
    fn parse_args(
        args: &[&str],
        no_num_opts: bool,
    ) -> Result<(usize, Vec<String>, Vec<String>), Error> {
        let mut parser = Parser::from_args(args.iter());
        if no_num_opts {
            parser.set_flag(Flag::DisableNumericOptions, true);
        }
        let mut x = 0;
        let mut messages = Vec::<String>::new();
        let mut extra = Vec::<String>::new();
        while let Some(param) = parser.param()? {
            if param.is_short('m') || param.is_long("message") {
                while parser.looks_at_value() {
                    messages.push(parser.value()?);
                }
            } else if param.is_short('x') || param.is_short('1') {
                x += 1;
            } else if param.is_pos() {
                extra.push(parser.value()?);
            } else {
                return Err(parser.unexpected());
            }
        }
        Ok((x, messages, extra))
    }

    let (x, messages, extra) = parse_args(&["-x", "-m", "1", "2"], false)?;
    assert_eq!(x, 1);
    assert_eq!(messages, vec!["1", "2"]);
    assert!(extra.is_empty());

    let (x, messages, extra) = parse_args(&["-x", "-m1", "2"], false)?;
    assert_eq!(x, 1);
    assert_eq!(messages, vec!["1", "2"]);
    assert!(extra.is_empty());

    let (x, messages, extra) = parse_args(&["-x", "--message", "1", "2"], false)?;
    assert_eq!(x, 1);
    assert_eq!(messages, vec!["1", "2"]);
    assert!(extra.is_empty());

    let (x, messages, extra) = parse_args(&["-x", "--message=1", "2"], false)?;
    assert_eq!(x, 1);
    assert_eq!(messages, vec!["1", "2"]);
    assert!(extra.is_empty());

    let (x, messages, extra) = parse_args(&["-m", "-x", "1", "2"], false)?;
    assert_eq!(x, 1);
    assert!(messages.is_empty());
    assert_eq!(extra, vec!["1", "2"]);

    let (x, messages, extra) = parse_args(&["-m-x", "1", "2"], false)?;
    assert_eq!(x, 0);
    assert_eq!(messages, vec!["-x", "1", "2"]);
    assert!(extra.is_empty());

    let (x, messages, extra) = parse_args(&["-m", "1", "-1", "a"], false)?;
    assert_eq!(x, 1);
    assert_eq!(messages, vec!["1"]);
    assert_eq!(extra, vec!["a"]);

    let (x, messages, extra) = parse_args(&["-m", "1", "-1", "a"], true)?;
    assert_eq!(x, 0);
    assert_eq!(messages, vec!["1", "-1", "a"]);
    assert!(extra.is_empty());

    Ok(())
}

#[test]
fn test_strip_short_option_equal_sign() -> Result<(), Error> {
    let mut parser = Parser::from_args(["-o=42"].into_iter());
    parser.set_flag(Flag::StripShortOptionEqualSign, true);
    assert!(matches!(parser.param()?, Some(Param::Short('o'))));
    assert_eq!(parser.value::<i32>()?, 42);
    assert!(parser.finished());

    let mut parser = Parser::from_args(["-o=42"].into_iter());
    parser.set_flag(Flag::StripShortOptionEqualSign, false);
    assert!(matches!(parser.param()?, Some(Param::Short('o'))));
    assert_eq!(parser.value::<String>()?, "=42");
    assert!(parser.finished());
    Ok(())
}

#[test]
fn test_parse_two_values_for_option() -> Result<(), Error> {
    let mut parser = Parser::from_args("-p -1 -1 -p -1 1 -p 1 -1 -p 1 1".split_ascii_whitespace());
    let mut points = Vec::<(i32, i32)>::new();

    while let Some(param) = parser.param()? {
        if param.is_short('p') {
            let x = parser.value()?;
            let y = parser.value()?;
            points.push((x, y));
        } else {
            return Err(parser.unexpected());
        }
    }

    assert_eq!(points, vec![(-1, -1), (-1, 1), (1, -1), (1, 1)]);
    assert!(parser.finished());
    Ok(())
}

#[test]
fn test_weird_args() -> Result<(), Error> {
    let mut p = Parser::from_args(
        [
            "", "---=---", "---", "---", "--=", "--=3", "-", "-x", "--", "-", "-x", "--", "", "-",
            "-x", "---",
        ]
        .into_iter(),
    );

    assert_eq!(p.param()?, Some(Param::Pos));
    assert_eq!(p.value::<String>()?, "");
    assert_eq!(p.param()?, Some(Param::Long("-".to_string())));
    assert_eq!(p.value::<String>()?, "---");
    assert_eq!(p.param()?, Some(Param::Long("-".to_string())));
    assert_eq!(p.value::<String>()?, "---");
    assert_eq!(p.param()?, Some(Param::Long("".to_string())));
    assert_eq!(p.value::<String>()?, "");
    assert_eq!(p.param()?, Some(Param::Long("".to_string())));
    assert_eq!(p.value::<String>()?, "3");
    assert_eq!(p.param()?, Some(Param::Pos));
    assert_eq!(p.value::<String>()?, "-");
    assert_eq!(p.param()?, Some(Param::Short('x')));
    assert_eq!(p.value::<String>()?, "--");
    assert_eq!(p.param()?, Some(Param::Pos));
    assert_eq!(p.value::<String>()?, "-");
    assert_eq!(p.param()?, Some(Param::Short('x')));
    assert_eq!(p.param()?, Some(Param::Pos));
    assert_eq!(p.value::<String>()?, "");
    assert_eq!(p.param()?, Some(Param::Pos));
    assert_eq!(p.value::<String>()?, "-");
    assert_eq!(p.param()?, Some(Param::Pos));
    assert_eq!(p.value::<String>()?, "-x");
    assert_eq!(p.param()?, Some(Param::Pos));
    assert_eq!(p.value::<String>()?, "---");
    assert_eq!(p.param()?, None);

    Ok(())
}

#[test]
fn test_invalid_unicode() -> Result<(), Error> {
    // Create an OsString with invalid unicode bytes
    let invalid_unicode = make_invalid_unicode_os_string();

    // Test invalid unicode in positional argument
    let mut parser = Parser::from_args([invalid_unicode.clone()].into_iter());
    assert_eq!(parser.param()?, Some(Param::Pos));
    assert!(parser.value::<String>().is_err()); // Should fail string parsing
    let mut parser = Parser::from_args([invalid_unicode.clone()].into_iter());
    assert_eq!(parser.param()?, Some(Param::Pos));
    assert_eq!(parser.raw_value()?, invalid_unicode); // But raw value works

    // Test invalid unicode in short option value
    let mut parser = Parser::from_args(["-x".into(), invalid_unicode.clone()].into_iter());
    assert_eq!(parser.param()?, Some(Param::Short('x')));
    assert!(parser.value::<String>().is_err()); // Should fail string parsing
    let mut parser = Parser::from_args(["-x".into(), invalid_unicode.clone()].into_iter());
    assert_eq!(parser.param()?, Some(Param::Short('x')));
    assert_eq!(parser.raw_value()?, invalid_unicode); // But raw value works

    // Test invalid unicode in short option value with direct concatenation
    let mut arg = OsString::from("-x");
    arg.push(&invalid_unicode);
    let mut parser = Parser::from_args([arg.clone()].into_iter());
    assert_eq!(parser.param()?, Some(Param::Short('x')));
    assert!(parser.value::<String>().is_err()); // Should fail string parsing
    let mut parser = Parser::from_args([arg].into_iter());
    assert_eq!(parser.param()?, Some(Param::Short('x')));
    assert_eq!(parser.raw_value()?, invalid_unicode); // But raw value works

    // Test invalid unicode in long option value
    let mut parser = Parser::from_args(["--foo".into(), invalid_unicode.clone()].into_iter());
    assert_eq!(parser.param()?, Some(Param::Long("foo".into())));
    assert!(parser.value::<String>().is_err()); // Should fail string parsing
    let mut parser = Parser::from_args(["--foo".into(), invalid_unicode.clone()].into_iter());
    assert_eq!(parser.param()?, Some(Param::Long("foo".into())));
    assert_eq!(parser.raw_value()?, invalid_unicode); // But raw value works

    // Test invalid unicode in long option value with equals
    let mut arg = OsString::from("--foo=");
    arg.push(&invalid_unicode);
    let mut parser = Parser::from_args([arg.clone()].into_iter());
    assert_eq!(parser.param()?, Some(Param::Long("foo".into())));
    assert!(parser.value::<String>().is_err()); // Should fail string parsing
    let mut parser = Parser::from_args([arg].into_iter());
    assert_eq!(parser.param()?, Some(Param::Long("foo".into())));
    assert_eq!(parser.raw_value()?, invalid_unicode); // But raw value works

    // Test invalid unicode in option name - should error
    let mut invalid_opt = OsString::from("-");
    invalid_opt.push(make_invalid_unicode_os_string());
    let mut parser = Parser::from_args([invalid_opt].into_iter());
    assert!(parser.param().is_err());

    Ok(())
}

#[test]
fn test_raw_arg_options_handling() -> Result<(), Error> {
    // Parse -- as raw arg (doesn't disable options)
    let mut parser = Parser::from_args(["foo", "bar", "--", "-x"].into_iter());
    assert_eq!(parser.raw_arg(), Some("foo".into()));
    assert_eq!(parser.value::<String>()?, "bar");
    assert_eq!(parser.raw_arg(), Some("--".into()));
    assert_eq!(parser.param()?, Some(Param::Short('x')));
    assert!(parser.finished());

    // Parse -- as string (disables options)
    let mut parser = Parser::from_args(["foo", "bar", "--", "-x"].into_iter());
    assert_eq!(parser.raw_arg(), Some("foo".into()));
    assert_eq!(parser.value::<String>()?, "bar");
    assert_eq!(parser.param()?, Some(Param::Pos));
    assert_eq!(parser.value::<String>()?, "-x");
    assert!(parser.finished());

    Ok(())
}

#[test]
fn test_raw_arg_options_peeking() -> Result<(), Error> {
    let mut parser = Parser::from_args(["foo", "-xbar", "blah"].into_iter());
    assert_eq!(parser.param()?, Some(Param::Pos));
    assert_eq!(parser.peek_raw_arg().and_then(|x| x.to_str()), Some("foo"));
    assert_eq!(parser.value::<String>()?, "foo");
    assert_eq!(parser.param()?, Some(Param::Short('x')));
    assert!(parser.peek_raw_arg().is_none());
    assert_eq!(parser.value::<String>()?, "bar");
    assert_eq!(parser.peek_raw_arg().and_then(|x| x.to_str()), Some("blah"));
    assert_eq!(parser.value::<String>()?, "blah");
    Ok(())
}

#[test]
fn test_skipped_arg_value() -> Result<(), Error> {
    let mut parser = Parser::from_args(["first", "second", "--third=value", "-xl"].into_iter());

    assert_eq!(parser.param()?, Some(Param::Pos));
    // first is intentionally lost

    assert_eq!(parser.param()?, Some(Param::Pos));
    assert_eq!(parser.value::<String>()?, "second");
    assert_eq!(parser.param()?, Some(Param::Long("third".into())));
    // value is intentionally lost

    assert_eq!(parser.param()?, Some(Param::Short('x')));
    assert_eq!(parser.param()?, Some(Param::Short('l')));
    assert_eq!(parser.param()?, None);

    assert!(parser.finished());

    Ok(())
}

#[test]
fn test_optional_vs_regular_value() -> Result<(), Error> {
    // Optional values only parse values directly attached to options
    let mut parser = Parser::from_args(["-n42", "-m", "23"].into_iter());

    assert_eq!(parser.param()?, Some(Param::Short('n')));
    assert_eq!(parser.optional_value::<i32>()?, Some(42)); // Parses "42" from "-n42"

    assert_eq!(parser.param()?, Some(Param::Short('m')));
    assert_eq!(parser.optional_value::<i32>()?, None); // No value in "-m"
    assert_eq!(parser.value::<i32>()?, 23); // Parses next arg "23"

    // Long options with = are also optional values
    let mut parser = Parser::from_args(["--num=42", "--val", "23"].into_iter());

    assert_eq!(parser.param()?, Some(Param::Long("num".into())));
    assert_eq!(parser.optional_value::<i32>()?, Some(42)); // Parses "42" from "--num=42"

    assert_eq!(parser.param()?, Some(Param::Long("val".into())));
    assert_eq!(parser.optional_value::<i32>()?, None); // No value in "--val"
    assert_eq!(parser.value::<i32>()?, 23); // Parses next arg "23"

    Ok(())
}

#[test]
fn test_value_parsing_edge_cases() -> Result<(), Error> {
    let mut parser =
        Parser::from_args(["0", "-0", "true", "false", "127.0.0.1", "::1"].into_iter());

    assert_eq!(parser.value::<i32>()?, 0);
    assert_eq!(parser.value::<i32>()?, 0);
    assert!(parser.value::<bool>()?);
    assert!(!parser.value::<bool>()?);
    assert!(matches!(parser.value::<IpAddr>()?, IpAddr::V4(_)));
    assert!(matches!(parser.value::<IpAddr>()?, IpAddr::V6(_)));

    Ok(())
}

#[test]
fn test_empty_args() -> Result<(), Error> {
    let parser = Parser::from_args(Vec::<String>::new().into_iter());
    assert!(parser.finished());
    Ok(())
}

#[test]
fn test_multiple_long_options() -> Result<(), Error> {
    let mut parser = Parser::from_args(["--foo=bar", "--baz", "qux", "--flag"].into_iter());

    assert_eq!(parser.param()?, Some(Param::Long("foo".into())));
    assert_eq!(parser.value::<String>()?, "bar");

    assert_eq!(parser.param()?, Some(Param::Long("baz".into())));
    assert_eq!(parser.value::<String>()?, "qux");

    assert_eq!(parser.param()?, Some(Param::Long("flag".into())));
    assert!(parser.finished());
    Ok(())
}

#[test]
fn test_combined_short_options() -> Result<(), Error> {
    let mut parser = Parser::from_args(["-abc", "value"].into_iter());

    assert_eq!(parser.param()?, Some(Param::Short('a')));
    assert_eq!(parser.param()?, Some(Param::Short('b')));
    assert_eq!(parser.param()?, Some(Param::Short('c')));
    assert_eq!(parser.value::<String>()?, "value");

    Ok(())
}

#[test]
fn test_mixed_value_types() -> Result<(), Error> {
    let mut parser = Parser::from_args(
        [
            "--int=42", "--float", "3.17", "--str", "hello", "--bool", "true",
        ]
        .into_iter(),
    );

    assert_eq!(parser.param()?, Some(Param::Long("int".into())));
    assert_eq!(parser.value::<i32>()?, 42);

    assert_eq!(parser.param()?, Some(Param::Long("float".into())));
    assert!((parser.value::<f64>()? - 3.17).abs() < f64::EPSILON);

    assert_eq!(parser.param()?, Some(Param::Long("str".into())));
    assert_eq!(parser.value::<String>()?, "hello");

    assert_eq!(parser.param()?, Some(Param::Long("bool".into())));
    assert!(parser.value::<bool>()?);
    Ok(())
}

#[test]
fn test_malformed_options() -> Result<(), Error> {
    let mut parser = Parser::from_args(["-", "--=", "---", "----"].into_iter());

    assert_eq!(parser.param()?, Some(Param::Pos));
    assert_eq!(parser.value::<String>()?, "-");
    assert_eq!(parser.param()?, Some(Param::Long("".into())));
    assert_eq!(parser.value::<String>()?, "");
    assert_eq!(parser.param()?, Some(Param::Long("-".into())));
    assert_eq!(parser.value::<String>()?, "----");

    Ok(())
}

#[test]
fn test_peek_multiple() -> Result<(), Error> {
    let mut parser = Parser::from_args(["first", "second", "third"].into_iter());

    assert_eq!(
        parser.peek_raw_arg().and_then(|x| x.to_str()),
        Some("first")
    );
    assert_eq!(
        parser.peek_raw_arg().and_then(|x| x.to_str()),
        Some("first")
    ); // Multiple peeks should return same value
    assert_eq!(parser.value::<String>()?, "first");

    assert_eq!(
        parser.peek_raw_arg().and_then(|x| x.to_str()),
        Some("second")
    );
    assert_eq!(parser.value::<String>()?, "second");

    assert_eq!(
        parser.peek_raw_arg().and_then(|x| x.to_str()),
        Some("third")
    );
    assert_eq!(parser.value::<String>()?, "third");

    assert!(parser.peek_raw_arg().is_none());
    Ok(())
}

#[test]
fn test_combined_short_options_with_values() -> Result<(), Error> {
    let mut parser = Parser::from_args(["-abcvalue"].into_iter());
    assert_eq!(parser.param()?, Some(Param::Short('a')));
    assert_eq!(parser.param()?, Some(Param::Short('b')));
    assert_eq!(parser.param()?, Some(Param::Short('c')));
    assert_eq!(parser.value::<String>()?, "value");
    Ok(())
}

#[test]
fn test_large_number_handling() -> Result<(), Error> {
    let mut parser = Parser::from_args(["--large-int", "18446744073709551615"].into_iter());
    assert_eq!(parser.param()?, Some(Param::Long("large-int".into())));
    assert_eq!(parser.value::<u64>()?, 18446744073709551615);
    Ok(())
}

#[test]
fn test_platform_specific_behavior() -> Result<(), Error> {
    let invalid_unicode = make_invalid_unicode_os_string();
    let mut parser = Parser::from_args([invalid_unicode.clone()].into_iter());
    assert_eq!(parser.param()?, Some(Param::Pos));
    assert!(parser.value::<String>().is_err()); // Should fail string parsing

    let mut parser = Parser::from_args([invalid_unicode.clone()].into_iter());
    assert_eq!(parser.param()?, Some(Param::Pos));
    assert_eq!(parser.raw_value()?, invalid_unicode); // But raw value works
    Ok(())
}
