use std::ffi::OsString;
use std::net::{IpAddr, Ipv4Addr};
use std::os::unix::ffi::OsStringExt;

use argument_parser::{Error, Flag, Param, Parser};

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
            return Err(param.into_unexpected_error());
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
    let mut args = Vec::new();
    while let Some(param) = parser.param()? {
        if param.is_short('n') {
            num = parser.value()?;
        } else if param.is_arg() {
            args.push(parser.string_value()?);
        } else {
            return Err(param.into_unexpected_error());
        }
    }
    assert_eq!(num, 23);
    assert_eq!(args, vec!["-n42", "meh"]);

    let mut parser = Parser::from_args(["-n23", "--", "-n42", "meh"].into_iter());
    parser.set_flag(Flag::HandleDoubleDash, false);
    assert!(!parser.get_flag(Flag::HandleDoubleDash));
    let mut num = 0;
    let mut args = Vec::new();
    while let Some(param) = parser.param()? {
        if param.is_short('n') {
            num = parser.value()?;
        } else if param.is_arg() {
            args.push(parser.string_value()?);
        } else {
            return Err(param.into_unexpected_error());
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
    let mut args = Vec::new();
    while let Some(param) = parser.param()? {
        if param.is_short('n') {
            num = parser.value()?;
        } else if param.is_arg() {
            args.push(parser.string_value()?);
        } else {
            return Err(param.into_unexpected_error());
        }
    }
    assert_eq!(num, 42);
    assert_eq!(args, vec!["foo"]);

    let mut parser = Parser::from_args(["-n23", "foo", "-n42"].into_iter());
    parser.set_flag(Flag::DisableOptionsAfterArgs, true);
    assert!(parser.get_flag(Flag::DisableOptionsAfterArgs));
    let mut num = 0;
    let mut args = Vec::new();
    while let Some(param) = parser.param()? {
        if param.is_short('n') {
            num = parser.value()?;
        } else if param.is_arg() {
            args.push(parser.string_value()?);
        } else {
            return Err(param.into_unexpected_error());
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
    let mut args = Vec::new();
    while let Some(param) = parser.param()? {
        if param.is_short('n') {
            num = parser.value()?;
        } else if param.is_short('1') || param.is_short('2') || param.is_short('3') {
            known_numeric_shorts += 1;
        } else if param.is_arg() {
            args.push(parser.string_value()?);
        } else {
            return Err(param.into_unexpected_error());
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
    let mut args = Vec::new();
    while let Some(param) = parser.param()? {
        if param.is_short('n') {
            num = parser.value()?;
        } else if param.is_short('1') || param.is_short('2') || param.is_short('3') {
            known_numeric_shorts += 1;
        } else if param.is_arg() {
            args.push(parser.string_value()?);
        } else {
            return Err(param.into_unexpected_error());
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
        let mut messages = Vec::new();
        let mut extra = Vec::new();
        while let Some(param) = parser.param()? {
            if param.is_short('m') || param.is_long("message") {
                while parser.looks_at_value() {
                    messages.push(parser.string_value()?);
                }
            } else if param.is_short('x') || param.is_short('1') {
                x += 1;
            } else if param.is_arg() {
                extra.push(parser.string_value()?);
            } else {
                return Err(param.into_unexpected_error());
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
    assert_eq!(parser.string_value()?, "=42");
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
            return Err(param.into_unexpected_error());
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

    assert_eq!(p.param()?, Some(Param::Arg));
    assert_eq!(p.string_value()?, "");
    assert_eq!(p.param()?, Some(Param::Long("-".to_string())));
    assert_eq!(p.string_value()?, "---");
    assert_eq!(p.param()?, Some(Param::Long("-".to_string())));
    assert_eq!(p.string_value()?, "---");
    assert_eq!(p.param()?, Some(Param::Long("".to_string())));
    assert_eq!(p.string_value()?, "");
    assert_eq!(p.param()?, Some(Param::Long("".to_string())));
    assert_eq!(p.string_value()?, "3");
    assert_eq!(p.param()?, Some(Param::Arg));
    assert_eq!(p.string_value()?, "-");
    assert_eq!(p.param()?, Some(Param::Short('x')));
    assert_eq!(p.string_value()?, "--");
    assert_eq!(p.param()?, Some(Param::Arg));
    assert_eq!(p.string_value()?, "-");
    assert_eq!(p.param()?, Some(Param::Short('x')));
    assert_eq!(p.param()?, Some(Param::Arg));
    assert_eq!(p.string_value()?, "");
    assert_eq!(p.param()?, Some(Param::Arg));
    assert_eq!(p.string_value()?, "-");
    assert_eq!(p.param()?, Some(Param::Arg));
    assert_eq!(p.string_value()?, "-x");
    assert_eq!(p.param()?, Some(Param::Arg));
    assert_eq!(p.string_value()?, "---");
    assert_eq!(p.param()?, None);

    Ok(())
}

#[test]
fn test_invalid_unicode() -> Result<(), Error> {
    // Create an OsString with invalid unicode bytes
    let invalid_unicode = OsString::from_vec(vec![0xFF, 0xFF]);

    // Test invalid unicode in positional argument
    let mut parser = Parser::from_args([invalid_unicode.clone()].into_iter());
    assert_eq!(parser.param()?, Some(Param::Arg));
    assert!(parser.string_value().is_err()); // Should fail string parsing
    let mut parser = Parser::from_args([invalid_unicode.clone()].into_iter());
    assert_eq!(parser.param()?, Some(Param::Arg));
    assert_eq!(parser.raw_value()?, invalid_unicode); // But raw value works

    // Test invalid unicode in short option value
    let mut parser = Parser::from_args(["-x".into(), invalid_unicode.clone()].into_iter());
    assert_eq!(parser.param()?, Some(Param::Short('x')));
    assert!(parser.string_value().is_err()); // Should fail string parsing
    let mut parser = Parser::from_args(["-x".into(), invalid_unicode.clone()].into_iter());
    assert_eq!(parser.param()?, Some(Param::Short('x')));
    assert_eq!(parser.raw_value()?, invalid_unicode); // But raw value works

    // Test invalid unicode in short option value with direct concatenation
    let mut arg = OsString::from("-x");
    arg.push(&invalid_unicode);
    let mut parser = Parser::from_args([arg.clone()].into_iter());
    assert_eq!(parser.param()?, Some(Param::Short('x')));
    assert!(parser.string_value().is_err()); // Should fail string parsing
    let mut parser = Parser::from_args([arg].into_iter());
    assert_eq!(parser.param()?, Some(Param::Short('x')));
    assert_eq!(parser.raw_value()?, invalid_unicode); // But raw value works

    // Test invalid unicode in long option value
    let mut parser = Parser::from_args(["--foo".into(), invalid_unicode.clone()].into_iter());
    assert_eq!(parser.param()?, Some(Param::Long("foo".into())));
    assert!(parser.string_value().is_err()); // Should fail string parsing
    let mut parser = Parser::from_args(["--foo".into(), invalid_unicode.clone()].into_iter());
    assert_eq!(parser.param()?, Some(Param::Long("foo".into())));
    assert_eq!(parser.raw_value()?, invalid_unicode); // But raw value works

    // Test invalid unicode in long option value with equals
    let mut arg = OsString::from("--foo=");
    arg.push(&invalid_unicode);
    let mut parser = Parser::from_args([arg.clone()].into_iter());
    assert_eq!(parser.param()?, Some(Param::Long("foo".into())));
    assert!(parser.string_value().is_err()); // Should fail string parsing
    let mut parser = Parser::from_args([arg].into_iter());
    assert_eq!(parser.param()?, Some(Param::Long("foo".into())));
    assert_eq!(parser.raw_value()?, invalid_unicode); // But raw value works

    // Test invalid unicode in option name - should error
    let invalid_opt = OsString::from_vec(vec![b'-', 0xFF, 0xFF]);
    let mut parser = Parser::from_args([invalid_opt].into_iter());
    assert!(parser.param().is_err());

    Ok(())
}

#[test]
fn test_raw_arg_options_handling() -> Result<(), Error> {
    // Parse -- as raw arg (doesn't disable options)
    let mut parser = Parser::from_args(["foo", "bar", "--", "-x"].into_iter());
    assert_eq!(parser.raw_arg(), Some("foo".into()));
    assert_eq!(parser.string_value()?, "bar");
    assert_eq!(parser.raw_arg(), Some("--".into()));
    assert_eq!(parser.param()?, Some(Param::Short('x')));
    assert!(parser.finished());

    // Parse -- as string (disables options)
    let mut parser = Parser::from_args(["foo", "bar", "--", "-x"].into_iter());
    assert_eq!(parser.raw_arg(), Some("foo".into()));
    assert_eq!(parser.string_value()?, "bar");
    assert_eq!(parser.param()?, Some(Param::Arg));
    assert_eq!(parser.string_value()?, "-x");
    assert!(parser.finished());

    Ok(())
}
