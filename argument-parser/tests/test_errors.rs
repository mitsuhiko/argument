use std::ffi::OsString;

use argument_parser::{Error, ErrorKind, Param, Parser};

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
fn test_missing_value_error() -> Result<(), Error> {
    let mut parser = Parser::from_args(["-n"]);
    assert_eq!(parser.param()?, Some(Param::Short('n')));
    let err = parser.value::<i64>().unwrap_err();
    assert_eq!(err.kind(), ErrorKind::MissingValue);
    assert_eq!(err.to_string(), "missing argument for '-n'");

    Ok(())
}

#[test]
fn test_invalid_value_error() -> Result<(), Error> {
    let mut parser = Parser::from_args(["-ninvalid"]);
    assert_eq!(parser.param()?, Some(Param::Short('n')));
    let err = parser.value::<i64>().unwrap_err();
    assert_eq!(err.kind(), ErrorKind::InvalidValue);
    assert_eq!(err.to_string(), "invalid value for '-n'");
    assert_eq!(
        format!("{:#}", err),
        "invalid value for '-n': \"invalid\" (invalid digit found in string)"
    );

    Ok(())
}

#[test]
fn test_invalid_unicode_error() -> Result<(), Error> {
    // Create an invalid unicode string
    let mut invalid_unicode = OsString::from("-n");
    invalid_unicode.push(make_invalid_unicode_os_string());

    let mut parser = Parser::from_args([&invalid_unicode]);
    assert_eq!(parser.param()?, Some(Param::Short('n')));
    let err = parser.value::<i64>().unwrap_err();
    assert_eq!(err.kind(), ErrorKind::InvalidUnicode);
    assert_eq!(
        err.to_string(),
        "argument for '-n' contains invalid unicode"
    );
    assert_eq!(
        format!("{:#}", err),
        format!(
            "argument for '-n' contains invalid unicode: {:?}",
            make_invalid_unicode_os_string()
        )
    );

    Ok(())
}

#[test]
fn test_missing_positional_error() -> Result<(), Error> {
    let mut parser = Parser::from_args(Vec::<OsString>::new());
    let err = parser.value::<String>().unwrap_err();
    assert_eq!(err.kind(), ErrorKind::MissingValue);
    assert_eq!(err.to_string(), "missing argument");

    let mut parser = Parser::from_args(["-n"]);
    assert_eq!(parser.param()?, Some(Param::Short('n')));
    let err = parser.value::<String>().unwrap_err();
    assert_eq!(err.kind(), ErrorKind::MissingValue);
    assert_eq!(err.to_string(), "missing argument for '-n'");

    let mut parser = Parser::from_args(["--name"]);
    assert_eq!(parser.param()?, Some(Param::Long("name".into())));
    let err = parser.value::<String>().unwrap_err();
    assert_eq!(err.kind(), ErrorKind::MissingValue);
    assert_eq!(err.to_string(), "missing argument for '--name'");
    Ok(())
}

#[test]
fn test_custom_error() -> Result<(), Error> {
    let err: Error = "custom error message".into();
    assert_eq!(err.kind(), ErrorKind::Custom);
    assert_eq!(err.to_string(), "custom error message");
    assert_eq!(format!("{:#}", err), "custom error message");
    assert!(err.value().is_none());

    let err: Error = String::from("custom error from String").into();
    assert_eq!(err.kind(), ErrorKind::Custom);
    assert_eq!(err.to_string(), "custom error from String");
    assert_eq!(format!("{:#}", err), "custom error from String");
    assert!(err.value().is_none());

    Ok(())
}

#[test]
fn test_unexpected_param_error() -> Result<(), Error> {
    let mut p = Parser::from_args(vec!["-x"]);
    p.param().unwrap();
    let err = p.unexpected();
    assert_eq!(err.kind(), ErrorKind::UnexpectedParam);
    assert_eq!(err.to_string(), "unexpected argument '-x'");

    let mut p = Parser::from_args(vec!["--test"]);
    p.param().unwrap();
    let err = p.unexpected();
    assert_eq!(err.kind(), ErrorKind::UnexpectedParam);
    assert_eq!(err.to_string(), "unexpected argument '--test'");

    let mut p = Parser::from_args(vec!["pos"]);
    p.param().unwrap();
    let err = p.unexpected();
    assert_eq!(err.kind(), ErrorKind::UnexpectedParam);
    assert_eq!(err.to_string(), "unexpected argument");
    assert_eq!(err.value(), Some("pos"));

    // when param() is not called, we get the current parameter value
    let mut p = Parser::from_args(vec!["pos"]);
    let err = p.unexpected();
    assert_eq!(err.kind(), ErrorKind::UnexpectedParam);
    assert_eq!(err.to_string(), "unexpected argument");
    assert_eq!(err.value(), Some("pos"));

    // the way this works is that it might even parse a parameter if it has to
    let mut p = Parser::from_args(vec!["--foo=bar"]);
    let err = p.unexpected();
    assert_eq!(err.kind(), ErrorKind::UnexpectedParam);
    assert_eq!(err.to_string(), "unexpected argument '--foo'");

    Ok(())
}

#[test]
fn test_value_overflow() -> Result<(), Error> {
    let mut parser = Parser::from_args(["999999999999999999999999"]);
    assert_eq!(parser.param()?, Some(Param::Pos));
    let err = parser.value::<i32>().unwrap_err();
    assert_eq!(err.kind(), ErrorKind::InvalidValue);
    assert_eq!(err.to_string(), "invalid value for argument");
    assert_eq!(format!("{:#}", err), "invalid value for argument: \"999999999999999999999999\" (number too large to fit in target type)");
    Ok(())
}
