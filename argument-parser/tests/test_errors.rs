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
    let mut parser = Parser::from_args(["-n"].into_iter());
    assert_eq!(parser.param()?, Some(Param::Short('n')));
    let err = parser.value::<i64>().unwrap_err();
    assert_eq!(err.kind(), ErrorKind::MissingValue);
    assert_eq!(err.to_string(), "missing argument for '-n'");
    assert_eq!(err.param(), Some(&Param::Short('n')));
    assert!(err.value().is_none());

    Ok(())
}

#[test]
fn test_invalid_value_error() -> Result<(), Error> {
    let mut parser = Parser::from_args(["-ninvalid"].into_iter());
    assert_eq!(parser.param()?, Some(Param::Short('n')));
    let err = parser.value::<i64>().unwrap_err();
    assert_eq!(err.kind(), ErrorKind::InvalidValue);
    assert_eq!(err.to_string(), "invalid value for '-n'");
    assert_eq!(
        format!("{:#}", err),
        "invalid value for '-n': \"invalid\" (invalid digit found in string)"
    );
    assert_eq!(err.param(), Some(&Param::Short('n')));
    assert_eq!(err.value(), Some("invalid"));

    Ok(())
}

#[test]
fn test_invalid_unicode_error() -> Result<(), Error> {
    // Create an invalid unicode string
    let mut invalid_unicode = OsString::from("-n42");
    invalid_unicode.push(make_invalid_unicode_os_string());

    let mut parser = Parser::from_args([invalid_unicode].into_iter());
    assert_eq!(parser.param()?, Some(Param::Short('n')));
    let err = parser.value::<i64>().unwrap_err();
    assert_eq!(err.kind(), ErrorKind::InvalidUnicode);
    assert_eq!(
        err.to_string(),
        "argument for '-n' contains invalid unicode"
    );
    assert_eq!(
        format!("{:#}", err),
        "argument for '-n' contains invalid unicode: \"42\\xFF\\xFF\""
    );
    assert_eq!(err.param(), Some(&Param::Short('n')));
    assert!(err.value().is_none());

    Ok(())
}
