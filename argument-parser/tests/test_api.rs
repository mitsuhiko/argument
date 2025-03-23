use argument_parser::Param;

#[test]
fn test_param_short() {
    let param = Param::Short('a');
    assert!(param.is_short('a'));
    assert!(!param.is_short('x'));
    assert!(!param.is_long("blah"));
    assert!(param.is_either('a', "blah"));
    assert!(!param.is_pos());
}

#[test]
fn test_param_long() {
    let param = Param::Long("test".to_string());
    assert!(!param.is_short('a'));
    assert!(param.is_long("test"));
    assert!(!param.is_long("other"));
    assert!(param.is_either('t', "test"));
    assert!(!param.is_pos());
}

#[test]
fn test_param_arg() {
    let param = Param::Pos;
    assert!(!param.is_short('a'));
    assert!(!param.is_long("test"));
    assert!(!param.is_either('a', "test"));
    assert!(param.is_pos());
}
