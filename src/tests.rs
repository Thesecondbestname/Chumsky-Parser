
use super::*;
    #[test]
    fn test_structs() {
        let input = r#"struct baz:
        lmao: int,
        lmao2: int
    ;"#;
    test(input);
    }
    #[test]
    fn test_use() {
        let input = r#"use foo::bar::baz "#;
        test(input);
    }
    #[test]
    fn test_seperator() {
        let input = r#"x= 50
        print(ksjdfo) "#;
        test(input);
    }
    #[test]
    fn test_angery_case() {
        let input = r#"x = 50.sqrt
        y = ksjdfo
        print(Works?)"#;
        test(input);
    }
    #[test]
    fn test_assign() {
        let input = r#"x = 5 + 5 * (69 +420)
        "#;
        test( input);
    }
    #[test]
    fn test_bool_expr() {
        let input = r#"4 == 4 and 5 <= (5 + 1)"#;
        test(input);
    }
    #[test]
    fn test_call() {
        let input = r#"foo.bar(test) "#;
        test(input);
    }
    #[test]
    fn test_string(){
        let input = r#""Hi!""#;
        test(input);
    }

    #[test]
    fn test_multiple_expressions() {
        let input = r#"(x = 4+5 x= 32)"#;
        test(input);
    }
    #[test]
    fn test_conditions() {
        let input = r#"if 4 == 4 (print(foo))"#;
        test( input);
        
    }
    #[test]
    fn test_blocks() {
        let input = r#"( print(foo) )"#;
        test(input);
    }
    #[test]
    fn test_call_string() {
        let input = r#"print ("foo")"#;
        test(input);
    }
     #[test]
    fn test_math_operation() {
        let input = r#"2+7/(3+4)"#;
        test(input);
    }
    fn test( input: &str){
        let parse = parser().parse(input);
        println!("{:?}",parse);
        debug_assert!(parse.is_ok());
    }
