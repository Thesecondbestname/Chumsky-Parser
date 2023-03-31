use crate::ast;
struct Compiler<'a> {
    ast: ast::Expression<'a>,
    error: CompilerError,
}
enum CompilerError {

}
