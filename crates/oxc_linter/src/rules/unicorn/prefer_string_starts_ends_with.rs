use oxc_allocator::Allocator;
use oxc_ast::{
    AstBuilder, AstKind,
    ast::{CallExpression, Expression, MemberExpression, RegExpFlags, RegExpLiteral},
};
use oxc_diagnostics::OxcDiagnostic;
use oxc_macros::declare_oxc_lint;
use oxc_regular_expression::ast::{BoundaryAssertionKind, Term};
use oxc_span::{GetSpan, SPAN, Span};

use crate::{
    AstNode,
    context::LintContext,
    fixer::{RuleFix, RuleFixer},
    rule::Rule,
};

fn starts_with(span: Span) -> OxcDiagnostic {
    OxcDiagnostic::warn("Prefer String#startsWith over a regex with a caret.").with_label(span)
}

fn ends_with(span: Span) -> OxcDiagnostic {
    OxcDiagnostic::warn("Prefer String#endsWith over a regex with a dollar sign.").with_label(span)
}

#[derive(Debug, Default, Clone)]
pub struct PreferStringStartsEndsWith;

declare_oxc_lint!(
    /// ### What it does
    ///
    /// Prefer [`String#startsWith()`](https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Global_Objects/String/startsWith) and [`String#endsWith()`](https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Global_Objects/String/endsWith) over using a regex with `/^foo/` or `/foo$/`.
    ///
    /// ### Why is this bad?
    ///
    /// Using `String#startsWith()` and `String#endsWith()` is more readable and performant as it does not need to parse a regex.
    ///
    /// ### Examples
    ///
    /// Examples of **incorrect** code for this rule:
    /// ```javascript
    /// const foo = "hello";
    /// /^abc/.test(foo);
    /// ```
    ///
    /// Examples of **correct** code for this rule:
    /// ```javascript
    /// const foo = "hello";
    /// foo.startsWith("abc");
    /// ```
    PreferStringStartsEndsWith,
    unicorn,
    correctness,
    fix
);

impl Rule for PreferStringStartsEndsWith {
    fn run<'a>(&self, node: &AstNode<'a>, ctx: &LintContext<'a>) {
        let AstKind::CallExpression(call_expr) = node.kind() else {
            return;
        };

        if call_expr.optional {
            return;
        }

        let Some(member_expr) = call_expr.callee.get_member_expr() else {
            return;
        };

        let MemberExpression::StaticMemberExpression(static_member_expr) = &member_expr else {
            return;
        };

        if !matches!(static_member_expr.property.name.as_str(), "test") {
            return;
        }

        let Expression::RegExpLiteral(regex) = &member_expr.object().without_parentheses() else {
            return;
        };

        let Some(err_kind) = check_regex(regex) else {
            return;
        };

        match err_kind {
            ErrorKind::StartsWith(_) => {
                ctx.diagnostic_with_fix(starts_with(member_expr.span()), |fixer| {
                    do_fix(fixer, err_kind, call_expr)
                });
            }
            ErrorKind::EndsWith(_) => {
                ctx.diagnostic_with_fix(ends_with(member_expr.span()), |fixer| {
                    do_fix(fixer, err_kind, call_expr)
                });
            }
        }
    }
}

fn do_fix<'a>(
    fixer: RuleFixer<'_, 'a>,
    err_kind: ErrorKind,
    call_expr: &CallExpression<'a>,
) -> RuleFix<'a> {
    let Some(target_span) = can_replace(call_expr) else { return fixer.noop() };
    let (argument, method) = match err_kind {
        ErrorKind::StartsWith(arg) => {
            (arg.into_iter().map(std::char::from_u32).collect::<Option<String>>(), "startsWith")
        }
        ErrorKind::EndsWith(arg) => {
            (arg.into_iter().map(std::char::from_u32).collect::<Option<String>>(), "endsWith")
        }
    };
    let Some(argument) = argument else { return fixer.noop() };
    let mut content = fixer.codegen();
    let alloc = Allocator::default();
    let ast = AstBuilder::new(&alloc);
    content.print_str(&format!(r"{}.{}(", fixer.source_range(target_span), method));
    content.print_expression(&ast.expression_string_literal(SPAN, ast.atom(&argument), None));
    content.print_str(r")");
    fixer.replace(call_expr.span, content)
}

fn can_replace(call_expr: &CallExpression) -> Option<Span> {
    if call_expr.arguments.len() != 1 {
        return None;
    }

    let arg = &call_expr.arguments[0];
    let expr = arg.as_expression()?;
    match expr.without_parentheses() {
        Expression::StringLiteral(s) => Some(s.span),
        Expression::TemplateLiteral(s) => Some(s.span),
        Expression::Identifier(ident) => Some(ident.span),
        Expression::StaticMemberExpression(m) => Some(m.span),
        Expression::ComputedMemberExpression(m) => Some(m.span),
        Expression::CallExpression(c) => Some(c.span),
        _ => None,
    }
}

enum ErrorKind {
    StartsWith(Vec<u32>),
    EndsWith(Vec<u32>),
}

fn check_regex(regexp_lit: &RegExpLiteral) -> Option<ErrorKind> {
    let pattern_text = regexp_lit.regex.pattern.text.as_str();
    if regexp_lit.regex.flags.intersects(RegExpFlags::M)
        || (regexp_lit.regex.flags.intersects(RegExpFlags::I | RegExpFlags::M)
            && is_useless_case_sensitive_regex_flag(pattern_text))
    {
        return None;
    }

    let alternatives =
        regexp_lit.regex.pattern.pattern.as_ref().map(|pattern| &pattern.body.body)?;
    // Must not be something with multiple alternatives like `/^a|b/`
    if alternatives.len() > 1 {
        return None;
    }
    let pattern_terms = alternatives.first().map(|it| &it.body)?;

    if let Some(Term::BoundaryAssertion(boundary_assert)) = pattern_terms.first() {
        if boundary_assert.kind == BoundaryAssertionKind::Start {
            return pattern_terms
                .iter()
                .skip(1)
                .map(|t| if let Term::Character(c) = t { Some(c.value) } else { None })
                .collect::<Option<Vec<_>>>()
                .map(ErrorKind::StartsWith);
        }
    }

    if let Some(Term::BoundaryAssertion(boundary_assert)) = pattern_terms.last() {
        if boundary_assert.kind == BoundaryAssertionKind::End {
            return pattern_terms
                .iter()
                .take(pattern_terms.len() - 1)
                .map(|t| if let Term::Character(c) = t { Some(c.value) } else { None })
                .collect::<Option<Vec<_>>>()
                .map(ErrorKind::EndsWith);
        }
    }

    None
}

// `/^#/i` => `true` (the `i` flag is useless)
// `/^foo/i` => `false` (the `i` flag is not useless)
fn is_useless_case_sensitive_regex_flag(pattern_text: &str) -> bool {
    // ignore `^` and `$` (start and end of string)
    let pat = pattern_text.trim_start_matches('^').trim_end_matches('$');
    pat.chars().any(|c| c.is_ascii_alphabetic())
}

#[test]
fn test() {
    use crate::tester::Tester;

    let pass = vec![
        // Unicorn Tests
        r#"foo.startsWith("bar")"#,
        r#"foo.endsWith("bar")"#,
        r#"reject(new Error("foo"))"#,
        r#""".test()"#,
        r"test()",
        r"test.test()",
        r#"startWith("bar")"#,
        r"foo()()",
        r"if (foo.match(/^foo/)) {}",
        r"if (/^foo/.exec(foo)) {}",
        r"/foo/.test(bar)",
        r"/^foo$/.test(bar)",
        r"/^foo+/.test(bar)",
        r"/foo+$/.test(bar)",
        r"/^[,af]/.test(bar)",
        r"/[,af]$/.test(bar)",
        r"/^\w/.test(bar)",
        r"/\w$/.test(bar)",
        r"/^foo./.test(bar)",
        r"/foo.$/.test(bar)",
        r"/\^foo/.test(bar)",
        r"/^foo/i.test(bar)",
        r"/^foo0/i.test(bar)",
        r"/^foo/m.test(bar)",
        r"/^foo/im.test(bar)",
        r"/^A|B/.test(bar)",
        r"/A|B$/.test(bar)",
        // Additional tests
        r"/^http/i.test(uri)",
        r"if (/^a/i.test(hex)) {}",
        r"if (/a$/i.test(hex)) {}",
    ];

    let fail = vec![
        r"/^foo/.test(bar)",
        r"/foo$/.test(bar)",
        r"/\$$/.test(bar)",
        r"/^\^/.test(bar)",
        r"/^!/.test(bar)",
        r"/!$/.test(bar)",
        r"/^ /.test(bar)",
        r"/ $/.test(bar)",
        r"const foo = {}; /^abc/.test(foo);",
        r"const foo = 123; /^abc/.test(foo);",
        r#"const foo = "hello"; /^abc/.test(foo);"#,
        r"/^b/.test((a))",
        r"(/^b/).test((a))",
        r"const fn = async () => /^b/.test(await foo)",
        r"const fn = async () => (/^b/).test(await foo)",
        r#"/^a/.test("string")"#,
        r#"/^a/.test((0, "string"))"#,
        r"async function a() {return /^a/.test(await foo())}",
        r"/^a/.test(foo + bar)",
        r"/^a/.test(foo || bar)",
        r"/^a/.test(new SomeString)",
        r"/^a/.test(new (SomeString))",
        r"/^a/.test(new SomeString())",
        r"/^a/.test(new new SomeClassReturnsAStringSubClass())",
        r"/^a/.test(new SomeString(/* comment */))",
        r#"/^a/.test(new SomeString("string"))"#,
        r"/^a/.test(foo.bar)",
        r"/^a/.test(foo.bar())",
        r"/^a/.test(foo?.bar)",
        r"/^a/.test(foo?.bar())",
        r"/^a/.test(`string`)",
        r"/^a/.test(tagged`string`)",
        r#"(/^a/).test((0, "string"))"#,
        r"/^a/.test(true ? a : b)",
        r"/a$/.test(a ??= b)",
        r"/^a/.test(a || b)",
        r"/^a/.test(a && b)",
        r#"/^a/u.test("string")"#,
        r#"/^a/v.test("string")"#,
        r"/a$/.test(`${unknown}`)",
        r"/a$/.test(String(unknown))",
        r"const a = /你$/.test('a');",
        r"const a = /^你/.test('a');",
        r"if (/^#/i.test(hex)) {}",
        r"if (/#$/i.test(hex)) {}",
    ];

    let fix = vec![
        ("/^foo/.test(x)", r"x.startsWith('foo')", None),
        ("/foo$/.test(x)", r"x.endsWith('foo')", None),
        ("/^foo/.test(x.y)", r"x.y.startsWith('foo')", None),
        ("/foo$/.test(x.y)", r"x.y.endsWith('foo')", None),
        ("/^foo/.test('x')", r"'x'.startsWith('foo')", None),
        ("/foo$/.test('x')", r"'x'.endsWith('foo')", None),
        ("/^foo/.test(`x${y}`)", r"`x${y}`.startsWith('foo')", None),
        ("/foo$/.test(`x${y}`)", r"`x${y}`.endsWith('foo')", None),
        ("/^foo/.test(String(x))", r"String(x).startsWith('foo')", None),
        ("/foo$/.test(String(x))", r"String(x).endsWith('foo')", None),
        // https://github.com/oxc-project/oxc/issues/10523
        (
            r"const makePosix = str => /^\\\\\?\\/.test(str)",
            r"const makePosix = str => str.startsWith('\\\\?\\')",
            None,
        ),
        ("/^'/.test('foo')", r"'foo'.startsWith('\'')", None),
        (r#"/^"/.test('foo')"#, r#"'foo'.startsWith('"')"#, None),
        // should not get fixed
        ("/^foo/.test(new String('bar'))", "/^foo/.test(new String('bar'))", None),
        ("/^foo/.test(x as string)", "/^foo/.test(x as string)", None),
        ("/^foo/.test(5)", "/^foo/.test(5)", None),
        ("/^foo/.test(x?.y)", "/^foo/.test(x?.y)", None),
        ("/^foo/.test(x + y)", "/^foo/.test(x + y)", None),
    ];

    Tester::new(PreferStringStartsEndsWith::NAME, PreferStringStartsEndsWith::PLUGIN, pass, fail)
        .expect_fix(fix)
        .test_and_snapshot();
}
