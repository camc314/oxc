use oxc_diagnostics::OxcDiagnostic;
use oxc_macros::declare_oxc_lint;
use oxc_span::Span;

use crate::{
    AstNode,
    context::LintContext,
    fixer::{RuleFix, RuleFixer},
    rule::Rule,
};

fn prefer_at_diagnostic(span: Span) -> OxcDiagnostic {
    // See <https://oxc.rs/docs/contribute/linter/adding-rules.html#diagnostics> for details
    OxcDiagnostic::warn("Should be an imperative statement about what is wrong")
        .with_help("Should be a command-like statement that tells the user how to fix the issue")
        .with_label(span)
}

#[derive(Debug, Default, Clone)]
pub struct PreferAt;

// See <https://github.com/oxc-project/oxc/issues/6050> for documentation details.
declare_oxc_lint!(
    /// ### What it does
    ///
    /// Briefly describe the rule's purpose.
    ///
    /// ### Why is this bad?
    ///
    /// Explain why violating this rule is problematic.
    ///
    /// ### Examples
    ///
    /// Examples of **incorrect** code for this rule:
    /// ```js
    /// FIXME: Tests will fail if examples are missing or syntactically incorrect.
    /// ```
    ///
    /// Examples of **correct** code for this rule:
    /// ```js
    /// FIXME: Tests will fail if examples are missing or syntactically incorrect.
    /// ```
    PreferAt,
    unicorn,
    nursery, // TODO: change category to `correctness`, `suspicious`, `pedantic`, `perf`, `restriction`, or `style`
             // See <https://oxc.rs/docs/contribute/linter.html#rule-category> for details
    pending  // TODO: describe fix capabilities. Remove if no fix can be done,
             // keep at 'pending' if you think one could be added but don't know how.
             // Options are 'fix', 'fix_dangerous', 'suggestion', and 'conditional_fix_suggestion'
);

impl Rule for PreferAt {
    fn run<'a>(&self, node: &AstNode<'a>, ctx: &LintContext<'a>) {
        // TODO
    }
}

#[test]
fn test() {
    use crate::tester::Tester;

    let pass = vec![
        ("array.at(-1)", None),
        ("array[array.length - 0];", None),
        ("array[array.length + 1]", None),
        ("array[array.length + -1]", None),
        ("foo[bar.length - 1]", None),
        ("array?.[array.length - 1];", None),
        ("array[array.length - 1] = 1", None),
        ("array[array.length - 1] %= 1", None),
        ("++ array[array.length - 1]", None),
        ("array[array.length - 1] --", None),
        ("delete array[array.length - 1]", None),
        ("class Foo {bar; #bar; baz() {return this.#bar[this.bar.length - 1]}}", None),
        ("([array[array.length - 1]] = [])", None),
        ("({foo: array[array.length - 1] = 9} = {})", None),
        ("string.charAt(string.length - 0);", None),
        ("string.charAt(string.length + 1)", None),
        ("string.charAt(string.length + -1)", None),
        ("foo.charAt(bar.length - 1)", None),
        ("string?.charAt?.(string.length - 1);", None),
        ("string?.charAt(string.length - 1);", None),
        ("string.charAt(9);", None),
        ("array.slice(-1)", None),
        ("new array.slice(-1)", None),
        ("array.slice(-0)[0]", None),
        ("array.slice(-9).pop()", None),
        ("array.slice(-1.1)[0]", None),
        ("array.slice(-1)?.[0]", None),
        ("array.slice?.(-1)[0]", None),
        ("array?.slice(-1)[0]", None),
        ("array.notSlice(-1)[0]", None),
        ("array.slice()[0]", None),
        ("array.slice(...[-1])[0]", None),
        ("array.slice(-1).shift?.()", None),
        ("array.slice(-1)?.shift()", None),
        ("array.slice(-1).shift(...[])", None),
        ("new array.slice(-1).shift()", None),
        ("array.slice(-1)[0] += 1", None),
        ("++ array.slice(-1)[0]", None),
        ("array.slice(-1)[0] --", None),
        ("delete array.slice(-1)[0]", None),
        ("array.slice(-9)[0]", None),
        ("array.slice(-9).shift()", None),
        ("array.slice(-0xA)[0b000]", None),
        ("array.slice(-9.1, -8.1)[0]", None),
        ("array.slice(-unknown, -unknown2)[0]", None),
        ("array.slice(-9.1, unknown)[0]", None),
        ("array.slice(-9, unknown).pop()", None),
        ("array.slice(-9, ...unknown)[0]", None),
        ("array.slice(...[-9], unknown)[0]", None),
        ("new _.last(array)", None),
        ("_.last(array, 2)", None),
        ("_.last(...array)", None),
    ];

    let fail = vec![
        ("array[array.length - 1];", None),
        ("array[array.length -1];", None),
        ("array[array.length - /* comment */ 1];", None),
        ("array[array.length - 1.];", None),
        ("array[array.length - 0b1];", None),
        ("array[array.length - 9];", None),
        ("array[0][array[0].length - 1];", None),
        ("array[(( array.length )) - 1];", None),
        ("array[array.length - (( 1 ))];", None),
        ("array[(( array.length - 1 ))];", None),
        ("(( array ))[array.length - 1];", None),
        ("(( array[array.length - 1] ));", None),
        ("array[array.length - 1].pop().shift()[0];", None),
        ("a = array[array.length - 1]", None),
        ("const a = array[array.length - 1]", None),
        ("const {a = array[array.length - 1]} = {}", None),
        ("typeof array[array.length - 1]", None),
        ("function foo() {return arguments[arguments.length - 1]}", None),
        ("class Foo {bar; baz() {return this.bar[this.bar.length - 1]}}", None),
        ("class Foo {#bar; baz() {return this.#bar[this.#bar.length - 1]}}", None),
        ("string.charAt(string.length - 1);", None),
        ("string.charAt(string.length - 0o11);", None),
        ("some.string.charAt(some.string.length - 1);", None),
        ("string.charAt((( string.length )) - 0xFF);", None),
        ("string.charAt(string.length - (( 1 )));", None),
        ("string.charAt((( string.length - 1 )));", None),
        ("(( string )).charAt(string.length - 1);", None),
        ("(( string.charAt ))(string.length - 1);", None),
        ("(( string.charAt(string.length - 1) ));", None),
        ("array.slice(-1)[0]", None),
        ("array.slice(-1).pop()", None),
        ("array.slice(-1.0).shift()", None),
        ("array.slice(-1)[(( 0 ))];", None),
        ("array.slice(-(( 1 )))[0];", None),
        ("array.slice((( -1 )))[0];", None),
        ("(( array.slice(-1) ))[0];", None),
        ("(( array )).slice(-1)[0];", None),
        ("(( array.slice(-1)[0] ));", None),
        ("(( array.slice(-1) )).pop();", None),
        ("(( array.slice(-1).pop ))();", None),
        ("(( array.slice(-1).pop() ));", None),
        ("array.slice(-1)[0].pop().shift().slice(-1)", None),
        ("array.slice(-9, -8)[0]", None),
        ("array.slice(-9, -0o10)[0]", None),
        ("array.slice(-9, -8).pop()", None),
        ("array.slice(-9, -8).shift()", None),
        ("array.slice((( -9 )), (( -8 )), ).shift()", None),
        ("(( array.slice(-9, -8).shift ))()", None),
        ("array.slice(-9, unknown)[0]", None),
        ("array.slice(-0o11, -7)[0]", None),
        ("array.slice(-9, unknown).shift()", None),
        ("const KNOWN = -8; array.slice(-9, KNOWN).shift()", None),
        ("array.slice(-9, 0)[0]", None),
        ("(( (( array.slice( ((-9)), ((unknown)), ).shift ))() ));", None),
        ("array.slice(-9, (a, really, _really, complicated, second) => argument)[0]", None),
        ("_.last(array)", None),
        ("lodash.last(array)", None),
        ("underscore.last(array)", None),
        ("_.last(new Array)", None),
        (
            "const foo = []
			_.last([bar])",
            None,
        ),
        (
            "const foo = []
			_.last( new Array )",
            None,
        ),
        (
            "const foo = []
			_.last( (( new Array )) )",
            None,
        ),
        ("if (foo) _.last([bar])", None),
        (
            "_.last(getLast(utils.lastOne(array)))",
            Some(
                serde_json::json!([{"getLastElementFunctions": ["getLast", "  utils.lastOne  "]}]),
            ),
        ),
        ("function foo() {return _.last(arguments)}", None),
    ];

    Tester::new(PreferAt::NAME, PreferAt::PLUGIN, pass, fail).test_and_snapshot();
}
