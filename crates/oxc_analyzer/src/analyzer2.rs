#![allow(clippy::todo, clippy::wildcard_imports, unused)]

use std::{
    cell::Cell,
    collections::{HashMap, HashSet},
    fmt::Display,
    fs,
    hash::Hash,
    path::Path,
};

use crate::types::*;

use oxc_allocator::Allocator;
use oxc_ast::ast::*;
use oxc_index::Idx;
use oxc_parser::Parser;
use oxc_semantic::{Semantic, SemanticBuilder};
use oxc_span::{GetSpan, SourceType, Span};
use oxc_syntax::{
    operator::{AssignmentOperator, BinaryOperator},
    reference::ReferenceId,
    scope::{ScopeFlags, ScopeId},
    symbol::SymbolId,
};

struct Scopes {}
impl Scopes {
    fn enter_scope(&mut self) {}
    fn leave_scope(&mut self) {}
}

pub struct Analyzer {
    scopes: Scopes,
}

impl<'a> Analyzer {
    pub fn new() -> Self {
        Self { scopes: Scopes {} }
    }

    pub fn enter_scope(&mut self, flags: ScopeFlags, scope_id: &Cell<Option<ScopeId>>) {
        self.scopes.enter_scope();
    }

    pub fn leave_scope(&mut self) {
        self.scopes.leave_scope();
    }
}

#[allow(unused)]
impl<'a> Analyzer {
    pub fn analyze_program(&mut self, it: &oxc_ast::ast::Program<'a>) {
        self.scopes.enter_scope();

        for stmt in &it.body {
            self.analyze_statement(stmt);
        }

        self.scopes.leave_scope();
    }

    fn analyze_statement(&mut self, it: &oxc_ast::ast::Statement<'a>) {
        match it {
            Statement::BlockStatement(v) => self.analyze_block_statement(v),
            Statement::BreakStatement(v) => self.analyze_break_statement(v),
            Statement::ContinueStatement(v) => self.analyze_continue_statement(v),
            Statement::DebuggerStatement(v) => self.analyze_debugger_statement(v),
            Statement::DoWhileStatement(v) => self.analyze_do_while_statement(v),
            Statement::EmptyStatement(v) => self.analyze_empty_statement(v),
            Statement::ExpressionStatement(v) => self.analyze_expression_statement(v),
            Statement::ForInStatement(v) => self.analyze_for_in_statement(v),
            Statement::ForOfStatement(v) => self.analyze_for_of_statement(v),
            Statement::ForStatement(v) => self.analyze_for_statement(v),
            Statement::IfStatement(v) => self.analyze_if_statement(v),
            Statement::LabeledStatement(v) => self.analyze_labeled_statement(v),
            Statement::ReturnStatement(v) => self.analyze_return_statement(v),
            Statement::SwitchStatement(v) => self.analyze_switch_statement(v),
            Statement::ThrowStatement(v) => self.analyze_throw_statement(v),
            Statement::TryStatement(v) => self.analyze_try_statement(v),
            Statement::WhileStatement(v) => self.analyze_while_statement(v),
            Statement::WithStatement(v) => self.analyze_with_statement(v),
            Statement::VariableDeclaration(v) => self.analyze_variable_declaration(v),
            Statement::FunctionDeclaration(v) => self.analyze_function_declaration(v),
            Statement::ClassDeclaration(v) => self.analyze_class_declaration(v),
            Statement::UsingDeclaration(v) => self.analyze_using_declaration(v),
            Statement::TSTypeAliasDeclaration(v) => self.analyze_ts_type_alias_declaration(v),
            Statement::TSInterfaceDeclaration(v) => self.analyze_ts_interface_declaration(v),
            Statement::TSEnumDeclaration(v) => self.analyze_ts_enum_declaration(v),
            Statement::TSModuleDeclaration(v) => self.analyze_ts_module_declaration(v),
            Statement::TSImportEqualsDeclaration(v) => self.analyze_ts_import_equals_declaration(v),
            Statement::ImportDeclaration(v) => self.analyze_import_declaration(v),
            Statement::ExportAllDeclaration(v) => self.analyze_export_all_declaration(v),
            Statement::ExportDefaultDeclaration(v) => self.analyze_export_default_declaration(v),
            Statement::ExportNamedDeclaration(v) => self.analyze_export_named_declaration(v),
            Statement::TSExportAssignment(v) => self.analyze_ts_export_assignment_statement(v),
            Statement::TSNamespaceExportDeclaration(v) => {
                self.analyze_ts_namespace_export_declaration(v);
            }
        }
    }

    fn analyze_block_statement(&mut self, block_stmt: &BlockStatement<'a>) {
        self.scopes.enter_scope();

        for stmt in &block_stmt.body {
            self.analyze_statement(stmt);
        }

        self.scopes.leave_scope();
    }

    #[allow(clippy::unused_self)]
    fn analyze_break_statement(&self, _break_stmt: &BreakStatement<'a>) {
        // no op
    }

    #[allow(clippy::unused_self)]
    fn analyze_continue_statement(&self, _continue_stmt: &ContinueStatement<'a>) {
        // no op
    }

    #[allow(clippy::unused_self)]
    fn analyze_debugger_statement(&self, _debugger_stmt: &DebuggerStatement) {
        // no op
    }

    fn analyze_do_while_statement(&mut self, do_while_stmt: &DoWhileStatement) {
        self.analyze_statement(&do_while_stmt.body);
        self.analyze_expression(&do_while_stmt.test);
    }

    #[allow(clippy::unused_self)]
    fn analyze_empty_statement(&self, empty_statement: &EmptyStatement) {
        // no op
    }

    fn analyze_expression_statement(&mut self, expression_statement: &ExpressionStatement<'a>) {
        let _ = self.analyze_expression(&expression_statement.expression);
    }

    fn analyze_for_in_statement(&mut self, for_in_statement: &ForInStatement<'a>) {
        let right_ty = self.analyze_expression(&for_in_statement.right);

        self.analyze_for_statement_left(&for_in_statement.left);

        self.analyze_statement(&for_in_statement.body);
    }

    fn analyze_for_statement_left(&mut self, for_statement_left: &ForStatementLeft<'a>) {
        match for_statement_left {
            ForStatementLeft::VariableDeclaration(variable_declaration) => {
                self.analyze_variable_declaration(variable_declaration);
            }
            ForStatementLeft::UsingDeclaration(using_declaration) => {
                self.analyze_using_declaration(using_declaration);
            }
            ForStatementLeft::AssignmentTargetIdentifier(assignment_target_identifier) => {
                self.analyze_identifier_reference(assignment_target_identifier);
            }
            ForStatementLeft::TSAsExpression(ts_as_expression) => {
                self.analyze_ts_as_expression(ts_as_expression);
            }
            ForStatementLeft::TSSatisfiesExpression(ts_satisfies_expression) => {
                self.analyze_ts_satisfies_expression(ts_satisfies_expression);
            }
            ForStatementLeft::TSNonNullExpression(ts_non_null_expression) => {
                self.analyze_ts_non_null_expression(ts_non_null_expression);
            }
            ForStatementLeft::TSTypeAssertion(ts_type_assertion) => {
                self.analyze_ts_type_assertion(ts_type_assertion);
            }
            ForStatementLeft::TSInstantiationExpression(ts_instantiation_expression) => {
                self.analyze_ts_instantiation_expression(ts_instantiation_expression);
            }
            ForStatementLeft::ComputedMemberExpression(computed_member_expression) => {
                self.analyze_computed_member_expression(computed_member_expression);
            }
            ForStatementLeft::StaticMemberExpression(static_member_expression) => {
                self.analyze_static_member_expression(static_member_expression);
            }
            ForStatementLeft::PrivateFieldExpression(private_field_expression) => {
                self.analyze_private_field_expression(private_field_expression);
            }
            ForStatementLeft::ArrayAssignmentTarget(array_assignment_target) => {
                todo!()
            }
            ForStatementLeft::ObjectAssignmentTarget(object_assignment_target) => {
                todo!()
            }
        }
    }

    fn analyze_for_of_statement(&mut self, for_of_statement: &ForOfStatement<'a>) {
        self.analyze_for_statement_left(&for_of_statement.left);

        self.analyze_statement(&for_of_statement.body);
    }

    fn analyze_for_statement(&mut self, for_statement: &ForStatement<'a>) {
        if let Some(init) = &for_statement.init {
            self.analyze_for_statement_init(init);
        }
        if let Some(test) = &for_statement.test {
            self.analyze_expression(test);
        }
        if let Some(update) = &for_statement.update {
            self.analyze_expression(update);
        }
        self.analyze_statement(&for_statement.body);
    }

    fn analyze_for_statement_init(&mut self, for_statement_init: &ForStatementInit<'a>) {
        match for_statement_init {
            ForStatementInit::VariableDeclaration(variable_declaration) => {
                self.analyze_variable_declaration(variable_declaration);
            }
            ForStatementInit::UsingDeclaration(_) => todo!(),
            _ => {
                self.analyze_expression(for_statement_init.to_expression());
            }
        };
    }

    fn analyze_if_statement(&mut self, if_statement: &IfStatement<'a>) {
        self.analyze_expression(&if_statement.test);
        self.analyze_statement(&if_statement.consequent);
        if let Some(alternate) = &if_statement.alternate {
            self.analyze_statement(alternate);
        }
    }

    #[allow(clippy::unused_self)]
    fn analyze_labeled_statement(&self, labeled_statement: &LabeledStatement<'a>) {
        // no op
    }

    fn analyze_return_statement(&mut self, return_statement: &ReturnStatement<'a>) {
        let return_ty = if let Some(return_argument) = &return_statement.argument {
            self.analyze_expression(return_argument)
        } else {
            Type::Keyword(KeywordType::Void)
        };
    }

    fn analyze_switch_statement(&mut self, switch_statement: &SwitchStatement<'a>) {
        let discriminant_ty = self.analyze_expression(&switch_statement.discriminant);

        for case in &switch_statement.cases {
            self.analyze_switch_case(case, &discriminant_ty);
        }
    }

    fn analyze_switch_case(&mut self, switch_case: &SwitchCase<'a>, _discriminant_ty: &Type) {
        if let Some(test) = &switch_case.test {
            let _test_ty = self.analyze_expression(test);
        }

        for stmt in &switch_case.consequent {
            self.analyze_statement(stmt);
        }
    }

    fn analyze_throw_statement(&mut self, throw_statement: &ThrowStatement<'a>) {
        self.analyze_expression(&throw_statement.argument);
    }

    fn analyze_try_statement(&mut self, try_statement: &TryStatement<'a>) {
        self.analyze_block_statement(&try_statement.block);

        if let Some(handler) = &try_statement.handler {
            self.analyze_catch_clause(handler);
        }
        if let Some(finalizer) = &try_statement.finalizer {
            self.analyze_block_statement(finalizer);
        }
    }

    fn analyze_catch_clause(&mut self, catch_clause: &CatchClause) {
        self.enter_scope(ScopeFlags::CatchClause, &catch_clause.scope_id);
        if let Some(catch_param) = &catch_clause.param {
            self.analyze_catch_parameter(catch_param);
        }

        self.analyze_block_statement(&catch_clause.body);
        self.leave_scope();
    }

    #[allow(clippy::unused_self)]
    fn analyze_catch_parameter(&mut self, catch_param: &CatchParameter<'a>) {
        // TODO
    }

    fn analyze_while_statement(&mut self, while_statement: &WhileStatement<'a>) {
        self.analyze_expression(&while_statement.test);
        self.analyze_statement(&while_statement.body);
    }

    #[allow(clippy::unused_self)]
    fn analyze_with_statement(&self, with_statement: &WithStatement<'a>) {
        // no op
    }

    fn analyze_variable_declaration(&mut self, variable_declaration: &VariableDeclaration<'a>) {
        for decl in &variable_declaration.declarations {
            self.analyze_variable_declarator(decl);
        }
    }

    fn analyze_function_declaration(&mut self, function_declaration: &Function<'a>) {
        self.analyze_function_like(function_declaration, false);
    }

    #[allow(clippy::unused_self)]
    fn analyze_class_declaration(&mut self, class_declaration: &Class<'a>) {
        // TODO
    }

    #[allow(clippy::unused_self)]
    fn analyze_using_declaration(&mut self, using_declaration: &UsingDeclaration<'a>) {
        // TODO
    }
}

impl<'a> Analyzer {
    #[allow(clippy::unused_self)]
    fn analyze_ts_module_declaration(&mut self, ts_module_declaration: &TSModuleDeclaration<'a>) {
        // TODO
    }

    #[allow(clippy::unused_self)]
    fn analyze_ts_import_equals_declaration(
        &mut self,
        ts_import_equals_declaration: &TSImportEqualsDeclaration<'a>,
    ) {
        // TODO
    }

    #[allow(clippy::unused_self)]
    fn analyze_import_declaration(&mut self, import_declaration: &ImportDeclaration<'a>) {
        // TODO
    }

    #[allow(clippy::unused_self)]
    fn analyze_export_all_declaration(
        &mut self,
        export_all_declaration: &ExportAllDeclaration<'a>,
    ) {
        // TODO
    }

    #[allow(clippy::unused_self)]
    fn analyze_export_default_declaration(
        &mut self,
        export_default_declaration: &ExportDefaultDeclaration<'a>,
    ) {
        // TODO
    }

    #[allow(clippy::unused_self)]
    fn analyze_export_named_declaration(
        &mut self,
        export_named_declaration: &ExportNamedDeclaration<'a>,
    ) {
        // TODO
    }

    #[allow(clippy::unused_self)]
    fn analyze_ts_export_assignment_statement(
        &mut self,
        ts_export_assignment_statement: &TSExportAssignment<'a>,
    ) {
        // TODO
    }

    #[allow(clippy::unused_self)]
    fn analyze_ts_namespace_export_declaration(
        &mut self,
        ts_namespace_export_declaration: &TSNamespaceExportDeclaration<'a>,
    ) {
        // TODO
    }
}

#[allow(unused)]
impl<'a> Analyzer {
    fn analyze_variable_declarator(&mut self, variable_declarator: &VariableDeclarator<'a>) {
        todo!()
    }

    fn analyze_object_pattern(&mut self, obj_pattern: &ObjectPattern<'a>, inferred_ty: &Type) {
        todo!()
    }

    fn analyze_array_pattern(&mut self, arr_pattern: &ArrayPattern<'a>, inferred_ty: &Type) {
        todo!()
    }

    fn analyze_assignment_pattern(
        &mut self,
        assign_pattern: &AssignmentPattern<'a>,
        inferred_ty: &Type,
    ) {
        todo!()
    }

    fn analyze_binding_pattern(&mut self, pattern: &BindingPattern<'a>, inferred_ty: Type) {
        todo!()
    }
}
#[allow(unused)]
impl<'a> Analyzer {
    fn analyze_expression(&mut self, expression: &Expression<'a>) -> Type {
        let ty = match &expression {
            Expression::ArrayExpression(v) => self.analyze_array_expression(v),
            Expression::BooleanLiteral(v) => self.analyze_boolean_literal(v),
            Expression::NullLiteral(v) => self.analyze_null_literal(v),
            Expression::NumericLiteral(v) => self.analyze_numeric_literal(v),
            Expression::BigIntLiteral(v) => self.analyze_big_int_literal(v),
            Expression::RegExpLiteral(v) => self.analyze_reg_exp_literal(v),
            Expression::StringLiteral(v) => self.analyze_string_literal(v),
            Expression::TemplateLiteral(v) => self.analyze_template_literal(v),
            Expression::Identifier(v) => self.analyze_identifier_reference(v),
            Expression::MetaProperty(v) => self.analyze_meta_property_expression(v),
            Expression::Super(v) => self.analyze_super_expression(v),
            Expression::ArrowFunctionExpression(v) => self.analyze_arrow_function_expression(v),
            Expression::AssignmentExpression(v) => self.analyze_assignment_expression(v),
            Expression::AwaitExpression(v) => self.analyze_await_expression(v),
            Expression::BinaryExpression(v) => self.analyze_binary_expression(v),
            Expression::CallExpression(v) => self.analyze_call_expression(v),
            Expression::ChainExpression(v) => self.analyze_chain_expression(v),
            Expression::ClassExpression(v) => self.analyze_class_expression(v),
            Expression::ConditionalExpression(v) => self.analyze_conditional_expression(v),
            Expression::FunctionExpression(v) => self.analyze_function_expression(v),
            Expression::ImportExpression(v) => self.analyze_import_expression(v),
            Expression::LogicalExpression(v) => self.analyze_logical_expression(v),
            Expression::NewExpression(v) => self.analyze_new_expression(v),
            Expression::ObjectExpression(v) => self.analyze_object_expression(v),
            Expression::ParenthesizedExpression(v) => self.analyze_parenthesized_expression(v),
            Expression::SequenceExpression(v) => self.analyze_sequence_expression(v),
            Expression::TaggedTemplateExpression(v) => self.analyze_tagged_template_expression(v),
            Expression::ThisExpression(v) => self.analyze_this_expression(v),
            Expression::UnaryExpression(v) => self.analyze_unary_expression(v),
            Expression::UpdateExpression(v) => self.analyze_update_expression(v),
            Expression::YieldExpression(v) => self.analyze_yield_expression(v),
            Expression::PrivateInExpression(v) => self.analyze_private_in_expression(v),
            Expression::JSXElement(v) => self.analyze_jsx_element(v),
            Expression::JSXFragment(v) => self.analyze_jsx_fragment(v),
            Expression::TSAsExpression(v) => self.analyze_ts_as_expression(v),
            Expression::TSSatisfiesExpression(v) => self.analyze_ts_satisfies_expression(v),
            Expression::TSTypeAssertion(v) => self.analyze_ts_type_assertion(v),
            Expression::TSNonNullExpression(v) => self.analyze_ts_non_null_expression(v),
            Expression::TSInstantiationExpression(v) => self.analyze_ts_instantiation_expression(v),
            Expression::ComputedMemberExpression(v) => self.analyze_computed_member_expression(v),
            Expression::StaticMemberExpression(v) => self.analyze_static_member_expression(v),
            Expression::PrivateFieldExpression(v) => self.analyze_private_field_expression(v),
        };

        ty
    }
}

#[allow(unused)]
impl<'a> Analyzer {
    fn analyze_function_like(&mut self, function: &Function<'a>, is_expression: bool) -> Type {
        self.enter_scope(ScopeFlags::Function, &function.scope_id);

        // Analyze parameters
        let param_types = function
            .params
            .items
            .iter()
            .map(|param| self.analyze_formal_parameter(param))
            .collect();

        // Analyze return type
        let return_type = if let Some(return_type) = &function.return_type {
            self.analyze_ts_type_annotation(return_type)
        } else {
            Type::Keyword(KeywordType::Any)
        };

        // Create function type
        let func_type = Type::Function(crate::types::FunctionType {
            params: param_types,
            return_type: Box::new(return_type),
        });

        // Analyze function body
        if let Some(body) = &function.body {
            // self.analyze_function_body(body);
        }

        self.leave_scope();

        // Declare function if it has an identifier
        if let Some(id) = &function.id {
            todo!()
        }

        func_type
    }
}

impl<'a> Analyzer {
    fn analyze_formal_parameter(
        &mut self,
        formal_parameter: &FormalParameter<'a>,
    ) -> FunctionParameterType {
        let param_type = if let Some(type_ann) = &formal_parameter.pattern.type_annotation {
            self.analyze_ts_type_annotation(type_ann)
            // TODO: analyze init
        } else {
            Type::Keyword(KeywordType::Any)
        };

        self.analyze_binding_pattern(&formal_parameter.pattern, param_type.clone());

        FunctionParameterType {
            r#type: Box::new(param_type),
            accessibility: formal_parameter.accessibility,
            readonly: formal_parameter.readonly,
            is_override: formal_parameter.r#override,
        }
    }
}

impl<'a> Analyzer {
    #[allow(clippy::unused_self)]
    fn analyze_jsdoc_nullable_type(
        &mut self,
        _jsdoc_nullable_type: &JSDocNullableType<'a>,
    ) -> Type {
        Type::Keyword(KeywordType::Any)
    }

    #[allow(clippy::unused_self)]
    fn analyze_jsdoc_non_nullable_type(
        &mut self,
        _jsdoc_non_nullable_type: &JSDocNonNullableType<'a>,
    ) -> Type {
        Type::Keyword(KeywordType::Any)
    }

    fn analyze_jsdoc_unknown_type(&mut self, _jsdoc_unknown_type: &JSDocUnknownType) -> Type {
        Type::Keyword(KeywordType::Any)
    }
}

// Literal Analysis
impl<'a> Analyzer {
    #[allow(clippy::unused_self)]
    fn analyze_boolean_literal(&mut self, boolean_literal: &BooleanLiteral) -> Type {
        Type::Literal(LiteralType::Boolean(boolean_literal.value))
    }

    #[allow(clippy::unused_self)]
    fn analyze_null_literal(&mut self, _null_literal: &NullLiteral) -> Type {
        Type::Literal(LiteralType::Null)
    }

    #[allow(clippy::unused_self)]
    fn analyze_numeric_literal(&mut self, numeric_literal: &NumericLiteral<'a>) -> Type {
        dbg!(numeric_literal.span);
        // panic!();
        Type::Literal(LiteralType::Number(numeric_literal.value))
    }

    #[allow(clippy::unused_self)]
    fn analyze_big_int_literal(&mut self, _big_int_literal: &BigIntLiteral<'a>) -> Type {
        Type::Literal(LiteralType::BigInt)
    }

    #[allow(clippy::unused_self)]
    fn analyze_reg_exp_literal(&mut self, _reg_exp_literal: &RegExpLiteral<'a>) -> Type {
        Type::Literal(LiteralType::RegExp)
    }

    #[allow(clippy::unused_self)]
    fn analyze_string_literal(&mut self, _string_literal: &StringLiteral<'a>) -> Type {
        // TODO: insert raw string inside.
        Type::Literal(LiteralType::String)
    }

    #[allow(clippy::unused_self)]
    fn analyze_template_literal(&mut self, template_literal: &TemplateLiteral<'a>) -> Type {
        for elem in &template_literal.expressions {
            self.analyze_expression(elem);
        }

        // TODO: insert raw string inside.
        Type::Literal(LiteralType::String)
    }
}

// Expression Analysis
impl<'a> Analyzer {
    // const baz = ['foo', 'bar'];
    //             ^^^^^^^^^^^^^^
    fn analyze_array_expression(&mut self, array_expression: &ArrayExpression<'a>) -> Type {
        for (elem) in &array_expression.elements {}

        todo!()
    }

    // const baz = ['foo', 'bar'];
    //              ^^^^^  ^^^^^
    fn analyze_array_expression_element(
        &mut self,
        array_expression_element: &ArrayExpressionElement<'a>,
    ) -> Type {
        match array_expression_element {
            ArrayExpressionElement::SpreadElement(v) => self.analyze_spread_element(v),
            ArrayExpressionElement::Elision(v) => Type::Keyword(KeywordType::Undefined),
            _ => self.analyze_expression(array_expression_element.to_expression()),
        }
    }

    fn analyze_spread_element(&mut self, spread_element: &SpreadElement<'a>) -> Type {
        self.analyze_expression(&spread_element.argument)
    }
    fn analyze_identifier_reference(
        &mut self,
        identifier_reference: &IdentifierReference<'a>,
    ) -> Type {
        todo!()
    }

    fn analyze_meta_property_expression(
        &mut self,
        meta_property_expression: &MetaProperty<'a>,
    ) -> Type {
        todo!()
    }

    #[allow(clippy::unused_self)]
    fn analyze_super_expression(&mut self, super_expression: &Super) -> Type {
        Type::Keyword(KeywordType::Intrinsic)
    }
    // const baz = () => {};
    //             ^^^^^^^^
    fn analyze_arrow_function_expression(
        &mut self,
        arrow_function_expression: &ArrowFunctionExpression<'a>,
    ) -> Type {
        todo!()
    }

    // baz = foo;
    // ^^^^^^^^^
    fn analyze_assignment_expression(
        &mut self,
        assignment_expression: &AssignmentExpression<'a>,
    ) -> Type {
        todo!()
    }

    #[allow(clippy::unused_self)]
    fn analyze_compound_assignment(
        &self,
        left_ty: &Type,
        right_ty: &Type,
        operator: AssignmentOperator,
    ) -> Type {
        match operator {
            AssignmentOperator::Addition => {
                if left_ty.is_subtype_of(&Type::Keyword(KeywordType::String))
                    || right_ty.is_subtype_of(&Type::Keyword(KeywordType::String))
                {
                    Type::Keyword(KeywordType::String)
                } else if left_ty.is_subtype_of(&Type::Keyword(KeywordType::Number))
                    && right_ty.is_subtype_of(&Type::Keyword(KeywordType::Number))
                {
                    Type::Keyword(KeywordType::Number)
                } else {
                    Type::Union(vec![
                        Type::Keyword(KeywordType::String),
                        Type::Keyword(KeywordType::Number),
                    ])
                }
            }
            AssignmentOperator::Subtraction
            | AssignmentOperator::Multiplication
            | AssignmentOperator::Division
            | AssignmentOperator::Remainder
            | AssignmentOperator::Exponential => {
                if left_ty.is_subtype_of(&Type::Keyword(KeywordType::Number))
                    && right_ty.is_subtype_of(&Type::Keyword(KeywordType::Number))
                {
                    Type::Keyword(KeywordType::Number)
                } else {
                    Type::Keyword(KeywordType::Any)
                }
            }
            AssignmentOperator::BitwiseAnd
            | AssignmentOperator::BitwiseOR
            | AssignmentOperator::BitwiseXOR
            | AssignmentOperator::ShiftLeft
            | AssignmentOperator::ShiftRight
            | AssignmentOperator::ShiftRightZeroFill => Type::Keyword(KeywordType::Number),
            AssignmentOperator::LogicalNullish => left_ty.clone().union(right_ty.clone()),
            AssignmentOperator::LogicalAnd | AssignmentOperator::LogicalOr => {
                left_ty.clone().union(right_ty.clone())
            }
            AssignmentOperator::Assign => unreachable!(),
        }
    }

    // baz = foo.bar;
    // ^^^
    // { baz } = foo.bar;
    // ^^^^^^^
    // [ baz ] = foo.bar;
    // ^^^^^^^
    fn analyze_assignment_target(&mut self, target: &AssignmentTarget<'a>) -> Type {
        match target {
            AssignmentTarget::ArrayAssignmentTarget(_) => Type::Keyword(KeywordType::Any),
            AssignmentTarget::ObjectAssignmentTarget(_) => Type::Keyword(KeywordType::Any),
            AssignmentTarget::AssignmentTargetIdentifier(ident) => {
                todo!()
            }
            AssignmentTarget::ComputedMemberExpression(expr) => {
                self.analyze_computed_member_expression(expr)
            }
            AssignmentTarget::StaticMemberExpression(expr) => {
                self.analyze_static_member_expression(expr)
            }
            AssignmentTarget::PrivateFieldExpression(expr) => {
                self.analyze_private_field_expression(expr)
            }
            // For TypeScript-specific targets, we'll need to add more sophisticated analysis
            AssignmentTarget::TSAsExpression(expr) => self.analyze_ts_as_expression(expr),
            AssignmentTarget::TSSatisfiesExpression(expr) => {
                self.analyze_ts_satisfies_expression(expr)
            }
            AssignmentTarget::TSNonNullExpression(expr) => {
                self.analyze_ts_non_null_expression(expr)
            }
            AssignmentTarget::TSTypeAssertion(expr) => self.analyze_ts_type_assertion(expr),
            AssignmentTarget::TSInstantiationExpression(expr) => {
                self.analyze_ts_instantiation_expression(expr)
            }
        }
    }

    // const baz = await foo;
    //             ^^^^^^^^^
    fn analyze_await_expression(&mut self, await_expression: &AwaitExpression<'a>) -> Type {
        let argument_ty = self.analyze_expression(&await_expression.argument);
        todo!()
    }

    // const baz = foo === bar;
    //             ^^^^^^^^^^^
    fn analyze_binary_expression(&mut self, binary_expression: &BinaryExpression<'a>) -> Type {
        let left_ty = self.analyze_expression(&binary_expression.left);
        let right_ty = self.analyze_expression(&binary_expression.right);

        match binary_expression.operator {
            BinaryOperator::In => Type::Keyword(KeywordType::Boolean),
            BinaryOperator::Equality => todo!(),
            BinaryOperator::Inequality => todo!(),
            BinaryOperator::StrictEquality => todo!(),
            BinaryOperator::StrictInequality => todo!(),
            BinaryOperator::LessThan => todo!(),
            BinaryOperator::LessEqualThan => todo!(),
            BinaryOperator::GreaterThan => todo!(),
            BinaryOperator::GreaterEqualThan => todo!(),
            BinaryOperator::ShiftLeft => todo!(),
            BinaryOperator::ShiftRight => todo!(),
            BinaryOperator::ShiftRightZeroFill => todo!(),
            BinaryOperator::Addition => todo!(),
            BinaryOperator::Subtraction => todo!(),
            BinaryOperator::Multiplication => todo!(),
            BinaryOperator::Division => todo!(),
            BinaryOperator::Remainder => todo!(),
            BinaryOperator::BitwiseOR => todo!(),
            BinaryOperator::BitwiseXOR => todo!(),
            BinaryOperator::BitwiseAnd => todo!(),
            BinaryOperator::Instanceof => todo!(),
            BinaryOperator::Exponential => todo!(),
        }
    }

    // const baz = foo.bar();
    //             ^^^^^^^^^
    fn analyze_call_expression(&mut self, call_expression: &CallExpression<'a>) -> Type {
        let callee_type = self.analyze_expression(&call_expression.callee);

        // TODO: analyze arguments

        let Type::Function(func_type) = callee_type else {
            return Type::Keyword(KeywordType::Any);
        };

        // TODO: handle generics

        *func_type.return_type
    }

    fn analyze_chain_expression(&mut self, chain_expression: &ChainExpression<'a>) -> Type {
        match &chain_expression.expression {
            ChainElement::CallExpression(v) => self.analyze_call_expression(v),
            ChainElement::ComputedMemberExpression(v) => self.analyze_computed_member_expression(v),
            ChainElement::StaticMemberExpression(v) => self.analyze_static_member_expression(v),
            ChainElement::PrivateFieldExpression(v) => self.analyze_private_field_expression(v),
        }
    }

    #[allow(clippy::unused_self)]
    fn analyze_class_expression(&mut self, class_expression: &Class<'a>) -> Type {
        Type::Keyword(KeywordType::Any)
    }

    // const baz = foo ? bar : baz;
    //             ^^^^^^^^^^^^^^^
    fn analyze_conditional_expression(
        &mut self,
        conditional_expression: &ConditionalExpression<'a>,
    ) -> Type {
        let _test_ty = self.analyze_expression(&conditional_expression.test);
        let consequent_ty = self.analyze_expression(&conditional_expression.consequent);
        let alternate_ty = self.analyze_expression(&conditional_expression.alternate);

        // TODO: return union type of consequent_ty and alternate_ty
        Type::Keyword(KeywordType::Any)
    }

    // const baz = function foo() {};
    //             ^^^^^^^^^^^^^^^^^
    fn analyze_function_expression(&mut self, function_expression: &Function<'a>) -> Type {
        self.analyze_function_like(function_expression, true)
    }
    // const baz = import('foo');
    //             ^^^^^^^^^^^^^
    #[allow(clippy::unused_self)]
    fn analyze_import_expression(&mut self, import_expression: &ImportExpression<'a>) -> Type {
        Type::Keyword(KeywordType::Any)
    }
    // const baz = foo in bar;
    //             ^^^^^^^^^^
    fn analyze_logical_expression(&mut self, logical_expression: &LogicalExpression<'a>) -> Type {
        let left_ty = self.analyze_expression(&logical_expression.left);
        let right_ty = self.analyze_expression(&logical_expression.right);
        Type::Keyword(KeywordType::Any)
    }

    // const baz = new foo();
    //             ^^^^^^^^^
    fn analyze_new_expression(&mut self, new_expression: &NewExpression<'a>) -> Type {
        let callee_ty = self.analyze_expression(&new_expression.callee);
        Type::Keyword(KeywordType::Any)
    }
    // const baz = { foo: bar };
    //             ^^^^^^^^^^^^
    fn analyze_object_expression(&mut self, object_expression: &ObjectExpression<'a>) -> Type {
        todo!()
    }

    fn analyze_property_key(&mut self, key: &PropertyKey<'a>, computed: bool) {
        todo!()
    }

    // const baz = (foo);
    //             ^^^^^
    fn analyze_parenthesized_expression(
        &mut self,
        parenthesized_expression: &ParenthesizedExpression<'a>,
    ) -> Type {
        self.analyze_expression(&parenthesized_expression.expression)
    }

    // const baz = (foo, bar);
    //             ^^^^^^^^^^
    fn analyze_sequence_expression(
        &mut self,
        sequence_expression: &SequenceExpression<'a>,
    ) -> Type {
        sequence_expression
            .expressions
            .iter()
            .map(|expr| self.analyze_expression(expr))
            .last()
            .expect("Sequence expression must have at least one expression")
    }
    // const baz = foo`bar`;
    //             ^^^^^^^^
    #[allow(clippy::unused_self)]
    fn analyze_tagged_template_expression(
        &mut self,
        tagged_template_expression: &TaggedTemplateExpression<'a>,
    ) -> Type {
        Type::Keyword(KeywordType::Any)
    }
    // const baz = this;
    //             ^^^^
    #[allow(clippy::unused_self)]
    fn analyze_this_expression(&mut self, this_expression: &ThisExpression) -> Type {
        Type::Keyword(KeywordType::Any)
    }
    // const baz = !foo;
    //             ^^^^^
    fn analyze_unary_expression(&mut self, unary_expression: &UnaryExpression<'a>) -> Type {
        let argument_ty = self.analyze_expression(&unary_expression.argument);

        match unary_expression.operator {
            oxc_syntax::operator::UnaryOperator::UnaryNegation
            | oxc_syntax::operator::UnaryOperator::UnaryPlus => {
                // TODO: actually compute the value if possible
                Type::Keyword(KeywordType::Number)
            }
            oxc_syntax::operator::UnaryOperator::LogicalNot => {
                // TODO: try to negate the value if possible
                // ex: !true -> false
                //     1.0 -> false
                //     0.0 -> true
                //     '' -> true
                //     'foo' -> false
                Type::Keyword(KeywordType::Boolean)
            }
            oxc_syntax::operator::UnaryOperator::BitwiseNot => Type::Keyword(KeywordType::Number),
            oxc_syntax::operator::UnaryOperator::Typeof => {
                // TODO: string union of `number`, `string`, `boolean`, `symbol`, `undefined`, `object`, `function` string literals
                Type::Keyword(KeywordType::String)
            }
            oxc_syntax::operator::UnaryOperator::Void => Type::Keyword(KeywordType::Undefined),
            oxc_syntax::operator::UnaryOperator::Delete => Type::Keyword(KeywordType::Boolean),
        }
    }

    // const baz = foo++;
    //             ^^^^^
    fn analyze_update_expression(&mut self, update_expression: &UpdateExpression<'a>) -> Type {
        let _ = self.analyze_simple_assignment_target(&update_expression.argument);
        Type::Keyword(KeywordType::Number)
    }

    fn analyze_simple_assignment_target(
        &mut self,
        simple_assignment_target: &SimpleAssignmentTarget<'a>,
    ) -> Type {
        match simple_assignment_target {
            SimpleAssignmentTarget::AssignmentTargetIdentifier(assignment_target_identifier) => {
                self.analyze_identifier_reference(assignment_target_identifier)
            }
            SimpleAssignmentTarget::TSAsExpression(ts_as_expression) => {
                self.analyze_ts_as_expression(ts_as_expression)
            }
            SimpleAssignmentTarget::TSSatisfiesExpression(ts_satisfies_expression) => {
                self.analyze_ts_satisfies_expression(ts_satisfies_expression)
            }
            SimpleAssignmentTarget::TSNonNullExpression(ts_non_null_expression) => {
                self.analyze_ts_non_null_expression(ts_non_null_expression)
            }
            SimpleAssignmentTarget::TSTypeAssertion(ts_type_assertion) => {
                self.analyze_ts_type_assertion(ts_type_assertion)
            }
            SimpleAssignmentTarget::TSInstantiationExpression(ts_instantiation_expression) => {
                self.analyze_ts_instantiation_expression(ts_instantiation_expression)
            }
            SimpleAssignmentTarget::ComputedMemberExpression(computed_member_expression) => {
                self.analyze_computed_member_expression(computed_member_expression)
            }
            SimpleAssignmentTarget::StaticMemberExpression(static_member_expression) => {
                self.analyze_static_member_expression(static_member_expression)
            }
            SimpleAssignmentTarget::PrivateFieldExpression(private_field_expression) => {
                self.analyze_private_field_expression(private_field_expression)
            }
        }
    }
    // const baz = yield foo;
    //             ^^^^^^^^^
    fn analyze_yield_expression(&mut self, yield_expression: &YieldExpression<'a>) -> Type {
        if let Some(argument) = &yield_expression.argument {
            let _ty = self.analyze_expression(argument);
            // TODO: check we are inside a generator function
            // TODO: if the return type of the function is explicit, check that this type is assignable to it
            // TODO: if the return type of the function is implicit, push this type onto a list of possible return types
        }

        Type::Keyword(KeywordType::Any)
    }

    // const baz = #foo in bar;
    //             ^^^^^^^^^^^
    #[allow(clippy::unused_self)]
    fn analyze_private_in_expression(
        &mut self,
        private_in_expression: &PrivateInExpression<'a>,
    ) -> Type {
        Type::Keyword(KeywordType::Boolean)
    }

    // const baz5 = foo['bar'];
    //              ^^^^^^^^^^
    #[allow(clippy::unused_self)]
    fn analyze_computed_member_expression(
        &mut self,
        computed_member_expression: &ComputedMemberExpression<'a>,
    ) -> Type {
        let object_ty = self.analyze_expression(&computed_member_expression.object);
        let property_ty = self.analyze_expression(&computed_member_expression.expression);

        // TODO: index into object_ty with property_ty

        Type::Keyword(KeywordType::Any)
    }

    // const baz6 = foo.bar;
    //              ^^^^^^^
    #[allow(clippy::unused_self)]
    fn analyze_static_member_expression(
        &mut self,
        static_member_expression: &StaticMemberExpression<'a>,
    ) -> Type {
        let object_ty = self.analyze_expression(&static_member_expression.object);

        let property_name = static_member_expression.property.name.to_string();

        match object_ty {
            Type::Object(object_ty) => {
                if let Some(prop_type) = object_ty.properties.get(&property_name) {
                    return prop_type.clone();
                }

                // If not found in properties, check if it's a method call
                // for call_sig in &object_ty.call_signatures {
                //     if call_sig.name == property_name {
                //         return Type::Function(call_sig.clone());
                //     }
                // }

                // If not found in call signatures, check constructor signatures
                // for construct_sig in &object_ty.construct_signatures {
                //     if construct_sig.name == property_name {
                //         return Type::Function(construct_sig.clone());
                //     }
                // }

                // If still not found, check index signatures
                for index_sig in &object_ty.index_signatures {
                    if index_sig.parameter_types[0]
                        .is_subtype_of(&Type::Keyword(KeywordType::String))
                    {
                        return index_sig.return_type.clone();
                    }
                }

                return Type::Keyword(KeywordType::Any);
            }
            Type::Keyword(object_ty) => {
                todo!();
            }
            Type::Union(object_ty) => Type::Union(
                object_ty
                    .iter()
                    .map(|ty| self.analyze_static_member_expression_on_type(ty, &property_name))
                    .collect(),
            )
            .flatten_union(),
            Type::Array(object_ty) => {
                todo!();
            }
            _ => Type::Keyword(KeywordType::Any),
        }
    }

    fn analyze_static_member_expression_on_type(
        &mut self,
        r#type: &Type,
        property_name: &str,
    ) -> Type {
        match r#type {
            Type::Object(obj_type) => {
                obj_type.properties.get(property_name).cloned().unwrap_or_else(|| {
                    // Check index signatures if the property is not found
                    for index_sig in &obj_type.index_signatures {
                        if index_sig.parameter_types[0]
                            .is_subtype_of(&Type::Keyword(KeywordType::String))
                        {
                            return index_sig.return_type.clone();
                        }
                    }
                    Type::Keyword(KeywordType::Any)
                })
            }
            Type::Keyword(KeywordType::Any) => Type::Keyword(KeywordType::Any),
            // Add more cases for other types as needed
            _ => Type::Keyword(KeywordType::Any),
        }
    }

    // const baz7 = foo.#bar;
    //              ^^^^^^^^
    #[allow(clippy::unused_self)]
    fn analyze_private_field_expression(
        &mut self,
        private_field_expression: &PrivateFieldExpression<'a>,
    ) -> Type {
        Type::Keyword(KeywordType::Any)
    }
}

// JSX/TSX specific analysis
impl<'a> Analyzer {
    // const baz = <div />;
    //             ^^^^^^^
    #[allow(clippy::unused_self)]
    fn analyze_jsx_element(&mut self, jsx_element_expression: &JSXElement<'a>) -> Type {
        // TODO: opening

        for child in &jsx_element_expression.children {
            self.analyze_jsx_child(child);
        }

        // TODO: closing

        Type::Keyword(KeywordType::Any)
    }

    fn analyze_jsx_child(&mut self, jsx_child: &JSXChild<'a>) {
        match jsx_child {
            JSXChild::Text(text) => {
                // noop
            }
            JSXChild::Element(element) => {
                self.analyze_jsx_element(element);
            }
            JSXChild::Fragment(fragment) => {
                let _ = self.analyze_jsx_fragment(fragment);
            }
            JSXChild::ExpressionContainer(expression) => {
                if let Some(expression) = expression.expression.as_expression() {
                    self.analyze_expression(expression);
                }
            }
            JSXChild::Spread(spread) => {
                let _ = self.analyze_expression(&spread.expression);
            }
        }
    }

    // const baz = <>{children}</>;
    //             ^^^^^^^^^^^^^^^
    #[allow(clippy::unused_self)]
    fn analyze_jsx_fragment(&mut self, jsx_fragment_expression: &JSXFragment<'a>) -> Type {
        for child in &jsx_fragment_expression.children {
            self.analyze_jsx_child(child);
        }

        Type::Keyword(KeywordType::Any)
    }
}

// Typescript specific analysis
impl<'a> Analyzer {
    // const baz0 = foo as string;
    //              ^^^^^^^^^^^^^^
    #[allow(clippy::let_and_return)]
    fn analyze_ts_as_expression(&mut self, ts_as_expression: &TSAsExpression<'a>) -> Type {
        let _ = self.analyze_expression(&ts_as_expression.expression);
        let expected_ty = self.analyze_ts_type(&ts_as_expression.type_annotation);

        expected_ty
    }

    // const baz1 = "hello" satisfies string;
    //               ^^^^^^^^^^^^^^^^^^^^^^^
    #[allow(clippy::unused_self)]
    fn analyze_ts_satisfies_expression(
        &mut self,
        ts_satisfies_expression: &TSSatisfiesExpression<'a>,
    ) -> Type {
        Type::Keyword(KeywordType::Any)
    }

    // const baz2 = <string>''
    //              ^^^^^^^^^^
    fn analyze_ts_type_assertion(
        &mut self,
        ts_type_assertion_expression: &TSTypeAssertion<'a>,
    ) -> Type {
        // TODO!

        Type::Keyword(KeywordType::Any)
    }

    // const baz3 = foo!
    //              ^^^^
    #[allow(clippy::let_and_return)]
    fn analyze_ts_non_null_expression(
        &mut self,
        ts_non_null_expression: &TSNonNullExpression<'a>,
    ) -> Type {
        let expression_ty = self.analyze_expression(&ts_non_null_expression.expression);

        // TODO: check expression_ty is not null or undefined, if it is, remove those types from expression_ty
        expression_ty
    }

    // const baz4 = foo<Bar>;
    //              ^^^^^^^^
    #[allow(clippy::unused_self)]
    fn analyze_ts_instantiation_expression(
        &mut self,
        ts_instantiation_expression: &TSInstantiationExpression<'a>,
    ) -> Type {
        // TODO
        Type::Keyword(KeywordType::Any)
    }

    fn analyze_ts_type_annotation(&mut self, ts_type_annotation: &TSTypeAnnotation<'a>) -> Type {
        self.analyze_ts_type(&ts_type_annotation.type_annotation)
    }

    fn analyze_ts_type(&mut self, ty: &TSType<'a>) -> Type {
        match ty {
            TSType::TSAnyKeyword(v) => self.analyze_ts_any_keyword(v),
            TSType::TSBigIntKeyword(v) => self.analyze_ts_big_int_keyword(v),
            TSType::TSBooleanKeyword(v) => self.analyze_ts_boolean_keyword(v),
            TSType::TSIntrinsicKeyword(v) => self.analyze_ts_intrinsic_keyword(v),
            TSType::TSNeverKeyword(v) => self.analyze_ts_never_keyword(v),
            TSType::TSNullKeyword(v) => self.analyze_ts_null_keyword(v),
            TSType::TSNumberKeyword(v) => self.analyze_ts_number_keyword(v),
            TSType::TSObjectKeyword(v) => self.analyze_ts_object_keyword(v),
            TSType::TSStringKeyword(v) => self.analyze_ts_string_keyword(v),
            TSType::TSSymbolKeyword(v) => self.analyze_ts_symbol_keyword(v),
            TSType::TSUndefinedKeyword(v) => self.analyze_ts_undefined_keyword(v),
            TSType::TSUnknownKeyword(v) => self.analyze_ts_unknown_keyword(v),
            TSType::TSVoidKeyword(v) => self.analyze_ts_void_keyword(v),
            TSType::TSArrayType(v) => self.analyze_ts_array_type(v),
            TSType::TSConditionalType(v) => self.analyze_ts_conditional_type(v),
            TSType::TSConstructorType(v) => self.analyze_ts_constructor_type(v),
            TSType::TSFunctionType(v) => self.analyze_ts_function_type(v),
            TSType::TSImportType(v) => self.analyze_ts_import_type(v),
            TSType::TSIndexedAccessType(v) => self.analyze_ts_indexed_access_type(v),
            TSType::TSInferType(v) => self.analyze_ts_infer_type(v),
            TSType::TSIntersectionType(v) => self.analyze_ts_intersection_type(v),
            TSType::TSLiteralType(v) => self.analyze_ts_literal_type(v),
            TSType::TSMappedType(v) => self.analyze_ts_mapped_type(v),
            TSType::TSNamedTupleMember(v) => self.analyze_ts_named_tuple_member(v),
            TSType::TSQualifiedName(v) => self.analyze_ts_qualified_name(v),
            TSType::TSTemplateLiteralType(v) => self.analyze_ts_template_literal_type(v),
            TSType::TSThisType(v) => self.analyze_ts_this_type(v),
            TSType::TSTupleType(v) => self.analyze_ts_tuple_type(v),
            TSType::TSTypeLiteral(v) => self.analyze_ts_type_literal(v),
            TSType::TSTypeOperatorType(v) => self.analyze_ts_type_operator_type(v),
            TSType::TSTypePredicate(v) => self.analyze_ts_type_predicate(v),
            TSType::TSTypeQuery(v) => self.analyze_ts_type_query(v),
            TSType::TSTypeReference(v) => self.analyze_ts_type_reference(v),
            TSType::TSUnionType(v) => self.analyze_ts_union_type(v),
            TSType::TSParenthesizedType(v) => self.analyze_ts_parenthesized_type(v),
            TSType::JSDocNullableType(v) => self.analyze_jsdoc_nullable_type(v),
            TSType::JSDocNonNullableType(v) => self.analyze_jsdoc_non_nullable_type(v),
            TSType::JSDocUnknownType(v) => self.analyze_jsdoc_unknown_type(v),
        }
    }

    #[allow(clippy::unused_self)]
    fn analyze_ts_any_keyword(&self, ts_any_keyword: &TSAnyKeyword) -> Type {
        Type::Keyword(KeywordType::Any)
    }

    #[allow(clippy::unused_self)]
    fn analyze_ts_big_int_keyword(&self, ts_big_int_keyword: &TSBigIntKeyword) -> Type {
        Type::Keyword(KeywordType::BigInt)
    }

    #[allow(clippy::unused_self)]
    fn analyze_ts_boolean_keyword(&self, ts_boolean_keyword: &TSBooleanKeyword) -> Type {
        Type::Keyword(KeywordType::Boolean)
    }

    #[allow(clippy::unused_self)]
    fn analyze_ts_intrinsic_keyword(&self, ts_intrinsic_keyword: &TSIntrinsicKeyword) -> Type {
        Type::Keyword(KeywordType::Intrinsic)
    }

    #[allow(clippy::unused_self)]
    fn analyze_ts_never_keyword(&self, ts_never_keyword: &TSNeverKeyword) -> Type {
        Type::Keyword(KeywordType::Never)
    }

    #[allow(clippy::unused_self)]
    fn analyze_ts_null_keyword(&self, ts_null_keyword: &TSNullKeyword) -> Type {
        Type::Keyword(KeywordType::Null)
    }

    #[allow(clippy::unused_self)]
    fn analyze_ts_number_keyword(&self, ts_number_keyword: &TSNumberKeyword) -> Type {
        Type::Keyword(KeywordType::Number)
    }

    #[allow(clippy::unused_self)]
    fn analyze_ts_object_keyword(&self, ts_object_keyword: &TSObjectKeyword) -> Type {
        Type::Keyword(KeywordType::Object)
    }

    #[allow(clippy::unused_self)]
    fn analyze_ts_string_keyword(&self, ts_string_keyword: &TSStringKeyword) -> Type {
        Type::Keyword(KeywordType::String)
    }

    #[allow(clippy::unused_self)]
    fn analyze_ts_symbol_keyword(&self, ts_symbol_keyword: &TSSymbolKeyword) -> Type {
        Type::Keyword(KeywordType::Symbol)
    }

    #[allow(clippy::unused_self)]
    fn analyze_ts_undefined_keyword(&self, ts_undefined_keyword: &TSUndefinedKeyword) -> Type {
        Type::Keyword(KeywordType::Undefined)
    }

    #[allow(clippy::unused_self)]
    fn analyze_ts_unknown_keyword(&self, ts_unknown_keyword: &TSUnknownKeyword) -> Type {
        Type::Keyword(KeywordType::Unknown)
    }

    #[allow(clippy::unused_self)]
    fn analyze_ts_void_keyword(&self, ts_void_keyword: &TSVoidKeyword) -> Type {
        Type::Keyword(KeywordType::Void)
    }

    fn analyze_ts_array_type(&mut self, ts_array_type: &TSArrayType<'a>) -> Type {
        let elem_ty = self.analyze_ts_type(&ts_array_type.element_type);
        Type::Array(Box::new(elem_ty))
    }

    fn analyze_ts_conditional_type(&mut self, ts_conditional_type: &TSConditionalType<'a>) -> Type {
        let check_type = self.analyze_ts_type(&ts_conditional_type.check_type);
        let extends_type = self.analyze_ts_type(&ts_conditional_type.extends_type);
        let true_type = self.analyze_ts_type(&ts_conditional_type.true_type);
        let false_type = self.analyze_ts_type(&ts_conditional_type.false_type);
        Type::Keyword(KeywordType::Any)
    }

    fn analyze_ts_constructor_type(&mut self, ts_constructor_type: &TSConstructorType<'a>) -> Type {
        let param_types: Vec<FunctionParameterType> = ts_constructor_type
            .params
            .items
            .iter()
            .map(|param| self.analyze_formal_parameter(param))
            .collect();

        let return_type =
            Box::new(self.analyze_ts_type_annotation(&ts_constructor_type.return_type));

        // TODO: type params

        Type::Keyword(KeywordType::Any)
    }

    fn analyze_ts_function_type(&mut self, ts_function_type: &TSFunctionType<'a>) -> Type {
        // TODO: handle `this`
        let param_types = ts_function_type
            .params
            .items
            .iter()
            .map(|param| self.analyze_formal_parameter(param))
            .collect();

        let return_type = Box::new(self.analyze_ts_type_annotation(&ts_function_type.return_type));

        // TODO: type params

        Type::Function(crate::types::FunctionType { params: param_types, return_type })
    }

    fn analyze_ts_import_type(&mut self, ts_import_type: &TSImportType<'a>) -> Type {
        todo!()
    }

    // const baz = foo['bar'];
    //             ^^^^^^^^^^
    fn analyze_ts_indexed_access_type(
        &mut self,
        ts_indexed_access_type: &TSIndexedAccessType<'a>,
    ) -> Type {
        let object_ty = self.analyze_ts_type(&ts_indexed_access_type.object_type);
        let index_ty = self.analyze_ts_type(&ts_indexed_access_type.index_type);
        // dbg!(ts_indexed_access_type);
        Type::Keyword(KeywordType::Any)
    }

    fn analyze_ts_infer_type(&mut self, ts_infer_type: &TSInferType<'a>) -> Type {
        Type::Keyword(KeywordType::Any)
    }

    fn analyze_ts_intersection_type(
        &mut self,
        ts_intersection_type: &TSIntersectionType<'a>,
    ) -> Type {
        let tys: Vec<Type> =
            ts_intersection_type.types.iter().map(|ty| self.analyze_ts_type(ty)).collect();

        Type::Keyword(KeywordType::Any)
    }

    fn analyze_ts_literal_type(&mut self, ts_literal_type: &TSLiteralType<'a>) -> Type {
        match &ts_literal_type.literal {
            TSLiteral::BooleanLiteral(v) => self.analyze_boolean_literal(v),
            TSLiteral::NullLiteral(v) => self.analyze_null_literal(v),
            TSLiteral::NumericLiteral(v) => self.analyze_numeric_literal(v),
            TSLiteral::BigIntLiteral(v) => self.analyze_big_int_literal(v),
            TSLiteral::RegExpLiteral(v) => self.analyze_reg_exp_literal(v),
            TSLiteral::StringLiteral(v) => self.analyze_string_literal(v),
            TSLiteral::TemplateLiteral(v) => self.analyze_template_literal(v),
            TSLiteral::UnaryExpression(v) => self.analyze_unary_expression(v),
        }
    }

    //
    fn analyze_ts_mapped_type(&mut self, ts_mapped_type: &TSMappedType<'a>) -> Type {
        Type::Keyword(KeywordType::Any)
    }

    fn analyze_ts_named_tuple_member(
        &mut self,
        ts_named_tuple_member: &TSNamedTupleMember<'a>,
    ) -> Type {
        todo!()
    }

    fn analyze_ts_qualified_name(&mut self, ts_qualified_name: &TSQualifiedName<'a>) -> Type {
        let left = match &ts_qualified_name.left {
            TSTypeName::IdentifierReference(ident) => {
                todo!();
                // TODO: get the type of the identifier
                Type::Keyword(KeywordType::Any)
            }
            TSTypeName::QualifiedName(qualified_name) => {
                self.analyze_ts_qualified_name(&qualified_name)
            }
        };

        let right = ts_qualified_name.right.name.to_string();

        // TODO: index into left with right

        Type::Keyword(KeywordType::Any)
    }

    #[allow(clippy::unused_self)]
    fn analyze_ts_template_literal_type(
        &mut self,
        _ts_template_literal_type: &TSTemplateLiteralType<'a>,
    ) -> Type {
        // TODO: make it proper
        Type::Literal(LiteralType::String)
    }

    #[allow(clippy::unused_self)]
    fn analyze_ts_this_type(&mut self, ts_this_type: &TSThisType) -> Type {
        Type::Keyword(KeywordType::Any)
    }

    fn analyze_ts_tuple_type(&mut self, ts_tuple_type: &TSTupleType<'a>) -> Type {
        let elem_types: Vec<Type> = ts_tuple_type
            .element_types
            .iter()
            .map(|elem_ty| self.analyze_ts_tuple_element_type(elem_ty))
            .collect();

        Type::Tuple(TupleType { members: elem_types })
    }

    fn analyze_ts_tuple_element_type(&mut self, ts_tuple_element: &TSTupleElement<'a>) -> Type {
        Type::Keyword(KeywordType::Any)
    }

    fn analyze_ts_type_literal(&mut self, ts_type_literal: &TSTypeLiteral<'a>) -> Type {
        let properties = self.analyze_type_members(&ts_type_literal.members);
        Type::Object(properties)
    }

    fn analyze_type_members(&mut self, members: &[TSSignature<'a>]) -> ObjectType {
        let mut object_type = ObjectType {
            properties: HashMap::new(),
            index_signatures: Vec::new(),
            call_signatures: Vec::new(),
            construct_signatures: Vec::new(),
        };

        for member in members {
            match member {
                TSSignature::TSPropertySignature(prop_sig) => {
                    // dbg!(prop_sig);

                    match &prop_sig.key {
                        PropertyKey::StaticIdentifier(static_ident) => {
                            let prop_type = if let Some(ty_annotation) = &prop_sig.type_annotation {
                                self.analyze_ts_type_annotation(&ty_annotation)
                            } else {
                                Type::Keyword(KeywordType::Any)
                            };
                            object_type.properties.insert(static_ident.name.to_string(), prop_type);
                        }
                        PropertyKey::StaticMemberExpression(static_member_expr) => {
                            // tod
                            dbg!(&static_member_expr);
                            // panic!();
                        }
                        PropertyKey::StringLiteral(string_lit) => {
                            let prop_type = if let Some(ty_annotation) = &prop_sig.type_annotation {
                                self.analyze_ts_type_annotation(&ty_annotation)
                            } else {
                                Type::Keyword(KeywordType::Any)
                            };
                            object_type.properties.insert(string_lit.value.to_string(), prop_type);
                        }
                        PropertyKey::NumericLiteral(numeric_lit) => {
                            let prop_type = if let Some(ty_annotation) = &prop_sig.type_annotation {
                                self.analyze_ts_type_annotation(&ty_annotation)
                            } else {
                                Type::Keyword(KeywordType::Any)
                            };
                            object_type.properties.insert(numeric_lit.value.to_string(), prop_type);
                        }
                        _ => {
                            dbg!(prop_sig);
                            panic!();
                            //todo
                        }
                    }

                    // TODO: handle computed property names
                }

                TSSignature::TSMethodSignature(method_sig) => {
                    // dbg!(&method_sig.key);

                    match &method_sig.key {
                        PropertyKey::StaticIdentifier(static_ident) => {
                            let method_type = self.analyze_ts_method_signature(method_sig);
                            object_type
                                .properties
                                .insert(static_ident.name.to_string(), method_type);
                        }
                        // typically { [Symbol.iterator](): Iterator<T> }
                        PropertyKey::StaticMemberExpression(static_member_expr) => {
                            dbg!(static_member_expr);
                            // todo
                        }
                        _ => {
                            dbg!(&method_sig.key);
                            panic!();
                        }
                    }

                    // Note: We're not handling computed method names here
                }
                TSSignature::TSIndexSignature(index_sig) => {
                    let parameter_types = index_sig
                        .parameters
                        .iter()
                        .map(|param| self.analyze_ts_index_signature_name(param))
                        .collect::<Vec<_>>();
                    let return_type =
                        self.analyze_ts_type(&index_sig.type_annotation.type_annotation);
                    object_type.index_signatures.push(IndexSignature {
                        parameter_types,
                        return_type,
                        readonly: index_sig.readonly,
                    });
                }
                TSSignature::TSCallSignatureDeclaration(call_sig) => {
                    let func_type = self.analyze_ts_call_signature(call_sig);
                    object_type.call_signatures.push(func_type);
                }
                TSSignature::TSConstructSignatureDeclaration(construct_sig) => {
                    let construct_type = self.analyze_ts_construct_signature(construct_sig);
                    object_type.construct_signatures.push(construct_type);
                }
            }
        }

        object_type
    }

    fn analyze_ts_method_signature(&mut self, ts_method_signature: &TSMethodSignature<'a>) -> Type {
        let params = ts_method_signature
            .params
            .items
            .iter()
            .map(|param| self.analyze_formal_parameter(param))
            .collect();

        let return_type = if let Some(return_type) = &ts_method_signature.return_type {
            Box::new(self.analyze_ts_type(&return_type.type_annotation))
        } else {
            Box::new(Type::Keyword(KeywordType::Any))
        };

        Type::Function(crate::types::FunctionType { params, return_type })
    }
    fn analyze_ts_index_signature_name(
        &mut self,
        ts_method_signature: &TSIndexSignatureName<'a>,
    ) -> Type {
        self.analyze_ts_type_annotation(&ts_method_signature.type_annotation)
    }

    fn analyze_ts_call_signature(
        &mut self,
        ts_call_signature_declaration: &TSCallSignatureDeclaration,
    ) -> crate::types::FunctionType {
        // TODO: this param

        let params = ts_call_signature_declaration
            .params
            .items
            .iter()
            .map(|param| self.analyze_formal_parameter(param))
            .collect();

        let return_type = if let Some(return_type) = &ts_call_signature_declaration.return_type {
            Box::new(self.analyze_ts_type(&return_type.type_annotation))
        } else {
            Box::new(Type::Keyword(KeywordType::Any))
        };

        // TODO: type parameters

        crate::types::FunctionType { params, return_type }
    }

    fn analyze_ts_construct_signature(
        &mut self,
        construct_sig: &TSConstructSignatureDeclaration<'a>,
    ) -> crate::types::FunctionType {
        let params: Vec<_> = construct_sig
            .params
            .items
            .iter()
            .map(|param| self.analyze_formal_parameter(param))
            .collect();

        let return_type = if let Some(return_type) = &construct_sig.return_type {
            Box::new(self.analyze_ts_type(&return_type.type_annotation))
        } else {
            Box::new(Type::Keyword(KeywordType::Any))
        };

        // let type_parameters = construct_sig
        //     .type_parameters
        //     .as_ref()
        //     .map(|type_params| self.analyze_ts_type_parameter_declaration(type_params));

        crate::types::FunctionType {
            // this_param: None,  // Construct signatures don't have a 'this' parameter
            params,
            return_type,
            // type_parameters,
        }
    }

    // type kk = keyof Foo;
    //           ^^^^^^^^^;
    // type kk = readonly Foo[];
    //           ^^^^^^^^^;
    // const l: unique symbol = Symbol();
    //          ^^^^^^^^^^^^^
    fn analyze_ts_type_operator_type(&mut self, ts_type_operator_type: &TSTypeOperator) -> Type {
        Type::Keyword(KeywordType::Any)
    }

    // let l = (v): v is string => v
    //              ^^^^^^^^^^^
    // let l = (v): asserts v is string => v
    //              ^^^^^^^^^^^^^^^^^^^
    fn analyze_ts_type_predicate(&mut self, ts_type_predicate: &TSTypePredicate<'a>) -> Type {
        // dbg!(ts_type_predicate);
        Type::Keyword(KeywordType::Any)
    }

    // const baz = typeof foo;
    //             ^^^^^^^^^^
    fn analyze_ts_type_query(&mut self, ts_type_query: &TSTypeQuery<'a>) -> Type {
        Type::Keyword(KeywordType::Any)
    }

    // type Dogs = foo;
    //             ^^^
    fn analyze_ts_type_reference(&mut self, ts_type_reference: &TSTypeReference<'a>) -> Type {
        // let id = if let TSTypeName::IdentifierReference(i) = &ts_type_reference.type_name {
        //     todo!()
        // } else {
        //     None
        // };

        // if let Some(id) = id {
        //     todo!()
        // } else {
        Type::Keyword(KeywordType::Any)
        // }
    }

    // type t = string | number;
    //          ^^^^^^^^^^^^^^^
    fn analyze_ts_union_type(&mut self, ts_union_type: &TSUnionType<'a>) -> Type {
        let tys: Vec<Type> =
            ts_union_type.types.iter().map(|ty| self.analyze_ts_type(ty)).collect();

        Type::Union(tys)
    }

    // type t = (string);
    //          ^^^^^^^^
    fn analyze_ts_parenthesized_type(
        &mut self,
        ts_parenthesized_type: &TSParenthesizedType<'a>,
    ) -> Type {
        self.analyze_ts_type(&ts_parenthesized_type.type_annotation)
    }

    #[allow(clippy::unused_self)]
    fn analyze_ts_type_alias_declaration(
        &mut self,
        ts_type_alias_declaration: &TSTypeAliasDeclaration<'a>,
    ) {
        let ty_annotation = self.analyze_ts_type(&ts_type_alias_declaration.type_annotation);

        todo!()
    }

    fn analyze_ts_interface_declaration(
        &mut self,
        ts_interface_declaration: &TSInterfaceDeclaration<'a>,
    ) {
        // dbg!(ts_interface_declaration);
        let properties = self.analyze_type_members(&ts_interface_declaration.body.body);

        if let Some(extends) = &ts_interface_declaration.extends {
            for extends in extends {
                // TODO
            }
        }

        todo!()
    }

    fn analyze_ts_signature(&mut self, ts_signature: &TSSignature<'a>) {
        match ts_signature {
            TSSignature::TSIndexSignature(ts_index_signature) => todo!(),
            TSSignature::TSPropertySignature(ts_property_signature) => todo!(),
            TSSignature::TSCallSignatureDeclaration(ts_call_signature_declaration) => todo!(),
            TSSignature::TSConstructSignatureDeclaration(ts_construct_signature_declaration) => {
                todo!()
            }
            TSSignature::TSMethodSignature(ts_method_signature) => todo!(),
        }
    }

    #[allow(clippy::unused_self)]
    fn analyze_ts_enum_declaration(&mut self, ts_enum_declaration: &TSEnumDeclaration<'a>) {
        // TODO
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[allow(clippy::needless_raw_string_hashes)]
    fn test_analyzer() {
        let path = "foo.tsx";
        let source_text = r#"
            type Test = {
                foo: Bar;
            }
            type Bar = any;
        "#;
        let source_type = SourceType::from_path(path).unwrap();
        let allocator = Allocator::default();
        let parser_return = Parser::new(&allocator, source_text, source_type).parse();
        assert!(parser_return.errors.is_empty());

        let program = allocator.alloc(parser_return.program);
        let mut analyzer = Analyzer::new();

        analyzer.analyze_program(program);
    }
}

// here is the plan
// pass 1 - do resolution things
// pass 2 - do type things
