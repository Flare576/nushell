use crate::errors::ShellError;
#[cfg(not(coloring_in_tokens))]
use crate::parser::hir::syntax_shape::FlatShape;
use crate::parser::{
    hir::syntax_shape::{
        color_syntax, expand_atom, expand_expr, expand_syntax, AtomicToken, ColorSyntax,
        ExpandContext, ExpandExpression, ExpandSyntax, ExpansionRule, MaybeSpaceShape,
    },
    hir::Expression,
    TokensIterator,
};
use crate::{Span, Spanned};

#[derive(Debug, Copy, Clone)]
pub struct ExternalTokensShape;

impl ExpandSyntax for ExternalTokensShape {
    type Output = Vec<Spanned<String>>;

    fn expand_syntax<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
    ) -> Result<Self::Output, ShellError> {
        let mut out: Vec<Spanned<String>> = vec![];

        loop {
            match expand_syntax(&ExternalExpressionShape, token_nodes, context) {
                Err(_) | Ok(None) => break,
                Ok(Some(span)) => out.push(span.spanned_string(context.source())),
            }
        }

        Ok(out)
    }
}

#[cfg(not(coloring_in_tokens))]
impl ColorSyntax for ExternalTokensShape {
    type Info = ();
    type Input = ();

    fn color_syntax<'a, 'b>(
        &self,
        _input: &(),
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
        shapes: &mut Vec<Spanned<FlatShape>>,
    ) -> Self::Info {
        loop {
            // Allow a space
            color_syntax(&MaybeSpaceShape, token_nodes, context, shapes);

            // Process an external expression. External expressions are mostly words, with a
            // few exceptions (like $variables and path expansion rules)
            match color_syntax(&ExternalExpression, token_nodes, context, shapes).1 {
                ExternalExpressionResult::Eof => break,
                ExternalExpressionResult::Processed => continue,
            }
        }
    }
}

#[cfg(coloring_in_tokens)]
impl ColorSyntax for ExternalTokensShape {
    type Info = ();
    type Input = ();

    fn name(&self) -> &'static str {
        "ExternalTokensShape"
    }

    fn color_syntax<'a, 'b>(
        &self,
        _input: &(),
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
    ) -> Self::Info {
        loop {
            // Allow a space
            color_syntax(&MaybeSpaceShape, token_nodes, context);

            // Process an external expression. External expressions are mostly words, with a
            // few exceptions (like $variables and path expansion rules)
            match color_syntax(&ExternalExpression, token_nodes, context).1 {
                ExternalExpressionResult::Eof => break,
                ExternalExpressionResult::Processed => continue,
            }
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct ExternalExpressionShape;

impl ExpandSyntax for ExternalExpressionShape {
    type Output = Option<Span>;

    fn expand_syntax<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
    ) -> Result<Self::Output, ShellError> {
        expand_syntax(&MaybeSpaceShape, token_nodes, context)?;

        let first = expand_atom(
            token_nodes,
            "external command",
            context,
            ExpansionRule::new().allow_external_command(),
        )?
        .span;

        let mut last = first;

        loop {
            let continuation = expand_expr(&ExternalContinuationShape, token_nodes, context);

            if let Ok(continuation) = continuation {
                last = continuation.span;
            } else {
                break;
            }
        }

        Ok(Some(first.until(last)))
    }
}

#[derive(Debug, Copy, Clone)]
struct ExternalExpression;

impl ExpandSyntax for ExternalExpression {
    type Output = Option<Span>;

    fn expand_syntax<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
    ) -> Result<Self::Output, ShellError> {
        expand_syntax(&MaybeSpaceShape, token_nodes, context)?;

        let first = expand_syntax(&ExternalHeadShape, token_nodes, context)?.span;
        let mut last = first;

        loop {
            let continuation = expand_syntax(&ExternalContinuationShape, token_nodes, context);

            if let Ok(continuation) = continuation {
                last = continuation.span;
            } else {
                break;
            }
        }

        Ok(Some(first.until(last)))
    }
}

#[derive(Debug, Copy, Clone)]
struct ExternalHeadShape;

impl ExpandExpression for ExternalHeadShape {
    fn expand_expr<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
    ) -> Result<Expression, ShellError> {
        match expand_atom(
            token_nodes,
            "external argument",
            context,
            ExpansionRule::new()
                .allow_external_word()
                .treat_size_as_word()
                .reject_whitespace(),
        )? {
            atom => match &atom {
                Spanned { item, span } => Ok(match item {
                    AtomicToken::Eof { .. } => unreachable!("ExpansionRule doesn't allow EOF"),
                    AtomicToken::Error { .. } => unreachable!("ExpansionRule doesn't allow Error"),
                    AtomicToken::Size { .. } => unreachable!("ExpansionRule treats size as word"),
                    AtomicToken::Whitespace { .. } => {
                        unreachable!("ExpansionRule doesn't allow Whitespace")
                    }
                    AtomicToken::ExternalCommand { command } => {
                        Expression::external_command(*command, *span)
                    }
                    AtomicToken::Number { number } => {
                        Expression::number(number.to_number(context.source()), *span)
                    }
                    AtomicToken::ShorthandFlag { .. }
                    | AtomicToken::LonghandFlag { .. }
                    | AtomicToken::SquareDelimited { .. }
                    | AtomicToken::ParenDelimited { .. }
                    | AtomicToken::BraceDelimited { .. }
                    | AtomicToken::Pipeline { .. } => {
                        return Err(ShellError::type_error(
                            "external command name",
                            atom.tagged_type_name(),
                        ))
                    }
                    AtomicToken::String { body } => Expression::string(*body, *span),
                    AtomicToken::ItVariable { name } => Expression::it_variable(*name, *span),
                    AtomicToken::Variable { name } => Expression::variable(*name, *span),
                    AtomicToken::ExternalWord { .. }
                    | AtomicToken::GlobPattern { .. }
                    | AtomicToken::FilePath { .. }
                    | AtomicToken::Word { .. }
                    | AtomicToken::Dot { .. }
                    | AtomicToken::Operator { .. } => Expression::external_command(*span, *span),
                }),
            },
        }
    }
}

#[derive(Debug, Copy, Clone)]
struct ExternalContinuationShape;

impl ExpandExpression for ExternalContinuationShape {
    fn expand_expr<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
    ) -> Result<Expression, ShellError> {
        match expand_atom(
            token_nodes,
            "external argument",
            context,
            ExpansionRule::new()
                .allow_external_word()
                .treat_size_as_word()
                .reject_whitespace(),
        )? {
            atom => match &atom {
                Spanned { item, span } => Ok(match item {
                    AtomicToken::Eof { .. } => unreachable!("ExpansionRule doesn't allow EOF"),
                    AtomicToken::Error { .. } => unreachable!("ExpansionRule doesn't allow Error"),
                    AtomicToken::Number { number } => {
                        Expression::number(number.to_number(context.source()), *span)
                    }
                    AtomicToken::Size { .. } => unreachable!("ExpansionRule treats size as word"),
                    AtomicToken::ExternalCommand { .. } => {
                        unreachable!("ExpansionRule doesn't allow ExternalCommand")
                    }
                    AtomicToken::Whitespace { .. } => {
                        unreachable!("ExpansionRule doesn't allow Whitespace")
                    }
                    AtomicToken::String { body } => Expression::string(*body, *span),
                    AtomicToken::ItVariable { name } => Expression::it_variable(*name, *span),
                    AtomicToken::Variable { name } => Expression::variable(*name, *span),
                    AtomicToken::ExternalWord { .. } => Expression::bare(*span),
                    AtomicToken::GlobPattern { .. } => Expression::bare(*span),
                    AtomicToken::FilePath { .. } => Expression::bare(*span),
                    AtomicToken::Word { .. } => Expression::bare(*span),
                    AtomicToken::ShorthandFlag { .. } => Expression::bare(*span),
                    AtomicToken::LonghandFlag { .. } => Expression::bare(*span),
                    AtomicToken::Dot { .. } => Expression::bare(*span),
                    AtomicToken::Operator { .. } => Expression::bare(*span),
                    AtomicToken::SquareDelimited { .. }
                    | AtomicToken::ParenDelimited { .. }
                    | AtomicToken::BraceDelimited { .. }
                    | AtomicToken::Pipeline { .. } => {
                        return Err(ShellError::type_error(
                            "external argument",
                            atom.tagged_type_name(),
                        ))
                    }
                }),
            },
        }
    }
}

#[must_use]
enum ExternalExpressionResult {
    Eof,
    Processed,
}

#[cfg(not(coloring_in_tokens))]
impl ColorSyntax for ExternalExpression {
    type Info = ExternalExpressionResult;
    type Input = ();

    fn color_syntax<'a, 'b>(
        &self,
        _input: &(),
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
        shapes: &mut Vec<Spanned<FlatShape>>,
    ) -> ExternalExpressionResult {
        let atom = match expand_atom(
            token_nodes,
            "external word",
            context,
            ExpansionRule::permissive(),
        ) {
            Err(_) => unreachable!("TODO: separate infallible expand_atom"),
            Ok(Spanned {
                item: AtomicToken::Eof { .. },
                ..
            }) => return ExternalExpressionResult::Eof,
            Ok(atom) => atom,
        };

        atom.color_tokens(shapes);
        return ExternalExpressionResult::Processed;
    }
}

#[cfg(coloring_in_tokens)]
impl ColorSyntax for ExternalExpression {
    type Info = ExternalExpressionResult;
    type Input = ();

    fn name(&self) -> &'static str {
        "ExternalExpression"
    }

    fn color_syntax<'a, 'b>(
        &self,
        _input: &(),
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
    ) -> ExternalExpressionResult {
        let atom = match expand_atom(
            token_nodes,
            "external word",
            context,
            ExpansionRule::permissive(),
        ) {
            Err(_) => unreachable!("TODO: separate infallible expand_atom"),
            Ok(Spanned {
                item: AtomicToken::Eof { .. },
                ..
            }) => return ExternalExpressionResult::Eof,
            Ok(atom) => atom,
        };

        token_nodes.mutate_shapes(|shapes| atom.color_tokens(shapes));
        return ExternalExpressionResult::Processed;
    }
}
