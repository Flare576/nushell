use crate::parser::hir::syntax_shape::{
    expand_atom, parse_single_node, ExpandContext, ExpandExpression, ExpansionRule,
    FallibleColorSyntax, FlatShape, ParseError,
};
use crate::parser::{
    hir,
    hir::{RawNumber, TokensIterator},
    RawToken,
};
use crate::prelude::*;

#[derive(Debug, Copy, Clone)]
pub struct NumberShape;

impl ExpandExpression for NumberShape {
    fn expand_expr<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        context: &ExpandContext,
    ) -> Result<hir::Expression, ParseError> {
        parse_single_node(token_nodes, "Number", |token, token_span, err| {
            Ok(match token {
                RawToken::GlobPattern | RawToken::Operator(..) => return Err(err.error()),
                RawToken::Variable(tag) if tag.slice(context.source) == "it" => {
                    hir::Expression::it_variable(tag, token_span)
                }
                RawToken::ExternalCommand(tag) => {
                    hir::Expression::external_command(tag, token_span)
                }
                RawToken::ExternalWord => {
                    return Err(ParseError::mismatch(
                        "number",
                        "syntax error".tagged(Tag {
                            span: token_span,
                            anchor: None,
                        }),
                    ))
                }
                RawToken::Variable(tag) => hir::Expression::variable(tag, token_span),
                RawToken::Number(number) => {
                    hir::Expression::number(number.to_number(context.source), token_span)
                }
                RawToken::Bare => hir::Expression::bare(token_span),
                RawToken::String(tag) => hir::Expression::string(tag, token_span),
            })
        })
    }
}

#[cfg(not(coloring_in_tokens))]
impl FallibleColorSyntax for NumberShape {
    type Info = ();
    type Input = ();

    fn color_syntax<'a, 'b>(
        &self,
        _input: &(),
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
        shapes: &mut Vec<Spanned<FlatShape>>,
    ) -> Result<(), ShellError> {
        let atom = token_nodes.spanned(|token_nodes| {
            expand_atom(token_nodes, "number", context, ExpansionRule::permissive())
        });

        let atom = match atom {
            Spanned { item: Err(_), span } => {
                shapes.push(FlatShape::Error.spanned(span));
                return Ok(());
            }
            Spanned { item: Ok(atom), .. } => atom,
        };

        atom.color_tokens(shapes);

        Ok(())
    }
}

#[cfg(coloring_in_tokens)]
impl FallibleColorSyntax for NumberShape {
    type Info = ();
    type Input = ();

    fn name(&self) -> &'static str {
        "NumberShape"
    }

    fn color_syntax<'a, 'b>(
        &self,
        _input: &(),
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
    ) -> Result<(), ShellError> {
        let atom = token_nodes.spanned(|token_nodes| {
            expand_atom(token_nodes, "number", context, ExpansionRule::permissive())
        });

        let atom = match atom {
            Spanned { item: Err(_), span } => {
                token_nodes.color_shape(FlatShape::Error.spanned(span));
                return Ok(());
            }
            Spanned { item: Ok(atom), .. } => atom,
        };

        token_nodes.mutate_shapes(|shapes| atom.color_tokens(shapes));

        Ok(())
    }
}

#[derive(Debug, Copy, Clone)]
pub struct IntShape;

impl ExpandExpression for IntShape {
    fn expand_expr<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        context: &ExpandContext,
    ) -> Result<hir::Expression, ParseError> {
        parse_single_node(token_nodes, "Integer", |token, token_span, err| {
            Ok(match token {
                RawToken::GlobPattern | RawToken::Operator(..) | RawToken::ExternalWord => {
                    return Err(err.error())
                }
                RawToken::Variable(span) if span.slice(context.source) == "it" => {
                    hir::Expression::it_variable(span, token_span)
                }
                RawToken::ExternalCommand(span) => {
                    hir::Expression::external_command(span, token_span)
                }
                RawToken::Variable(span) => hir::Expression::variable(span, token_span),
                RawToken::Number(number @ RawNumber::Int(_)) => {
                    hir::Expression::number(number.to_number(context.source), token_span)
                }
                RawToken::Number(_) => return Err(err.error()),
                RawToken::Bare => hir::Expression::bare(token_span),
                RawToken::String(span) => hir::Expression::string(span, token_span),
            })
        })
    }
}

#[cfg(not(coloring_in_tokens))]
impl FallibleColorSyntax for IntShape {
    type Info = ();
    type Input = ();

    fn color_syntax<'a, 'b>(
        &self,
        _input: &(),
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
        shapes: &mut Vec<Spanned<FlatShape>>,
    ) -> Result<(), ShellError> {
        let atom = token_nodes.spanned(|token_nodes| {
            expand_atom(token_nodes, "integer", context, ExpansionRule::permissive())
        });

        let atom = match atom {
            Spanned { item: Err(_), span } => {
                shapes.push(FlatShape::Error.spanned(span));
                return Ok(());
            }
            Spanned { item: Ok(atom), .. } => atom,
        };

        atom.color_tokens(shapes);

        Ok(())
    }
}

#[cfg(coloring_in_tokens)]
impl FallibleColorSyntax for IntShape {
    type Info = ();
    type Input = ();

    fn name(&self) -> &'static str {
        "IntShape"
    }

    fn color_syntax<'a, 'b>(
        &self,
        _input: &(),
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
    ) -> Result<(), ShellError> {
        let atom = token_nodes.spanned(|token_nodes| {
            expand_atom(token_nodes, "integer", context, ExpansionRule::permissive())
        });

        let atom = match atom {
            Spanned { item: Err(_), span } => {
                token_nodes.color_shape(FlatShape::Error.spanned(span));
                return Ok(());
            }
            Spanned { item: Ok(atom), .. } => atom,
        };

        token_nodes.mutate_shapes(|shapes| atom.color_tokens(shapes));

        Ok(())
    }
}
