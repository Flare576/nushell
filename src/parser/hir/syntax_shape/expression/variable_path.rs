use crate::parser::hir::syntax_shape::{
    color_fallible_syntax, color_fallible_syntax_with, expand_atom, expand_expr, expand_syntax,
    parse_single_node, AnyExpressionShape, AtomicToken, BareShape, ExpandContext, ExpandExpression,
    ExpandSyntax, ExpansionRule, FallibleColorSyntax, FlatShape, Peeked, SkipSyntax, StringShape,
    TestSyntax, WhitespaceShape,
};
use crate::parser::{hir, hir::Expression, hir::TokensIterator, Operator, RawToken};
use crate::prelude::*;

#[derive(Debug, Copy, Clone)]
pub struct VariablePathShape;

impl ExpandExpression for VariablePathShape {
    fn expand_expr<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        context: &ExpandContext,
    ) -> Result<hir::Expression, ShellError> {
        // 1. let the head be the first token, expecting a variable
        // 2. let the tail be an empty list of members
        // 2. while the next token (excluding ws) is a dot:
        //   1. consume the dot
        //   2. consume the next token as a member and push it onto tail

        let head = expand_expr(&VariableShape, token_nodes, context)?;
        let start = head.span;
        let mut end = start;
        let mut tail: Vec<Spanned<String>> = vec![];

        loop {
            match DotShape.skip(token_nodes, context) {
                Err(_) => break,
                Ok(_) => {}
            }

            let syntax = expand_syntax(&MemberShape, token_nodes, context)?;
            let member = syntax.to_spanned_string(context.source);

            end = member.span;
            tail.push(member);
        }

        Ok(hir::Expression::path(head, tail, start.until(end)))
    }
}

#[cfg(not(coloring_in_tokens))]
impl FallibleColorSyntax for VariablePathShape {
    type Info = ();
    type Input = ();

    fn color_syntax<'a, 'b>(
        &self,
        _input: &(),
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
        shapes: &mut Vec<Spanned<FlatShape>>,
    ) -> Result<(), ShellError> {
        token_nodes.atomic(|token_nodes| {
            // If the head of the token stream is not a variable, fail
            color_fallible_syntax(&VariableShape, token_nodes, context, shapes)?;

            loop {
                // look for a dot at the head of a stream
                let dot = color_fallible_syntax_with(
                    &ColorableDotShape,
                    &FlatShape::Dot,
                    token_nodes,
                    context,
                    shapes,
                );

                // if there's no dot, we're done
                match dot {
                    Err(_) => break,
                    Ok(_) => {}
                }

                // otherwise, look for a member, and if you don't find one, fail
                color_fallible_syntax(&MemberShape, token_nodes, context, shapes)?;
            }

            Ok(())
        })
    }
}

#[cfg(coloring_in_tokens)]
impl FallibleColorSyntax for VariablePathShape {
    type Info = ();
    type Input = ();

    fn name(&self) -> &'static str {
        "VariablePathShape"
    }

    fn color_syntax<'a, 'b>(
        &self,
        _input: &(),
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
    ) -> Result<(), ShellError> {
        token_nodes.atomic(|token_nodes| {
            // If the head of the token stream is not a variable, fail
            color_fallible_syntax(&VariableShape, token_nodes, context)?;

            loop {
                // look for a dot at the head of a stream
                let dot = color_fallible_syntax_with(
                    &ColorableDotShape,
                    &FlatShape::Dot,
                    token_nodes,
                    context,
                );

                // if there's no dot, we're done
                match dot {
                    Err(_) => break,
                    Ok(_) => {}
                }

                // otherwise, look for a member, and if you don't find one, fail
                color_fallible_syntax(&MemberShape, token_nodes, context)?;
            }

            Ok(())
        })
    }
}

#[derive(Debug, Copy, Clone)]
pub struct PathTailShape;

#[cfg(not(coloring_in_tokens))]
/// The failure mode of `PathTailShape` is a dot followed by a non-member
impl FallibleColorSyntax for PathTailShape {
    type Info = ();
    type Input = ();

    fn color_syntax<'a, 'b>(
        &self,
        _input: &(),
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
        shapes: &mut Vec<Spanned<FlatShape>>,
    ) -> Result<(), ShellError> {
        token_nodes.atomic(|token_nodes| loop {
            let result = color_fallible_syntax_with(
                &ColorableDotShape,
                &FlatShape::Dot,
                token_nodes,
                context,
                shapes,
            );

            match result {
                Err(_) => return Ok(()),
                Ok(_) => {}
            }

            // If we've seen a dot but not a member, fail
            color_fallible_syntax(&MemberShape, token_nodes, context, shapes)?;
        })
    }
}

#[cfg(coloring_in_tokens)]
/// The failure mode of `PathTailShape` is a dot followed by a non-member
impl FallibleColorSyntax for PathTailShape {
    type Info = ();
    type Input = ();

    fn name(&self) -> &'static str {
        "PathTailShape"
    }

    fn color_syntax<'a, 'b>(
        &self,
        _input: &(),
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
    ) -> Result<(), ShellError> {
        token_nodes.atomic(|token_nodes| loop {
            let result = color_fallible_syntax_with(
                &ColorableDotShape,
                &FlatShape::Dot,
                token_nodes,
                context,
            );

            match result {
                Err(_) => return Ok(()),
                Ok(_) => {}
            }

            // If we've seen a dot but not a member, fail
            color_fallible_syntax(&MemberShape, token_nodes, context)?;
        })
    }
}

impl ExpandSyntax for PathTailShape {
    type Output = (Vec<Spanned<String>>, Span);
    fn expand_syntax<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
    ) -> Result<Self::Output, ShellError> {
        let mut end: Option<Span> = None;
        let mut tail = vec![];

        loop {
            match DotShape.skip(token_nodes, context) {
                Err(_) => break,
                Ok(_) => {}
            }

            let syntax = expand_syntax(&MemberShape, token_nodes, context)?;
            let member = syntax.to_spanned_string(context.source);
            end = Some(member.span);
            tail.push(member);
        }

        match end {
            None => {
                return Err(ShellError::type_error("path tail", {
                    let typed_span = token_nodes.typed_span_at_cursor();

                    Tagged {
                        tag: typed_span.span.into(),
                        item: typed_span.item,
                    }
                }))
            }

            Some(end) => Ok((tail, end)),
        }
    }
}

#[derive(Debug)]
pub enum ExpressionContinuation {
    DotSuffix(Span, Spanned<String>),
    InfixSuffix(Spanned<Operator>, Expression),
}

/// An expression continuation
#[derive(Debug, Copy, Clone)]
pub struct ExpressionContinuationShape;

impl ExpandSyntax for ExpressionContinuationShape {
    type Output = ExpressionContinuation;

    fn expand_syntax<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        context: &ExpandContext,
    ) -> Result<ExpressionContinuation, ShellError> {
        // Try to expand a `.`
        let dot = expand_syntax(&DotShape, token_nodes, context);

        match dot {
            // If a `.` was matched, it's a `Path`, and we expect a `Member` next
            Ok(dot) => {
                let syntax = expand_syntax(&MemberShape, token_nodes, context)?;
                let member = syntax.to_spanned_string(context.source);

                Ok(ExpressionContinuation::DotSuffix(dot, member))
            }

            // Otherwise, we expect an infix operator and an expression next
            Err(_) => {
                let (_, op, _) = expand_syntax(&InfixShape, token_nodes, context)?;
                let next = expand_expr(&AnyExpressionShape, token_nodes, context)?;

                Ok(ExpressionContinuation::InfixSuffix(op, next))
            }
        }
    }
}

pub enum ContinuationInfo {
    Dot,
    Infix,
}

#[cfg(not(coloring_in_tokens))]
impl FallibleColorSyntax for ExpressionContinuationShape {
    type Info = ContinuationInfo;
    type Input = ();

    fn color_syntax<'a, 'b>(
        &self,
        _input: &(),
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
        shapes: &mut Vec<Spanned<FlatShape>>,
    ) -> Result<ContinuationInfo, ShellError> {
        token_nodes.atomic(|token_nodes| {
            // Try to expand a `.`
            let dot = color_fallible_syntax_with(
                &ColorableDotShape,
                &FlatShape::Dot,
                token_nodes,
                context,
                shapes,
            );

            match dot {
                Ok(_) => {
                    // we found a dot, so let's keep looking for a member; if no member was found, fail
                    color_fallible_syntax(&MemberShape, token_nodes, context, shapes)?;

                    Ok(ContinuationInfo::Dot)
                }
                Err(_) => {
                    let mut new_shapes = vec![];
                    let result = token_nodes.atomic(|token_nodes| {
                        // we didn't find a dot, so let's see if we're looking at an infix. If not found, fail
                        color_fallible_syntax(&InfixShape, token_nodes, context, &mut new_shapes)?;

                        // now that we've seen an infix shape, look for any expression. If not found, fail
                        color_fallible_syntax(
                            &AnyExpressionShape,
                            token_nodes,
                            context,
                            &mut new_shapes,
                        )?;

                        Ok(ContinuationInfo::Infix)
                    })?;
                    shapes.extend(new_shapes);
                    Ok(result)
                }
            }
        })
    }
}

#[cfg(coloring_in_tokens)]
impl FallibleColorSyntax for ExpressionContinuationShape {
    type Info = ContinuationInfo;
    type Input = ();

    fn name(&self) -> &'static str {
        "ExpressionContinuationShape"
    }

    fn color_syntax<'a, 'b>(
        &self,
        _input: &(),
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
    ) -> Result<ContinuationInfo, ShellError> {
        token_nodes.atomic(|token_nodes| {
            // Try to expand a `.`
            let dot = color_fallible_syntax_with(
                &ColorableDotShape,
                &FlatShape::Dot,
                token_nodes,
                context,
            );

            match dot {
                Ok(_) => {
                    // we found a dot, so let's keep looking for a member; if no member was found, fail
                    color_fallible_syntax(&MemberShape, token_nodes, context)?;

                    Ok(ContinuationInfo::Dot)
                }
                Err(_) => {
                    let result = token_nodes.atomic(|token_nodes| {
                        // we didn't find a dot, so let's see if we're looking at an infix. If not found, fail
                        color_fallible_syntax(&InfixShape, token_nodes, context)?;

                        // now that we've seen an infix shape, look for any expression. If not found, fail
                        color_fallible_syntax(&AnyExpressionShape, token_nodes, context)?;

                        Ok(ContinuationInfo::Infix)
                    })?;

                    Ok(result)
                }
            }
        })
    }
}

#[derive(Debug, Copy, Clone)]
pub struct VariableShape;

impl ExpandExpression for VariableShape {
    fn expand_expr<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        context: &ExpandContext,
    ) -> Result<hir::Expression, ShellError> {
        parse_single_node(token_nodes, "variable", |token, token_tag, _| {
            Ok(match token {
                RawToken::Variable(tag) => {
                    if tag.slice(context.source) == "it" {
                        hir::Expression::it_variable(tag, token_tag)
                    } else {
                        hir::Expression::variable(tag, token_tag)
                    }
                }
                _ => {
                    return Err(ShellError::type_error(
                        "variable",
                        token.type_name().tagged(token_tag),
                    ))
                }
            })
        })
    }
}

#[cfg(not(coloring_in_tokens))]
impl FallibleColorSyntax for VariableShape {
    type Info = ();
    type Input = ();

    fn color_syntax<'a, 'b>(
        &self,
        _input: &(),
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
        shapes: &mut Vec<Spanned<FlatShape>>,
    ) -> Result<(), ShellError> {
        let atom = expand_atom(
            token_nodes,
            "variable",
            context,
            ExpansionRule::permissive(),
        );

        let atom = match atom {
            Err(err) => return Err(err),
            Ok(atom) => atom,
        };

        match &atom.item {
            AtomicToken::Variable { .. } => {
                shapes.push(FlatShape::Variable.spanned(atom.span));
                Ok(())
            }
            AtomicToken::ItVariable { .. } => {
                shapes.push(FlatShape::ItVariable.spanned(atom.span));
                Ok(())
            }
            _ => Err(ShellError::type_error("variable", atom.tagged_type_name())),
        }
    }
}

#[cfg(coloring_in_tokens)]
impl FallibleColorSyntax for VariableShape {
    type Info = ();
    type Input = ();

    fn name(&self) -> &'static str {
        "VariableShape"
    }

    fn color_syntax<'a, 'b>(
        &self,
        _input: &(),
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
    ) -> Result<(), ShellError> {
        let atom = expand_atom(
            token_nodes,
            "variable",
            context,
            ExpansionRule::permissive(),
        );

        let atom = match atom {
            Err(err) => return Err(err),
            Ok(atom) => atom,
        };

        match &atom.item {
            AtomicToken::Variable { .. } => {
                token_nodes.color_shape(FlatShape::Variable.spanned(atom.span));
                Ok(())
            }
            AtomicToken::ItVariable { .. } => {
                token_nodes.color_shape(FlatShape::ItVariable.spanned(atom.span));
                Ok(())
            }
            _ => Err(ShellError::type_error("variable", atom.tagged_type_name())),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Member {
    String(/* outer */ Span, /* inner */ Span),
    Bare(Span),
}

impl Member {
    pub(crate) fn to_expr(&self) -> hir::Expression {
        match self {
            Member::String(outer, inner) => hir::Expression::string(*inner, *outer),
            Member::Bare(span) => hir::Expression::string(*span, *span),
        }
    }

    pub(crate) fn span(&self) -> Span {
        match self {
            Member::String(outer, _inner) => *outer,
            Member::Bare(span) => *span,
        }
    }

    pub(crate) fn to_spanned_string(&self, source: &str) -> Spanned<String> {
        match self {
            Member::String(outer, inner) => inner.string(source).spanned(*outer),
            Member::Bare(span) => span.spanned_string(source),
        }
    }

    pub(crate) fn tagged_type_name(&self) -> Tagged<&'static str> {
        match self {
            Member::String(outer, _inner) => "string".tagged(outer),
            Member::Bare(span) => "word".tagged(Tag {
                span: *span,
                anchor: None,
            }),
        }
    }
}

enum ColumnPathState {
    Initial,
    LeadingDot(Span),
    Dot(Span, Vec<Member>, Span),
    Member(Span, Vec<Member>),
    Error(ShellError),
}

impl ColumnPathState {
    pub fn dot(self, dot: Span) -> ColumnPathState {
        match self {
            ColumnPathState::Initial => ColumnPathState::LeadingDot(dot),
            ColumnPathState::LeadingDot(_) => {
                ColumnPathState::Error(ShellError::type_error("column", "dot".tagged(dot)))
            }
            ColumnPathState::Dot(..) => {
                ColumnPathState::Error(ShellError::type_error("column", "dot".tagged(dot)))
            }
            ColumnPathState::Member(tag, members) => ColumnPathState::Dot(tag, members, dot),
            ColumnPathState::Error(err) => ColumnPathState::Error(err),
        }
    }

    pub fn member(self, member: Member) -> ColumnPathState {
        match self {
            ColumnPathState::Initial => ColumnPathState::Member(member.span(), vec![member]),
            ColumnPathState::LeadingDot(tag) => {
                ColumnPathState::Member(tag.until(member.span()), vec![member])
            }

            ColumnPathState::Dot(tag, mut tags, _) => {
                ColumnPathState::Member(tag.until(member.span()), {
                    tags.push(member);
                    tags
                })
            }
            ColumnPathState::Member(..) => {
                ColumnPathState::Error(ShellError::type_error("column", member.tagged_type_name()))
            }
            ColumnPathState::Error(err) => ColumnPathState::Error(err),
        }
    }

    pub fn into_path(self, next: Peeked) -> Result<Tagged<Vec<Member>>, ShellError> {
        match self {
            ColumnPathState::Initial => Err(next.type_error("column path")),
            ColumnPathState::LeadingDot(dot) => {
                Err(ShellError::type_error("column", "dot".tagged(dot)))
            }
            ColumnPathState::Dot(_tag, _members, dot) => {
                Err(ShellError::type_error("column", "dot".tagged(dot)))
            }
            ColumnPathState::Member(tag, tags) => Ok(tags.tagged(tag)),
            ColumnPathState::Error(err) => Err(err),
        }
    }
}

pub fn expand_column_path<'a, 'b>(
    token_nodes: &'b mut TokensIterator<'a>,
    context: &ExpandContext,
) -> Result<Tagged<Vec<Member>>, ShellError> {
    let mut state = ColumnPathState::Initial;

    loop {
        let member = MemberShape.expand_syntax(token_nodes, context);

        match member {
            Err(_) => break,
            Ok(member) => state = state.member(member),
        }

        let dot = DotShape.expand_syntax(token_nodes, context);

        match dot {
            Err(_) => break,
            Ok(dot) => state = state.dot(dot),
        }
    }

    state.into_path(token_nodes.peek_non_ws())
}

#[derive(Debug, Copy, Clone)]
pub struct ColumnPathShape;

#[cfg(not(coloring_in_tokens))]
impl FallibleColorSyntax for ColumnPathShape {
    type Info = ();
    type Input = ();

    fn color_syntax<'a, 'b>(
        &self,
        _input: &(),
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
        shapes: &mut Vec<Spanned<FlatShape>>,
    ) -> Result<(), ShellError> {
        // If there's not even one member shape, fail
        color_fallible_syntax(&MemberShape, token_nodes, context, shapes)?;

        loop {
            let checkpoint = token_nodes.checkpoint();

            match color_fallible_syntax_with(
                &ColorableDotShape,
                &FlatShape::Dot,
                checkpoint.iterator,
                context,
                shapes,
            ) {
                Err(_) => {
                    // we already saw at least one member shape, so return successfully
                    return Ok(());
                }

                Ok(_) => {
                    match color_fallible_syntax(&MemberShape, checkpoint.iterator, context, shapes)
                    {
                        Err(_) => {
                            // we saw a dot but not a member (but we saw at least one member),
                            // so don't commit the dot but return successfully
                            return Ok(());
                        }

                        Ok(_) => {
                            // we saw a dot and a member, so commit it and continue on
                            checkpoint.commit();
                        }
                    }
                }
            }
        }
    }
}

#[cfg(coloring_in_tokens)]
impl FallibleColorSyntax for ColumnPathShape {
    type Info = ();
    type Input = ();

    fn name(&self) -> &'static str {
        "ColumnPathShape"
    }

    fn color_syntax<'a, 'b>(
        &self,
        _input: &(),
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
    ) -> Result<(), ShellError> {
        // If there's not even one member shape, fail
        color_fallible_syntax(&MemberShape, token_nodes, context)?;

        loop {
            let checkpoint = token_nodes.checkpoint();

            match color_fallible_syntax_with(
                &ColorableDotShape,
                &FlatShape::Dot,
                checkpoint.iterator,
                context,
            ) {
                Err(_) => {
                    // we already saw at least one member shape, so return successfully
                    return Ok(());
                }

                Ok(_) => {
                    match color_fallible_syntax(&MemberShape, checkpoint.iterator, context) {
                        Err(_) => {
                            // we saw a dot but not a member (but we saw at least one member),
                            // so don't commit the dot but return successfully
                            return Ok(());
                        }

                        Ok(_) => {
                            // we saw a dot and a member, so commit it and continue on
                            checkpoint.commit();
                        }
                    }
                }
            }
        }
    }
}

impl ExpandSyntax for ColumnPathShape {
    type Output = Tagged<Vec<Member>>;

    fn expand_syntax<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
    ) -> Result<Self::Output, ShellError> {
        expand_column_path(token_nodes, context)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct MemberShape;

#[cfg(not(coloring_in_tokens))]
impl FallibleColorSyntax for MemberShape {
    type Info = ();
    type Input = ();

    fn color_syntax<'a, 'b>(
        &self,
        _input: &(),
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
        shapes: &mut Vec<Spanned<FlatShape>>,
    ) -> Result<(), ShellError> {
        let bare = color_fallible_syntax_with(
            &BareShape,
            &FlatShape::BareMember,
            token_nodes,
            context,
            shapes,
        );

        match bare {
            Ok(_) => return Ok(()),
            Err(_) => {
                // If we don't have a bare word, we'll look for a string
            }
        }

        // Look for a string token. If we don't find one, fail
        color_fallible_syntax_with(
            &StringShape,
            &FlatShape::StringMember,
            token_nodes,
            context,
            shapes,
        )
    }
}

#[cfg(coloring_in_tokens)]
impl FallibleColorSyntax for MemberShape {
    type Info = ();
    type Input = ();

    fn name(&self) -> &'static str {
        "MemberShape"
    }

    fn color_syntax<'a, 'b>(
        &self,
        _input: &(),
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
    ) -> Result<(), ShellError> {
        let bare =
            color_fallible_syntax_with(&BareShape, &FlatShape::BareMember, token_nodes, context);

        match bare {
            Ok(_) => return Ok(()),
            Err(_) => {
                // If we don't have a bare word, we'll look for a string
            }
        }

        // Look for a string token. If we don't find one, fail
        color_fallible_syntax_with(&StringShape, &FlatShape::StringMember, token_nodes, context)
    }
}

impl ExpandSyntax for MemberShape {
    type Output = Member;

    fn expand_syntax<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        context: &ExpandContext,
    ) -> Result<Member, ShellError> {
        let bare = BareShape.test(token_nodes, context);
        if let Some(peeked) = bare {
            let node = peeked.not_eof("column")?.commit();
            return Ok(Member::Bare(node.span()));
        }

        let string = StringShape.test(token_nodes, context);

        if let Some(peeked) = string {
            let node = peeked.not_eof("column")?.commit();
            let (outer, inner) = node.as_string().unwrap();

            return Ok(Member::String(outer, inner));
        }

        Err(token_nodes.peek_any().type_error("column"))
    }
}

#[derive(Debug, Copy, Clone)]
pub struct DotShape;

#[derive(Debug, Copy, Clone)]
pub struct ColorableDotShape;

#[cfg(not(coloring_in_tokens))]
impl FallibleColorSyntax for ColorableDotShape {
    type Info = ();
    type Input = FlatShape;

    fn color_syntax<'a, 'b>(
        &self,
        input: &FlatShape,
        token_nodes: &'b mut TokensIterator<'a>,
        _context: &ExpandContext,
        shapes: &mut Vec<Spanned<FlatShape>>,
    ) -> Result<(), ShellError> {
        let peeked = token_nodes.peek_any().not_eof("dot")?;

        match peeked.node {
            node if node.is_dot() => {
                peeked.commit();
                shapes.push((*input).spanned(node.span()));
                Ok(())
            }

            other => Err(ShellError::type_error("dot", other.tagged_type_name())),
        }
    }
}

#[cfg(coloring_in_tokens)]
impl FallibleColorSyntax for ColorableDotShape {
    type Info = ();
    type Input = FlatShape;

    fn name(&self) -> &'static str {
        "ColorableDotShape"
    }

    fn color_syntax<'a, 'b>(
        &self,
        input: &FlatShape,
        token_nodes: &'b mut TokensIterator<'a>,
        _context: &ExpandContext,
    ) -> Result<(), ShellError> {
        let peeked = token_nodes.peek_any().not_eof("dot")?;

        match peeked.node {
            node if node.is_dot() => {
                peeked.commit();
                token_nodes.color_shape((*input).spanned(node.span()));
                Ok(())
            }

            other => Err(ShellError::type_error("dot", other.tagged_type_name())),
        }
    }
}

impl SkipSyntax for DotShape {
    fn skip<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        context: &ExpandContext,
    ) -> Result<(), ShellError> {
        expand_syntax(self, token_nodes, context)?;

        Ok(())
    }
}

impl ExpandSyntax for DotShape {
    type Output = Span;

    fn expand_syntax<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        _context: &ExpandContext,
    ) -> Result<Self::Output, ShellError> {
        parse_single_node(token_nodes, "dot", |token, token_span, _| {
            Ok(match token {
                RawToken::Operator(Operator::Dot) => token_span,
                _ => {
                    return Err(ShellError::type_error(
                        "dot",
                        token.type_name().tagged(token_span),
                    ))
                }
            })
        })
    }
}

#[derive(Debug, Copy, Clone)]
pub struct InfixShape;

#[cfg(not(coloring_in_tokens))]
impl FallibleColorSyntax for InfixShape {
    type Info = ();
    type Input = ();

    fn color_syntax<'a, 'b>(
        &self,
        _input: &(),
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
        outer_shapes: &mut Vec<Spanned<FlatShape>>,
    ) -> Result<(), ShellError> {
        let checkpoint = token_nodes.checkpoint();
        let mut shapes = vec![];

        // An infix operator must be prefixed by whitespace. If no whitespace was found, fail
        color_fallible_syntax(&WhitespaceShape, checkpoint.iterator, context, &mut shapes)?;

        // Parse the next TokenNode after the whitespace
        parse_single_node(
            checkpoint.iterator,
            "infix operator",
            |token, token_span, _| {
                match token {
                    // If it's an operator (and not `.`), it's a match
                    RawToken::Operator(operator) if operator != Operator::Dot => {
                        shapes.push(FlatShape::Operator.spanned(token_span));
                        Ok(())
                    }

                    // Otherwise, it's not a match
                    _ => Err(ShellError::type_error(
                        "infix operator",
                        token.type_name().tagged(token_span),
                    )),
                }
            },
        )?;

        // An infix operator must be followed by whitespace. If no whitespace was found, fail
        color_fallible_syntax(&WhitespaceShape, checkpoint.iterator, context, &mut shapes)?;

        outer_shapes.extend(shapes);
        checkpoint.commit();
        Ok(())
    }
}

#[cfg(coloring_in_tokens)]
impl FallibleColorSyntax for InfixShape {
    type Info = ();
    type Input = ();

    fn name(&self) -> &'static str {
        "InfixShape"
    }

    fn color_syntax<'a, 'b>(
        &self,
        _input: &(),
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
    ) -> Result<(), ShellError> {
        let checkpoint = token_nodes.checkpoint();

        // An infix operator must be prefixed by whitespace. If no whitespace was found, fail
        color_fallible_syntax(&WhitespaceShape, checkpoint.iterator, context)?;

        // Parse the next TokenNode after the whitespace
        let operator_span = parse_single_node(
            checkpoint.iterator,
            "infix operator",
            |token, token_span, _| {
                match token {
                    // If it's an operator (and not `.`), it's a match
                    RawToken::Operator(operator) if operator != Operator::Dot => Ok(token_span),

                    // Otherwise, it's not a match
                    _ => Err(ShellError::type_error(
                        "infix operator",
                        token.type_name().tagged(token_span),
                    )),
                }
            },
        )?;

        checkpoint
            .iterator
            .color_shape(FlatShape::Operator.spanned(operator_span));

        // An infix operator must be followed by whitespace. If no whitespace was found, fail
        color_fallible_syntax(&WhitespaceShape, checkpoint.iterator, context)?;

        checkpoint.commit();
        Ok(())
    }
}

impl ExpandSyntax for InfixShape {
    type Output = (Span, Spanned<Operator>, Span);

    fn expand_syntax<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
    ) -> Result<Self::Output, ShellError> {
        let checkpoint = token_nodes.checkpoint();

        // An infix operator must be prefixed by whitespace
        let start = expand_syntax(&WhitespaceShape, checkpoint.iterator, context)?;

        // Parse the next TokenNode after the whitespace
        let operator = parse_single_node(
            checkpoint.iterator,
            "infix operator",
            |token, token_span, _| {
                Ok(match token {
                    // If it's an operator (and not `.`), it's a match
                    RawToken::Operator(operator) if operator != Operator::Dot => {
                        operator.spanned(token_span)
                    }

                    // Otherwise, it's not a match
                    _ => {
                        return Err(ShellError::type_error(
                            "infix operator",
                            token.type_name().tagged(token_span),
                        ))
                    }
                })
            },
        )?;

        // An infix operator must be followed by whitespace
        let end = expand_syntax(&WhitespaceShape, checkpoint.iterator, context)?;

        checkpoint.commit();

        Ok((start, operator, end))
    }
}
