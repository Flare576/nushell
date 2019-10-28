use crate::parser::TokenNode;
use crate::traits::ToDebug;
use crate::{Span, Spanned};
use derive_new::new;
use getset::Getters;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Getters, new)]
pub struct Pipeline {
    #[get = "pub"]
    pub(crate) parts: Vec<Spanned<PipelineElement>>,
}

impl ToDebug for Pipeline {
    fn fmt_debug(&self, f: &mut fmt::Formatter, source: &str) -> fmt::Result {
        for part in self.parts.iter() {
            write!(f, "{}", part.debug(source))?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Getters, new)]
pub struct PipelineElement {
    pub pipe: Option<Span>,
    #[get = "pub"]
    pub tokens: Spanned<Vec<TokenNode>>,
}

impl ToDebug for PipelineElement {
    fn fmt_debug(&self, f: &mut fmt::Formatter, source: &str) -> fmt::Result {
        if let Some(pipe) = self.pipe {
            write!(f, "{}", pipe.slice(source))?;
        }

        for token in &self.tokens.item {
            write!(f, "{}", token.debug(source))?;
        }

        Ok(())
    }
}
