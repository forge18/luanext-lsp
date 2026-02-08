//! Navigation features (go-to, hover, references)

pub mod definition;
pub mod hover;
pub mod references;

pub use definition::DefinitionProvider;
pub use hover::HoverProvider;
pub use references::ReferencesProvider;
