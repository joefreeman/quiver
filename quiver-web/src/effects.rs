use quiver_core::effects::Effect;
use quiver_core::value::ResourceId;
use serde::{Deserialize, Serialize};

/// Web platform effects (minimal - most operations done via JS interop)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum WebEffect {
    // Placeholder for future web-specific effects
    // For now, web operations are handled via JavaScript interop
}

impl Effect for WebEffect {
    fn resource_id(&self) -> Option<ResourceId> {
        // No resource-based effects in web platform yet
        None
    }
}
