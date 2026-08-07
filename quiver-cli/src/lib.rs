pub mod client;
pub mod compile;
pub mod native_transport;
pub mod protocol;

pub use native_transport::spawn_worker;

/// Build complete builtin registry including core builtins and network builtins
pub fn build_builtin_registry() -> quiver_core::builtins::BuiltinRegistry<quiver_io::NativeEffect> {
    let mut registry = quiver_core::builtins::BuiltinRegistry::with_modules(
        &quiver_core::builtins::core_modules(),
    );
    for module in quiver_core::builtins::io_modules()
        .into_iter()
        .chain(quiver_core::builtins::tls_modules())
    {
        module(&mut registry);
    }
    // Add I/O builtins from quiver-io
    // Signatures came from `core_modules`; attach the native implementations.
    quiver_io::attach_network_builtins(&mut registry);
    quiver_io::attach_file_builtins(&mut registry);
    quiver_io::attach_system_builtins(&mut registry);
    quiver_io::attach_tls_builtins(&mut registry);
    registry
}

/// Create an effect backend for the new effects system
pub fn create_effect_backend()
-> Option<Box<dyn quiver_core::effects::EffectBackend<E = quiver_io::NativeEffect>>> {
    quiver_io::NativeEffectBackend::new(256)
        .ok()
        .map(|backend| {
            Box::new(backend)
                as Box<dyn quiver_core::effects::EffectBackend<E = quiver_io::NativeEffect>>
        })
}
