pub mod client;
pub mod compile;
pub mod native_transport;
pub mod protocol;

pub use native_transport::spawn_worker;

/// The native host's registry: the universal signature contract (so compilation is
/// host-independent), with the native implementations attached.
pub fn build_builtin_registry() -> quiver_core::builtins::BuiltinRegistry<quiver_io::NativeEffect> {
    let mut registry = quiver_core::builtins::BuiltinRegistry::with_modules(
        &quiver_core::builtins::universal_modules(),
    );
    quiver_io::attach_network_builtins(&mut registry);
    quiver_io::attach_file_builtins(&mut registry);
    quiver_io::attach_system_builtins(&mut registry);
    quiver_io::attach_tls_builtins(&mut registry);
    quiver_io::attach_http_builtins(&mut registry);
    registry
}

/// Create the native effect backend. `waker` is the driver loop's: the backend pokes it when a
/// blocking call completes, so the loop notices at once rather than on its next io poll.
pub fn create_effect_backend(
    waker: native_transport::Waker,
) -> Option<Box<dyn quiver_core::effects::EffectBackend<E = quiver_io::NativeEffect>>> {
    quiver_io::NativeEffectBackend::new(256)
        .ok()
        .map(|backend| {
            backend.set_notify(Box::new(move || waker.wake()));
            Box::new(backend)
                as Box<dyn quiver_core::effects::EffectBackend<E = quiver_io::NativeEffect>>
        })
}
