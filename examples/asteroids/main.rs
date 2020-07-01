use amethyst::{
    core::transform::TransformBundle,
    prelude::*,
    renderer::{plugins::RenderToWindow, types::DefaultBackend, RenderFlat2D, RenderingBundle},
    utils::application_root_dir,
};

mod asteroids;

type InputHandler = amethyst::input::InputHandler<amethyst::input::StringBindings>;
type InputBundle = amethyst::input::InputBundle<amethyst::input::StringBindings>;

fn main() -> amethyst::Result<()> {
    amethyst::start_logger(amethyst::LoggerConfig {
        level_filter: amethyst::LogLevelFilter::Warn,
        ..Default::default()
    });

    let state = asteroids::Asteroids::new();

    let path = format!(
        "{}/examples/asteroids/resources/display_config.ron",
        application_root_dir()?.display()
    );

    let game_data = GameDataBuilder::default()
        .with_bundle(
            RenderingBundle::<DefaultBackend>::new()
                .with_plugin(
                    RenderToWindow::from_config_path(path)?.with_clear([0.0, 0.0, 0.0, 1.0]),
                )
                .with_plugin(RenderFlat2D::default()),
        )?
        .with_bundle(TransformBundle::new())?
        .with_bundle(InputBundle::new())?;
    let mut game = Application::new("./examples/assets", state, game_data)?;

    game.run();

    Ok(())
}
