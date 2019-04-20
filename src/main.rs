//! Game setup and very basic main loop.
//! All the actual work gets done in the Scene.

extern crate chrono;
extern crate env_logger;
#[macro_use]
extern crate failure;
extern crate fern;
extern crate ggez;
extern crate ggez_goodies;
#[macro_use]
extern crate log;
extern crate itertools;
extern crate shred;
extern crate specs;
#[macro_use]
extern crate specs_derive;
extern crate warmy;

extern crate gluon;
#[macro_use]
extern crate gluon_vm;
#[macro_use]
extern crate gluon_codegen;

use ggez::conf;
use ggez::event;
use ggez::*;

use ggez::event::*;
use ggez::graphics;
use ggez::timer;

use std::path;

#[macro_use]
mod gluon_system;

// Modules that define actual content
mod components;
mod scenes;
mod systems;
mod world;

// Modules that define utility stuff.
mod error;
mod input;
mod resources;
mod util;

/// Main game state.  This holds all our STUFF,
/// but most of the actual game data are
/// in `Scenes`, and the `FSceneStack` contains them
/// plus global game state.
pub struct MainState {
    scenes: scenes::FSceneStack,
    input_binding: input::InputBinding,
}

impl MainState {
    pub fn new(
        resource_dir: Option<path::PathBuf>,
        ctx: &mut Context,
    ) -> Result<Self, failure::Error> {
        let world = world::World::new(ctx, resource_dir.clone());
        let mut scenestack = scenes::FSceneStack::new(ctx, world);
        let initial_scene = Box::new(scenes::level::LevelScene::new(ctx, &mut scenestack.world)?);
        scenestack.push(initial_scene);
        Ok(MainState {
            scenes: scenestack,
            input_binding: input::create_input_binding(),
        })
    }
}

impl EventHandler for MainState {
    fn update(&mut self, ctx: &mut Context) -> GameResult<()> {
        const DESIRED_FPS: u32 = 60;
        while timer::check_update_time(ctx, DESIRED_FPS) {
            self.scenes.update();
        }
        self.scenes.world.specs_world.maintain();
        self.scenes.world.assets.sync(ctx);

        Ok(())
    }

    fn draw(&mut self, ctx: &mut Context) -> GameResult<()> {
        graphics::clear(ctx);
        self.scenes.draw(ctx);
        graphics::present(ctx);
        Ok(())
    }

    fn key_down_event(
        &mut self,
        _ctx: &mut Context,
        keycode: Keycode,
        _keymod: Mod,
        _repeat: bool,
    ) {
        if let Some(ev) = self.input_binding.resolve(keycode) {
            self.scenes.input(ev, true);
        }
    }

    fn key_up_event(&mut self, _ctx: &mut Context, keycode: Keycode, _keymod: Mod, _repeat: bool) {
        if let Some(ev) = self.input_binding.resolve(keycode) {
            self.scenes.input(ev, false);
        }
    }
}

pub fn main() {
    env_logger::init();
    let mut cb = ContextBuilder::new("game-template", "ggez")
        .window_setup(conf::WindowSetup::default().title("game-template"))
        .window_mode(conf::WindowMode::default().dimensions(800, 600));

    // We add the CARGO_MANIFEST_DIR/resources to the filesystems paths so
    // we we look in the cargo project for files.
    // And save it so we can feed there result into warmy
    let cargo_path: Option<path::PathBuf> = option_env!("CARGO_MANIFEST_DIR").map(|env_path| {
        let mut res_path = path::PathBuf::from(env_path);
        res_path.push("resources");
        res_path
    });
    // If we have such a path then add it to the context builder too
    // (modifying the cb from inside a closure gets sticky)
    if let Some(ref s) = cargo_path {
        cb = cb.add_resource_path(s);
    }

    let ctx = &mut cb.build().unwrap();

    let state = &mut MainState::new(cargo_path, ctx).unwrap_or_else(|err| panic!("{}", err));
    if let Err(e) = event::run(ctx, state) {
        println!("Error encountered: {}", e);
    } else {
        println!("Game exited cleanly.");
    }
}
