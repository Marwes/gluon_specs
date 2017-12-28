// `error_chain!` can recurse deeply
#![recursion_limit = "1024"]

extern crate ggez;
extern crate ggez_goodies;
extern crate specs;
#[macro_use]
extern crate specs_derive;
extern crate warmy;
#[macro_use]
extern crate failure;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate slog;
extern crate slog_async;
extern crate slog_term;

use ggez::*;
use ggez::conf;
use ggez::event;

use ggez::event::*;
use ggez::graphics;
use ggez::timer;
// use ggez_goodies::camera;
use ggez_goodies::input as ginput;
use ggez_goodies::scene;

use std::path;


mod components;
mod input;
mod scenes;
mod systems;
mod world;

mod log;
mod util;


pub struct MainState {
    scenes: scenes::FSceneStack,
    input_binding: ginput::InputBinding<input::Axis, input::Button>,
}


impl MainState {
    pub fn new(resource_dir: Option<path::PathBuf>, ctx: &mut Context) -> Self {        
        let mut scenestack = scenes::FSceneStack::new(ctx, |ctx| world::World::new(ctx, resource_dir.clone()));
        let initial_scene = Box::new(scenes::level::LevelScene::new(ctx, &mut scenestack.world));
        scenestack.push(initial_scene);
        MainState {
            scenes: scenestack,
            input_binding: input::create_input_binding(),
        }
    }
}

impl EventHandler for MainState {
    fn update(&mut self, _ctx: &mut Context) -> GameResult<()> {
        // println!("Vert axis: {}", self.input_manager.get_axis(input::Axis::Vert));
        // println!("Horz axis: {}", self.input_manager.get_axis(input::Axis::Horz));
        // println!("Button: {}", self.input_manager.get_button_pressed(input::Button::Fire));
        // TODO: update timing properly
        self.scenes.update();

        Ok(())
    }

    fn draw(&mut self, ctx: &mut Context) -> GameResult<()> {
        graphics::clear(ctx);
        self.scenes.draw(ctx);
        graphics::present(ctx);
        Ok(())
    }

    fn key_down_event(&mut self, ctx: &mut Context, keycode: Keycode, _keymod: Mod, _repeat: bool) {
        // self.input.update_keydown(keycode);
        if let Some(ev) = self.input_binding.resolve(keycode) {
            self.scenes.input(ev, true);
        }
    }


    fn key_up_event(&mut self, ctx: &mut Context, keycode: Keycode, _keymod: Mod, _repeat: bool) {

        if let Some(ev) = self.input_binding.resolve(keycode) {
            self.scenes.input(ev, false);
        }
    }
}


use std::env;

pub fn main() {
    let mut cb = ContextBuilder::new("rk2", "noctis-productions")
        .window_setup(conf::WindowSetup::default()
                      .title("RK2")
        )
        .window_mode(conf::WindowMode::default()
                     .dimensions(800, 600)
    );


    // We add the CARGO_MANIFEST_DIR/resources to the filesystems paths so
    // we we look in the cargo project for files.
    // And save it so we can feed there result into warmy
    let cargo_path: Option<path::PathBuf> = option_env!("CARGO_MANIFEST_DIR")
        .map(|env_path| {
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

    let state = &mut MainState::new(cargo_path, ctx);
    if let Err(e) = event::run(ctx, state) {
        println!("Error encountered: {}", e);
    } else {
        println!("Game exited cleanly.");
    }   
}
