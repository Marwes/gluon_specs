use amethyst::{
    assets::{AssetStorage, Loader},
    core::transform::Transform,
    prelude::*,
    renderer::{
        Camera, Event, KeyboardInput, PngFormat, Projection, SpriteRender, SpriteSheet,
        SpriteSheetFormat, Texture, TextureMetadata, VirtualKeyCode, WindowEvent,
    },
};

use gluon_codegen::{Getable, Pushable, VmType};
use specs_derive::Component;

pub const ARENA_HEIGHT: f32 = 300.0;
pub const ARENA_WIDTH: f32 = 300.0;

#[derive(Component, VmType, Getable, Pushable)]
#[gluon(newtype)]
struct Player;

#[derive(Clone, Debug, Component, VmType, Getable, Pushable)]
#[gluon(newtype)]
struct Position {
    x: f64,
    y: f64,
}
gluon_specs::impl_clone_marshal!(Position);

pub struct Asteroids {
    thread: gluon::RootedThread,
}

impl Asteroids {
    pub fn new() -> Self {
        Asteroids {
            thread: gluon::new_vm(),
        }
    }
}

impl SimpleState for Asteroids {
    fn on_start(&mut self, data: StateData<'_, GameData<'_, '_>>) {
        let world = data.world;

        gluon_specs::init_resources(&mut world.res, &self.thread);

        world.register::<Player>();
        gluon_specs::register_component::<Position>(world, &self.thread, "Position");

        initialise_camera(world);
        initialise_player(world);
    }

    fn handle_event(&mut self, _: StateData<GameData>, event: StateEvent) -> SimpleTrans {
        if let StateEvent::Window(event) = &event {
            match event {
                Event::WindowEvent { event, .. } => match event {
                    WindowEvent::KeyboardInput {
                        input:
                            KeyboardInput {
                                virtual_keycode: Some(VirtualKeyCode::Escape),
                                ..
                            },
                        ..
                    }
                    | WindowEvent::CloseRequested => Trans::Quit,
                    _ => Trans::None,
                },
                _ => Trans::None,
            }
        } else {
            Trans::None
        }
    }

    fn update(&mut self, _: &mut StateData<GameData>) -> SimpleTrans {
        Trans::None
    }
}

fn initialise_camera(world: &mut World) {
    let mut transform = Transform::default();
    transform.set_z(1.0);
    world
        .create_entity()
        .with(Camera::from(Projection::orthographic(
            0.0,
            ARENA_WIDTH,
            0.0,
            ARENA_HEIGHT,
        )))
        .with(transform)
        .build();
}

fn initialise_player(world: &mut World) {
    let texture_handle = {
        let loader = world.read_resource::<Loader>();
        let texture_storage = world.read_resource::<AssetStorage<Texture>>();
        loader.load(
            "texture/asteroids_spritesheet.png",
            PngFormat,
            TextureMetadata::srgb_scale(),
            (),
            &texture_storage,
        )
    };

    let sprite_sheet = {
        let loader = world.read_resource::<Loader>();
        let sprite_sheet_store = world.read_resource::<AssetStorage<SpriteSheet>>();
        loader.load(
            "texture/asteroids_spritesheet.ron", // Here we load the associated ron file
            SpriteSheetFormat,
            texture_handle, // We pass it the handle of the texture we want it to use
            (),
            &sprite_sheet_store,
        )
    };

    let mut transform = Transform::default();
    transform.set_x(ARENA_WIDTH / 2.).set_y(ARENA_HEIGHT / 2.);

    world
        .create_entity()
        .with(SpriteRender {
            sprite_sheet,
            sprite_number: 0,
        })
        .with(transform)
        .with(Player)
        .build();
}
