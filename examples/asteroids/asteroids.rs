use std::fs;

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
#[gluon(vm_type = "component.Position")]
struct Position {
    x: f32,
    y: f32,
}
gluon_specs::impl_clone_marshal!(Position);

#[derive(Clone, Debug, Component, VmType, Getable, Pushable)]
#[gluon(vm_type = "component.Rotation")]
struct Rotation(f32);
gluon_specs::impl_clone_marshal!(Rotation);

pub struct Asteroids {
    thread: gluon::RootedThread,
    script_dispatcher: specs::Dispatcher<'static, 'static>,
}

impl Asteroids {
    pub fn new() -> Self {
        Asteroids {
            thread: gluon::VmBuilder::new()
                .import_paths(Some(vec!["examples/asteroids".into()]))
                .build(),
            script_dispatcher: specs::DispatcherBuilder::new().build(),
        }
    }
}

impl SimpleState for Asteroids {
    fn on_start(&mut self, data: StateData<'_, GameData<'_, '_>>) {
        let world = data.world;

        gluon_specs::init_resources(&mut world.res, &self.thread);

        world.register::<Player>();

        gluon::Compiler::new()
            .load_file(&self.thread, "component.glu")
            .unwrap_or_else(|err| panic!("{}", err));
        gluon_specs::register_component::<Position>(world, &self.thread, "Position");
        gluon_specs::register_component::<Rotation>(world, &self.thread, "Rotation");

        initialise_camera(world);
        initialise_player(world);

        let script_system = gluon_specs::ScriptSystem::from_script(
            &self.thread,
            &world.res,
            &fs::read_to_string("examples/asteroids/system.glu")
                .unwrap_or_else(|err| panic!("{}", err)),
        )
        .unwrap_or_else(|err| panic!("{}", err));
        self.script_dispatcher = specs::DispatcherBuilder::new()
            .with(script_system, "script_system", &[])
            .with(PositionSystem, "position_system", &["script_system"])
            .build();
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

    fn update(&mut self, data: &mut StateData<GameData>) -> SimpleTrans {
        self.script_dispatcher.dispatch(&data.world.res);
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
        .with(Position {
            x: transform.translation().x,
            y: transform.translation().y,
        })
        .with(Rotation(0.0))
        .with(transform)
        .with(Player)
        .build();
}

use specs::{Join, ReadStorage, System, WriteStorage};

struct PositionSystem;
impl<'s> System<'s> for PositionSystem {
    type SystemData = (
        WriteStorage<'s, Transform>,
        ReadStorage<'s, Position>,
        ReadStorage<'s, Rotation>,
    );

    fn run(&mut self, (mut t, p, r): Self::SystemData) {
        for (transform, position, rotation) in (&mut t, &p, &r).join() {
            transform
                .set_x(position.x)
                .set_y(position.y)
                .set_rotation_euler(0., 0., rotation.0);
        }
    }
}
