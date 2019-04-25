use amethyst::{
    assets::{AssetStorage, Loader},
    core::transform::Transform,
    input::Button,
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

#[derive(Clone, Debug, Default, VmType, Getable, Pushable)]
#[gluon(vm_type = "component.Vec2")]
struct Vec2 {
    x: f32,
    y: f32,
}

#[derive(Clone, Debug, Component, VmType, Getable, Pushable)]
#[gluon(vm_type = "component.Position")]
struct Position(Vec2);
gluon_specs::impl_clone_marshal!(Position);

#[derive(Clone, Debug, Default, Component, VmType, Getable, Pushable)]
#[gluon(vm_type = "component.Motion")]
struct Motion {
    velocity: Vec2,
    acceleration: Vec2,
}
gluon_specs::impl_clone_marshal!(Motion);

#[derive(Clone, Debug, Component, VmType, Getable, Pushable)]
#[gluon(vm_type = "component.Rotation")]
struct Rotation(f32);
gluon_specs::impl_clone_marshal!(Rotation);

#[derive(Clone, Debug, Component, VmType, Getable, Pushable)]
#[gluon(newtype)]
enum Direction {
    Left,
    Right,
    None,
}
gluon_specs::impl_clone_marshal!(Direction);
gluon_specs::impl_reflection!(Direction);

impl Default for Direction {
    fn default() -> Self {
        Direction::None
    }
}

#[derive(Clone, Debug, Default, Component, VmType, Getable, Pushable)]
#[gluon(newtype)]
struct Input {
    direction: Direction,
    shoot: bool,
}
gluon_specs::impl_clone_marshal!(Input);
gluon_specs::impl_reflection!(Input);

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

        world
            .res
            .fetch_mut::<amethyst::core::frame_limiter::FrameLimiter>()
            .set_rate(
                amethyst::core::frame_limiter::FrameRateLimitStrategy::SleepAndYield(
                    ::std::time::Duration::from_millis(1),
                ),
                60,
            );

        gluon_specs::init_resources(&mut world.res, &self.thread);

        world.register::<Player>();

        initialise_gluon(&self.thread);
        gluon::Compiler::new()
            .load_file(&self.thread, "component.glu")
            .unwrap_or_else(|err| panic!("{}", err));

        gluon_specs::register_component::<Position>(world, &self.thread, "Position");
        gluon_specs::register_component::<Motion>(world, &self.thread, "Motion");
        gluon_specs::register_component::<Rotation>(world, &self.thread, "Rotation");
        gluon_specs::register::<Input>(&mut world.res, &self.thread, "Input");
        world.res.entry().or_insert_with(Input::default);

        initialise_camera(world);
        initialise_player(world);

        let mut dispatcher_builder =
            specs::DispatcherBuilder::new().with(InputSystem, "input_system", &[]);

        dispatcher_builder.add_barrier();

        for entry in walkdir::WalkDir::new("examples/asteroids/system")
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| e.path().extension().and_then(|s| s.to_str()) == Some("glu"))
        {
            let script_system =
                gluon_specs::ScriptSystem::from_file(&self.thread, &world.res, entry.path())
                    .unwrap_or_else(|err| panic!("{}", err));

            dispatcher_builder.add(script_system, &entry.path().display().to_string(), &[]);
        }

        dispatcher_builder.add_barrier();

        self.script_dispatcher = dispatcher_builder
            .with(PositionSystem, "position_system", &[])
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

    fn fixed_update(&mut self, data: StateData<GameData>) -> SimpleTrans {
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
        .with(Position(Vec2 {
            x: transform.translation().x,
            y: transform.translation().y,
        }))
        .with(Motion::default())
        .with(Rotation(0.0))
        .with(transform)
        .with(Player)
        .build();
}

fn initialise_gluon(thread: &gluon::Thread) {
    use gluon::vm::{self, record, ExternModule};
    fn load(thread: &gluon::Thread) -> vm::Result<ExternModule> {
        ExternModule::new(
            thread,
            record! {
                type Direction => Direction,
                type Input => Input,
            },
        )
    }

    gluon::import::add_extern_module(thread, "component_prim", load);
}

use specs::{Join, Read, ReadStorage, System, Write, WriteStorage};

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
                .set_x(position.0.x)
                .set_y(position.0.y)
                .set_rotation_euler(0., 0., rotation.0);
        }
    }
}

struct InputSystem;
impl<'s> System<'s> for InputSystem {
    type SystemData = (Read<'s, crate::InputHandler>, Write<'s, Input>);

    fn run(&mut self, (input_handler, mut input): Self::SystemData) {
        input.direction = if input_handler.button_is_down(Button::Key(VirtualKeyCode::Left)) {
            Direction::Left
        } else if input_handler.button_is_down(Button::Key(VirtualKeyCode::Right)) {
            Direction::Right
        } else {
            Direction::None
        };
        input.shoot = input_handler.button_is_down(Button::Key(VirtualKeyCode::Space));
    }
}
