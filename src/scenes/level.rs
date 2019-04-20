use std::fs;

use ggez;
use ggez::graphics;
use ggez_goodies::scene;
use specs::{self, Join};
use warmy;

use gluon_system;

use components as c;
use input;
use resources;
use scenes::*;
use systems::*;
use world::World;

pub struct LevelScene {
    done: bool,
    kiwi: warmy::Res<resources::Image>,
    dispatcher: specs::Dispatcher<'static, 'static>,
}

impl LevelScene {
    pub fn new(ctx: &mut ggez::Context, world: &mut World) -> Result<Self, failure::Error> {
        let done = false;
        let kiwi = world
            .assets
            .get::<_, resources::Image>(&warmy::FSKey::new("/images/kiwi.png"), ctx)
            .unwrap();

        let thread = gluon::VmBuilder::new()
            .import_paths(Some(vec!["src".into()]))
            .build();

        gluon_system::init_resources(&mut world.specs_world.res, &thread);

        gluon::Compiler::new().run_expr::<()>(
            &thread,
            "",
            "let _ = import! gluon_component in ()",
        )?;

        use components::*;
        register_components! {world.specs_world, thread,
            Position,
            Motion
        };

        let dispatcher = Self::register_systems(&thread, world)?;
        Ok(LevelScene {
            done,
            kiwi,
            dispatcher,
        })
    }

    fn register_systems(
        thread: &gluon::Thread,
        world: &World,
    ) -> Result<specs::Dispatcher<'static, 'static>, failure::Error> {
        let script = fs::read_to_string("src/gluon_system.glu")?;
        let (function, typ) = gluon::Compiler::new().run_expr(thread, "update", &script)?;
        let gluon_system =
            gluon_system::create_script_system(thread, &world.specs_world.res, function, &typ)?;

        Ok(specs::DispatcherBuilder::new()
            .with(MovementSystem, "sys_movement", &[])
            .with(gluon_system, "script", &[])
            .build())
    }
}

impl scene::Scene<World, input::InputEvent> for LevelScene {
    fn update(&mut self, gameworld: &mut World) -> FSceneSwitch {
        self.dispatcher.dispatch(&mut gameworld.specs_world.res);
        if self.done {
            scene::SceneSwitch::Pop
        } else {
            scene::SceneSwitch::None
        }
    }

    fn draw(&mut self, gameworld: &mut World, ctx: &mut ggez::Context) -> ggez::GameResult<()> {
        let pos = gameworld.specs_world.read_storage::<c::Position>();
        for p in pos.join() {
            graphics::draw(ctx, &(self.kiwi.borrow().0), p.0, 0.0)?;
        }
        Ok(())
    }

    fn name(&self) -> &str {
        "LevelScene"
    }

    fn input(&mut self, gameworld: &mut World, ev: input::InputEvent, _started: bool) {
        debug!("Input: {:?}", ev);
        if gameworld.input.get_button_pressed(input::Button::Menu) {
            self.done = true;
        }
    }
}
