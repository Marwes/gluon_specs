//use ggez::nalgebra as na;
use ggez::graphics::*;
use specs::*;

use gluon::{
    vm::{
        self,
        api::{ActiveThread, Getable, Pushable},
    },
    Thread,
};

//use util::*;

#[derive(Getable, Pushable, VmType)]
struct Vec2 {
    x: f32,
    y: f32,
}

impl Into<Vector2> for Vec2 {
    fn into(self) -> Vector2 {
        Vector2::new(self.x, self.y)
    }
}

#[derive(Getable, Pushable)]
struct MotionMarshal {
    pub velocity: Vec2,
    pub acceleration: Vec2,
}

/// ///////////////////////////////////////////////////////////////////////
/// Components
/// ///////////////////////////////////////////////////////////////////////
#[derive(Clone, Debug, Component, VmType)]
#[gluon(vm_type = "gluon_component.Position")]
#[component(VecStorage)]
pub struct Position(pub Point2);

impl<'vm, 'value> Pushable<'vm> for Position {
    fn push(self, context: &mut ActiveThread<'vm>) -> vm::Result<()> {
        (Vec2 {
            x: self.0[0],
            y: self.0[1],
        })
        .push(context)
    }
}

impl<'vm, 'value> Getable<'vm, 'value> for Position {
    impl_getable_simple!();
    fn from_value(thread: &'vm Thread, value: vm::Variants<'value>) -> Self {
        let Vec2 { x, y } = Vec2::from_value(thread, value);
        Self(Point2::new(x, y))
    }
}

impl_clone_marshal!(Position);

#[derive(Clone, Debug, Component, VmType)]
#[gluon(vm_type = "gluon_component.Motion")]
#[component(VecStorage)]
pub struct Motion {
    pub velocity: Vector2,
    pub acceleration: Vector2,
}

impl<'vm, 'value> Pushable<'vm> for Motion {
    fn push(self, context: &mut ActiveThread<'vm>) -> vm::Result<()> {
        MotionMarshal {
            velocity: Vec2 {
                x: self.velocity.x,
                y: self.velocity.y,
            },
            acceleration: Vec2 {
                x: self.acceleration.x,
                y: self.acceleration.y,
            },
        }
        .push(context)
    }
}

impl<'vm, 'value> Getable<'vm, 'value> for Motion {
    impl_getable_simple!();
    fn from_value(thread: &'vm Thread, value: vm::Variants<'value>) -> Self {
        let MotionMarshal {
            velocity,
            acceleration,
        } = Getable::from_value(thread, value);
        Motion {
            velocity: Vec2::into(velocity),
            acceleration: Vec2::into(acceleration),
        }
    }
}
impl_clone_marshal!(Motion);

// Just a marker that a particular entity is the player.
#[derive(Clone, Debug, Default, Component)]
#[component(NullStorage)]
pub struct Player;

#[derive(Clone, Debug, Default, Component)]
#[component(VecStorage)]
pub struct Shot {
    pub damage: u32,
}

#[derive(Clone, Debug, Component)]
#[component(HashMapStorage)]
pub struct CBackgroundScroller {
    pub scroll_speed: Vector2,
}

impl CBackgroundScroller {
    //pub fn new() -> Self {
    //    CBackgroundScroller { scroll_speed: Vector2::new(0.0, -0.01) }
    //}
}
