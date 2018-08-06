// in a real application you would use `fnv`
use std::collections::HashMap;

use failure;

use specs::{Builder, Join, ReadStorage, World};

use shred::cell::{Ref, RefMut};
use shred::{
    Accessor, AccessorCow, CastFrom, DispatcherBuilder, DynamicSystemData, MetaTable, Read,
    Resource, ResourceId, Resources, System, SystemData,
};

use gluon::{
    base::types::ArcType,
    new_vm,
    vm::{
        api::{Getable, Hole, OpaqueValue, OwnedFunction, Pushable, ValueRef},
        internal::InternedStr,
        thread::ThreadInternal,
        Variants,
    },
    Compiler, RootedThread, Thread,
};

type GluonAny = OpaqueValue<RootedThread, Hole>;

struct Dependencies {
    read_type: ArcType,
    write_type: ArcType,
    thread: RootedThread,
    reads: Vec<ResourceId>,
    writes: Vec<ResourceId>,
}

impl Accessor for Dependencies {
    fn try_new() -> Option<Self> {
        // there's no default for this
        None
    }

    fn reads(&self) -> Vec<ResourceId> {
        let mut reads = self.reads.clone();
        reads.push(ResourceId::new::<ReflectionTable>());

        reads
    }

    fn writes(&self) -> Vec<ResourceId> {
        self.writes.clone()
    }
}

/// A dynamic system that represents and calls the script.
struct DynamicSystem {
    dependencies: Dependencies,
    read_type: ArcType,
    write_type: ArcType,
    script: OwnedFunction<fn(GluonAny) -> GluonAny>,
}

impl<'a> System<'a> for DynamicSystem {
    type SystemData = ScriptSystemData<'a>;

    fn run(&mut self, data: Self::SystemData) {
        println!("Run dynamic");
        let meta = data.meta_table;
        let mut writes = data.writes;

        let input = {
            let reads: Vec<&Reflection> = data.reads.iter().map(|resource| {
            // explicitly use the type because we're dealing with `&Resource` which is implemented
            // by a lot of types; we don't want to accidentally get a `&Box<Resource>` and cast
            // it to a `&Resource`.
            match resource{
                ReadType::Read(resource) => {
                    let res = Box::as_ref(resource);

                    meta.get(res).expect("Not registered in meta table")}
                ReadType::Write(i) => {
                    let res = Box::as_ref(&writes[*i]);

                    meta.get(res).expect("Not registered in meta table")
                }
            }
        }).collect();

            let thread = self.script.vm();
            for read in &reads {
                read.to_gluon(thread);
            }

            thread
                .context()
                .push_new_record(thread, reads.len(), &data.read_fields)
                .unwrap();
            let mut context = self.script.vm().current_context();
            let variant = context.pop();
            <GluonAny as Getable>::from_value(self.script.vm(), *variant)
        };

        let writes: Vec<&mut Reflection> = writes
            .iter_mut()
            .map(|resource| {
                // explicitly use the type because we're dealing with `&mut Resource` which is
                // implemented by a lot of types; we don't want to accidentally get a
                // `&mut Box<Resource>` and cast it to a `&mut Resource`.
                let res = Box::as_mut(resource);

                // For some reason this needs a type ascription, otherwise Rust will think it's
                // a `&mut (Reflection + '_)` (as opposed to `&mut (Reflection + 'static)`.
                let res: &mut Reflection = meta.get_mut(res).expect("Not registered in meta table");

                res
            }).collect();

        // call the script with the input
        let value = self.script.call(input).unwrap();
        match value.get_variant().as_ref() {
            ValueRef::Data(data) => for (variant, write) in data.iter().zip(writes) {
                write.from_gluon(self.script.vm(), variant);
            },
            _ => panic!(),
        }
    }

    fn accessor<'b>(&'b self) -> AccessorCow<'a, 'b, Self> {
        AccessorCow::Ref(&self.dependencies)
    }

    fn setup(&mut self, _res: &mut Resources) {
        // this could call a setup function of the script
    }
}

/// Some trait that all of your dynamic resources should implement.
/// This trait should be able to register / transfer it to the scripting framework.
trait Reflection {
    fn to_gluon(&self, thread: &Thread);

    fn from_gluon(&mut self, thread: &Thread, variants: Variants);
}

impl<T> Reflection for T
where
    T: for<'vm> Pushable<'vm> + for<'vm, 'value> Getable<'vm, 'value>,
    T: Clone,
{
    fn to_gluon(&self, thread: &Thread) {
        Pushable::push(self.clone(), &mut thread.current_context()).unwrap()
    }

    fn from_gluon(&mut self, thread: &Thread, variants: Variants) {
        *self = Getable::from_value(thread, variants)
    }
}

// necessary for `MetaTable`
impl<T> CastFrom<T> for Reflection
where
    T: Reflection + 'static,
{
    fn cast(t: &T) -> &Self {
        t
    }

    fn cast_mut(t: &mut T) -> &mut Self {
        t
    }
}

type ReflectionTable = MetaTable<Reflection>;

/// Maps resource names to resource ids.
struct ResourceTable {
    map: HashMap<String, ResourceId>,
}

impl ResourceTable {
    fn new() -> Self {
        ResourceTable {
            map: HashMap::default(),
        }
    }

    fn register<T: Resource>(&mut self, name: &str) {
        self.map.insert(name.to_owned(), ResourceId::new::<T>());
    }

    fn get(&self, name: &str) -> ResourceId {
        *self
            .map
            .get(name)
            .unwrap_or_else(|| panic!("Expected resource `{}`", name))
    }
}

enum ReadType<'a> {
    Write(usize),
    Read(Ref<'a, Box<Resource + 'static>>),
}

struct ScriptSystemData<'a> {
    meta_table: Read<'a, ReflectionTable>,
    read_fields: Vec<InternedStr>,
    reads: Vec<ReadType<'a>>,
    writes: Vec<RefMut<'a, Box<Resource + 'static>>>,
}

impl<'a> DynamicSystemData<'a> for ScriptSystemData<'a> {
    type Accessor = Dependencies;

    fn setup(_accessor: &Dependencies, _res: &mut Resources) {}

    fn fetch(access: &Dependencies, res: &'a Resources) -> Self {
        let writes = access
            .writes
            .iter()
            .map(|id| id.0)
            .map(|id| {
                res.try_fetch_internal(id)
                    .expect("bug: the requested resource does not exist")
                    .borrow_mut()
            }).collect();
        let reads = access
            .reads
            .iter()
            .map(|id| {
                if let Some(i) = access.writes.iter().position(|write_id| write_id == id) {
                    ReadType::Write(i)
                } else {
                    ReadType::Read(
                        res.try_fetch_internal(id.0)
                            .expect("bug: the requested resource does not exist")
                            .borrow(),
                    )
                }
            }).collect();

        let read_fields = access
            .read_type
            .row_iter()
            .map(|field| {
                access
                    .thread
                    .global_env()
                    .intern(field.name.declared_name())
            }).collect::<Result<_, _>>()
            .unwrap();

        ScriptSystemData {
            meta_table: SystemData::fetch(res),
            read_fields,
            reads,
            writes,
        }
    }
}

fn create_script_sys(thread: &Thread, res: &Resources) -> DynamicSystem {
    // -- how we create the system --
    let table = res.fetch::<ResourceTable>();

    let update_type = thread.get_global_type("update").unwrap();
    let (read_type, write_type) = match update_type.as_function() {
        Some(x) => x,
        None => panic!(),
    };
    let reads = read_type
        .row_iter()
        .map(|field| field.name.declared_name())
        .collect::<Vec<_>>();
    let writes = write_type
        .row_iter()
        .map(|field| field.name.declared_name())
        .collect::<Vec<_>>();
    let function = thread
        .get_global::<OwnedFunction<fn(GluonAny) -> GluonAny>>("update")
        .unwrap();

    let sys = DynamicSystem {
        dependencies: Dependencies {
            thread: thread.root_thread(),
            read_type: read_type.clone(),
            write_type: write_type.clone(),
            reads: reads.iter().map(|r| table.get(r)).collect(),
            writes: writes.iter().map(|r| table.get(r)).collect(),
        },
        read_type: read_type.clone(),
        write_type: write_type.clone(),
        // just pass the function pointer
        script: function,
    };

    sys
}

pub fn main() -> Result<(), failure::Error> {
    /// Some resource
    #[derive(Debug, Default, Getable, Pushable, Clone, Component)]
    struct Pos {
        x: f64,
        y: f64,
    }

    /// Another resource
    #[derive(Debug, Default, Getable, Pushable, Clone, Component)]
    struct Bar;

    struct NormalSys;

    impl<'a> System<'a> for NormalSys {
        type SystemData = (ReadStorage<'a, Pos>, ReadStorage<'a, Bar>);

        fn run(&mut self, (foo, bar): Self::SystemData) {
            for (foo, bar) in (&foo, &bar).join() {
                println!("Fetched foo: {:?}", &foo as &Pos);
                println!("Fetched bar: {:?}", &bar as &Bar);
            }
        }
    }

    let mut world = World::new();
    {
        let mut table = world.res.entry().or_insert_with(|| ReflectionTable::new());

        table.register(&Pos { x: 1., y: 2. });
        table.register(&Bar);
    }

    {
        let mut table = world.res.entry().or_insert_with(|| ResourceTable::new());
        table.register::<Pos>("pos");
        table.register::<Bar>("bar");
    }
    world.register::<Pos>();
    world.register::<Bar>();

    world.create_entity().with(Pos { x: 1., y: 2. }).build();
    world.create_entity().with(Pos { x: 1., y: 2. }).build();

    let mut dispatcher = DispatcherBuilder::new()
        .with(NormalSys, "normal", &[])
        .build();
    dispatcher.setup(&mut world.res);

    let vm = new_vm();
    let script = r#"
    type Pos = { x : Float, y : Float }
    let update r : { pos : Pos } -> { pos: Pos } =
        let { pos } = r
        { pos = { x = pos.x, y = pos.y + 1.0 } }

    update
    "#;
    Compiler::new().load_script(&vm, "update", script)?;
    let script0 = create_script_sys(&vm, &world.res);

    // it is recommended you create a second dispatcher dedicated to scripts,
    // that'll allow you to rebuild if necessary
    let mut scripts = DispatcherBuilder::new()
        .with(script0, "script0", &[])
        .build();
    scripts.setup(&mut world.res);

    // Game loop
    for _ in 0..3 {
        dispatcher.dispatch(&mut world.res);
        scripts.dispatch(&mut world.res);
    }
    Ok(())
}
