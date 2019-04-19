// in a real application you would use `fnv`
use std::{collections::HashMap, fmt, fs, mem};

use failure;

use itertools::Itertools;

use specs::{
    storage::{MaskedStorage, Storage, UnprotectedStorage},
    world::EntitiesRes,
    BitSet, Builder, Component, Entity, Join, LazyUpdate, ReadStorage, World, WriteStorage,
};

use shred::cell::{Ref, RefMut};
use shred::{
    self, Accessor, AccessorCow, CastFrom, DispatcherBuilder, DynamicSystemData, Fetch, MetaTable,
    Read, Resource, ResourceId, System, SystemData,
};

use gluon::{
    base::types::{ArcType, Type, TypeExt},
    import::add_extern_module,
    vm::{
        self,
        api::{
            generic, scoped, Getable, Hole, OpaqueRef, OpaqueValue, OwnedFunction, Pushable,
            RuntimeResult, Userdata, ValueRef, VmType, WithVM,
        },
        internal::InternedStr,
        thread::ThreadInternal,
        ExternModule, Variants,
    },
    Compiler, RootedThread, Thread, VmBuilder,
};

type GluonAny = OpaqueValue<RootedThread, Hole>;

/// Some trait that all of your dynamic resources should implement.
/// This trait should be able to register / transfer it to the scripting framework.
trait GluonMarshalTo {
    fn to_gluon<'a>(&'a self, thread: &Thread, proxies: &mut Vec<Box<Dropbox + 'a>>);
}

trait GluonMarshalFrom: GluonMarshalTo {
    fn from_gluon(&mut self, thread: &Thread, variants: Variants);

    fn new_value(thread: &Thread, variants: Variants) -> Self
    where
        Self: Sized;
}

impl<T> GluonMarshalTo for T
where
    T: Userdata + VmType,
    T: ::std::fmt::Debug,
{
    fn to_gluon<'a>(&'a self, thread: &Thread, proxies: &mut Vec<Box<Dropbox + 'a>>) {
        eprintln!("Push {:?}", self);
        let mut proxy = scoped::Ref::new(self);
        Pushable::push(&mut proxy, &mut thread.current_context()).unwrap();
        proxies.push(Box::new(proxy));
    }
}

macro_rules! impl_clone_marshal {
    ($ty: ty) => {
        impl GluonMarshalTo for $ty {
            fn to_gluon<'a>(&'a self, thread: &Thread, _proxies: &mut Vec<Box<Dropbox + 'a>>) {
                eprintln!("Push {:?}", self);
                Pushable::push(self.clone(), &mut thread.current_context()).unwrap()
            }
        }

        impl GluonMarshalFrom for $ty {
            fn from_gluon(&mut self, thread: &Thread, variants: Variants) {
                *self = Self::new_value(thread, variants);
            }

            fn new_value(thread: &Thread, variants: Variants) -> Self {
                let self_ = Getable::from_value(thread, variants);
                eprintln!("Return {:?}", self_);
                self_
            }
        }
    };
}

#[derive(Debug, Clone, Default, Getable, Pushable, VmType)]
struct DeltaTime(f64);

impl_clone_marshal!(DeltaTime);

#[derive(Userdata, VmType)]
#[gluon(vm_type = "EntitiesRes")]
#[repr(transparent)]
struct GluonEntities(EntitiesRes);

impl fmt::Debug for GluonEntities {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "GluonEntities(..)")
    }
}

#[derive(Userdata, Default, VmType)]
#[gluon(vm_type = "LazyUpdate")]
#[repr(transparent)]
struct GluonLazyUpdate(LazyUpdate);

impl fmt::Debug for GluonLazyUpdate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "LazyUpdate(..)")
    }
}

/// Some resource
#[derive(Debug, Default, Clone, Getable, Pushable, Component, VmType)]
#[gluon(vm_type = "gluon_component.Pos")]
struct Pos {
    x: f64,
    y: f64,
}
impl_clone_marshal!(Pos);

#[derive(Debug, Default, Clone, Getable, Pushable, Component, VmType)]
#[gluon(vm_type = "gluon_component.Vel")]
struct Vel {
    x: f64,
    y: f64,
}
impl_clone_marshal!(Vel);

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
        reads.push(ResourceId::new::<EntitiesRes>());

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
        let entities = data.entities;
        let mut writes = data.writes;

        let mask;

        let mut outputs = Vec::new();
        {
            let reads: Vec<&Reflection> = data
                .reads
                .iter()
                .map(|resource| {
                    // explicitly use the type because we're dealing with `&Resource` which is implemented
                    // by a lot of types; we don't want to accidentally get a `&Box<Resource>` and cast
                    // it to a `&Resource`.
                    match resource {
                        ReadType::Read(resource) => {
                            let res = Box::as_ref(resource);

                            meta.reflections
                                .get(res)
                                .expect("Not registered in meta table")
                        }
                        ReadType::Write(i) => {
                            let res = Box::as_ref(&writes[*i]);

                            meta.reflections
                                .get(res)
                                .expect("Not registered in meta table")
                        }
                    }
                })
                .collect();
            mask = reflection_bitset(
                reads.iter().cloned().chain(writes.iter().map(|resource| {
                    let res = Box::as_ref(resource);

                    let res: &Reflection = meta
                        .reflections
                        .get(res)
                        .expect("Not registered in meta table");

                    res
                })),
                entities.clone(),
            );

            let thread = self.script.vm().root_thread();

            for input in (GluonJoin {
                reads: &reads,
                entities: entities.clone(),
                thread: &thread,
                fields: &data.read_fields,
                mask: &mask,
                proxies: Vec::new(),
            })
            .join()
            {
                let value = self.script.call(input).unwrap();
                Vec::push(&mut outputs, value);
            }
        }

        let mut writes: Vec<&mut ReflectionMut> = writes
            .iter_mut()
            .map(|resource| {
                let res = Box::as_mut(resource);

                let res: &mut ReflectionMut = meta
                    .reflections_mut
                    .get_mut(res)
                    .expect("Not registered in meta table");

                res
            })
            .collect();

        outputs.reverse();
        let writes = GluonJoinMut(&mut writes, outputs, entities.clone(), &mask);
        for () in writes.join() {}
    }

    fn accessor<'b>(&'b self) -> AccessorCow<'a, 'b, Self> {
        AccessorCow::Ref(&self.dependencies)
    }

    fn setup(&mut self, _res: &mut shred::World) {
        // this could call a setup function of the script
    }
}

trait Reflection {
    unsafe fn open(&self, entities: Fetch<EntitiesRes>) -> Option<&BitSet>;
    unsafe fn get(&self, entities: Fetch<EntitiesRes>, index: u32) -> &GluonMarshalTo;
}

trait ReflectionMut: Reflection {
    unsafe fn get_mut(&mut self, entities: Fetch<EntitiesRes>, index: u32)
        -> &mut GluonMarshalFrom;
    fn add_component(lazy: &LazyUpdate, thread: &Thread, entity: Entity, value: Variants)
    where
        Self: Sized,
    {
        let _ = (lazy, thread, entity, value);
        unimplemented!()
    }
}

unsafe fn forget_lifetime<'a, 'b, T: ?Sized>(x: &'a T) -> &'b T {
    ::std::mem::transmute(x)
}

unsafe fn forget_lifetime_mut<'a, 'b, T: ?Sized>(x: &'a mut T) -> &'b mut T {
    ::std::mem::transmute(x)
}

macro_rules! impl_reflection {
    ($($ty: ty),*) => {
        $(
            impl Reflection for $ty {
                unsafe fn open(&self, _entities: Fetch<EntitiesRes>) -> Option<&BitSet> {
                    None
                }

                unsafe fn get(&self, _entities: Fetch<EntitiesRes>, _index: u32) -> &GluonMarshalTo {
                    self
                }
            }
        )*
    }
}

impl_reflection! { DeltaTime, ResourceTable, ReflectionTable, GluonEntities, GluonLazyUpdate }

impl Reflection for EntitiesRes {
    unsafe fn open(&self, _entities: Fetch<EntitiesRes>) -> Option<&BitSet> {
        None
    }

    unsafe fn get(&self, _entities: Fetch<EntitiesRes>, _index: u32) -> &GluonMarshalTo {
        mem::transmute::<_, &GluonEntities>(self)
    }
}

impl Reflection for LazyUpdate {
    unsafe fn open(&self, _entities: Fetch<EntitiesRes>) -> Option<&BitSet> {
        None
    }

    unsafe fn get(&self, _entities: Fetch<EntitiesRes>, _index: u32) -> &GluonMarshalTo {
        mem::transmute::<_, &GluonLazyUpdate>(self)
    }
}

impl<T> Reflection for MaskedStorage<T>
where
    T: Component + GluonMarshalTo + Send + Sync,
{
    unsafe fn open(&self, entities: Fetch<EntitiesRes>) -> Option<&BitSet> {
        // mask is actually bound to `self`
        Some(forget_lifetime(Storage::new(entities, self).mask()))
    }

    unsafe fn get(&self, entities: Fetch<EntitiesRes>, index: u32) -> &GluonMarshalTo {
        forget_lifetime(
            Storage::new(entities, self)
                .unprotected_storage()
                .get(index),
        )
    }
}

impl<T> ReflectionMut for MaskedStorage<T>
where
    T: Component + GluonMarshalFrom + Send + Sync,
{
    unsafe fn get_mut(
        &mut self,
        entities: Fetch<EntitiesRes>,
        index: u32,
    ) -> &mut GluonMarshalFrom {
        forget_lifetime_mut(
            Storage::new(entities, self)
                .unprotected_storage_mut()
                .get_mut(index),
        )
    }

    fn add_component(lazy: &LazyUpdate, thread: &Thread, entity: Entity, value: Variants)
    where
        Self: Sized,
    {
        lazy.insert(entity, T::new_value(thread, value));
    }
}

// Dummy trait to
trait Dropbox {}

impl<T> Dropbox for T {}

struct GluonJoin<'a> {
    reads: &'a [&'a Reflection],
    proxies: Vec<Box<Dropbox + 'a>>,
    entities: Fetch<'a, EntitiesRes>,
    thread: &'a Thread,
    fields: &'a [InternedStr],
    mask: &'a BitSet,
}

impl<'a> Join for GluonJoin<'a> {
    type Type = GluonAny;
    type Value = Self;
    type Mask = &'a BitSet;
    unsafe fn open(self) -> (Self::Mask, Self::Value) {
        (self.mask, self)
    }

    unsafe fn get(value: &mut Self::Value, index: u32) -> Self::Type {
        let thread = value.thread;
        for reflection in value.reads {
            let read = Reflection::get(*reflection, value.entities.clone(), index);
            read.to_gluon(thread, &mut value.proxies);
        }

        thread
            .context()
            .push_new_record(thread, value.reads.len(), &value.fields)
            .unwrap();
        let mut context = thread.current_context();
        let variant = context.pop();
        <GluonAny as Getable>::from_value(thread, *variant)
    }
}

fn reflection_bitset<'a>(
    iter: impl IntoIterator<Item = &'a Reflection>,
    entities: Fetch<EntitiesRes>,
) -> BitSet {
    unsafe {
        iter.into_iter()
            .flat_map(|reflection| Reflection::open(reflection, entities.clone()))
            .fold(None, |acc, set| {
                Some(match acc {
                    Some(mut acc) => {
                        acc &= set;
                        acc
                    }
                    None => set.clone(),
                })
            })
            .unwrap_or_default()
    }
}

struct GluonJoinMut<'a, 'e>(
    &'a mut [&'a mut ReflectionMut],
    Vec<GluonAny>,
    Fetch<'e, EntitiesRes>,
    &'a BitSet,
);

impl<'a, 'e> Join for GluonJoinMut<'a, 'e> {
    type Type = ();
    type Value = Self;
    type Mask = &'a BitSet;
    unsafe fn open(self) -> (Self::Mask, Self::Value) {
        let mask = self.3;
        (mask, self)
    }

    unsafe fn get(value: &mut Self::Value, index: u32) -> Self::Type {
        let GluonJoinMut(ref mut writes, ref mut outputs, ref entities, _) = *value;

        // FIXME Don't rely on the indexes being sequential
        let value = outputs.pop().unwrap();
        let thread = value.vm();
        // call the script with the input
        match value.get_variant().as_ref() {
            ValueRef::Data(data) => {
                for (variant, write) in data.iter().zip(&mut **writes) {
                    write
                        .get_mut(entities.clone(), index)
                        .from_gluon(thread, variant);
                }
            }
            _ => panic!(),
        }
    }
}

#[derive(Userdata, Debug, VmType)]
#[gluon(vm_type = "Entity")]
struct GluonEntity(Entity);

fn create_entity(entities: &GluonEntities) -> GluonEntity {
    GluonEntity(entities.0.create())
}

fn add_component(
    WithVM { vm, value: lazy }: WithVM<&GluonLazyUpdate>,
    reflection_table: &ReflectionTable,
    name: &str,
    component: OpaqueRef<generic::A>,
    entity: OpaqueRef<GluonEntity>,
) -> RuntimeResult<(), String> {
    let entity = entity.0.clone();
    eprintln!("{}", reflection_table.add_component.keys().format(","));
    match reflection_table.add_component.get(name) {
        Some(f) => RuntimeResult::Return(f(&lazy.0, vm, entity, component.get_variant())),
        None => RuntimeResult::Panic(format!("Unknown component `{}`", name)),
    }
}

fn load(vm: &Thread) -> vm::Result<ExternModule> {
    ExternModule::new(
        vm,
        record! {
            create => primitive!(1, create_entity),
            add_component => primitive!(5, add_component)
        },
    )
}

// necessary for `MetaTable`
unsafe impl<T> CastFrom<T> for Reflection
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

unsafe impl<T> CastFrom<T> for ReflectionMut
where
    T: ReflectionMut + 'static,
{
    fn cast(t: &T) -> &Self {
        t
    }

    fn cast_mut(t: &mut T) -> &mut Self {
        t
    }
}

#[derive(Userdata, Default, VmType)]
#[gluon(vm_type = "ReflectionTable")]
struct ReflectionTable {
    reflections: MetaTable<Reflection>,
    reflections_mut: MetaTable<ReflectionMut>,
    add_component: HashMap<String, fn(&LazyUpdate, &Thread, Entity, Variants)>,
}

impl fmt::Debug for ReflectionTable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("ReflectionTable").finish()
    }
}

impl ReflectionTable {
    fn register<R>(&mut self, _name: &str, r: &R)
    where
        R: Reflection + Resource + Sized,
        Reflection: CastFrom<R>,
    {
        self.reflections.register(r);
    }

    fn register_mut<R>(&mut self, name: &str, r: &R)
    where
        R: ReflectionMut + Resource + Sized,
        ReflectionMut: CastFrom<R>,
    {
        self.register(name, r);

        self.reflections_mut.register(r);
        self.add_component
            .insert(name.to_string(), R::add_component);
    }
}

/// Maps resource names to resource ids.
#[derive(Userdata, Debug, Default, VmType)]
#[gluon(vm_type = "ResourceTable")]
struct ResourceTable {
    map: HashMap<ArcType, ResourceId>,
}

impl ResourceTable {
    fn new() -> Self {
        ResourceTable {
            map: HashMap::default(),
        }
    }

    fn register<T: Send + Sync + 'static>(&mut self, name: ArcType) {
        self.map.insert(name.to_owned(), ResourceId::new::<T>());
    }

    fn register_component<T: Component>(&mut self, name: ArcType) {
        self.map
            .insert(name.to_owned(), ResourceId::new::<MaskedStorage<T>>());
    }

    fn get(&self, name: &ArcType) -> Option<&ResourceId> {
        self.map.get(name)
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
    entities: Fetch<'a, EntitiesRes>,
}

impl<'a> DynamicSystemData<'a> for ScriptSystemData<'a> {
    type Accessor = Dependencies;

    fn setup(_accessor: &Dependencies, _res: &mut shred::World) {}

    fn fetch(access: &Dependencies, res: &'a shred::World) -> Self {
        let writes = access
            .writes
            .iter()
            .map(|id| {
                res.try_fetch_internal(id.clone())
                    .unwrap_or_else(|| {
                        panic!("bug: the requested resource does not exist: {:?}", id)
                    })
                    .borrow_mut()
            })
            .collect();
        let reads = access
            .reads
            .iter()
            .map(|id| {
                if let Some(i) = access.writes.iter().position(|write_id| write_id == id) {
                    ReadType::Write(i)
                } else {
                    ReadType::Read(
                        res.try_fetch_internal(id.clone())
                            .unwrap_or_else(|| {
                                panic!("bug: the requested resource does not exist: {:?}", id)
                            })
                            .borrow(),
                    )
                }
            })
            .collect();

        let read_fields = access
            .read_type
            .row_iter()
            .map(|field| {
                access
                    .thread
                    .global_env()
                    .intern(field.name.declared_name())
            })
            .collect::<Result<_, _>>()
            .unwrap();

        let entities = res.fetch();

        ScriptSystemData {
            meta_table: SystemData::fetch(res),
            read_fields,
            reads,
            entities,
            writes,
        }
    }
}

fn create_script_sys(
    thread: &Thread,
    res: &shred::World,
    function: OwnedFunction<fn(GluonAny) -> GluonAny>,
    update_type: &ArcType,
) -> Result<DynamicSystem, failure::Error> {
    // -- how we create the system --
    let table = res.fetch::<ResourceTable>();

    let (read_type, write_type) = match update_type.as_function() {
        Some(x) => x,
        None => return Err(failure::err_msg("Expected function type")),
    };
    match **read_type {
        Type::Record(_) => (),
        _ => {
            return Err(failure::err_msg(format!(
                "Argument type is not a record\nActual: {}",
                read_type
            )))
        }
    }
    match **write_type {
        Type::Record(_) => (),
        _ => {
            return Err(failure::err_msg(format!(
                "Return type is not a record\nActual: {}",
                write_type
            )))
        }
    }
    let reads = read_type
        .row_iter()
        .map(|field| &field.typ)
        .collect::<Vec<_>>();
    let writes = write_type
        .row_iter()
        .map(|field| &field.typ)
        .collect::<Vec<_>>();

    let get_resource = |r| {
        table
            .get(r)
            .cloned()
            .map(|resource| {
                eprintln!("Got resource: {:?} for {}", resource, r);
                resource
            })
            .ok_or_else(|| {
                failure::err_msg(format!(
                    "Missing resource `{}`. Existing resources: [{}]",
                    r,
                    table.map.keys().format(",")
                ))
            })
    };

    let sys = DynamicSystem {
        dependencies: Dependencies {
            thread: thread.root_thread(),
            read_type: read_type.clone(),
            write_type: write_type.clone(),
            reads: reads
                .iter()
                .map(|r| get_resource(r))
                .collect::<Result<_, _>>()?,
            writes: writes
                .iter()
                .map(|r| get_resource(r))
                .collect::<Result<_, _>>()?,
        },
        read_type: read_type.clone(),
        write_type: write_type.clone(),
        // just pass the function pointer
        script: function,
    };

    Ok(sys)
}

fn init_resources(res: &mut shred::World) {
    res.entry().or_insert_with(|| ReflectionTable::default());
    res.entry().or_insert_with(|| ResourceTable::new());
}

macro_rules! register_components {
    ($world: expr, $thread: expr, $($t: ty),+) => {{
        let ref mut world = $world;
        let ref thread = $thread;
        {
            world.res.entry().or_insert_with(|| ReflectionTable::default());
            world.res.entry().or_insert_with(|| ResourceTable::new());
            let mut reflection_table = world.res.fetch_mut::<ReflectionTable>();
            let mut resource_table = world.res.fetch_mut::<ResourceTable>();
            $(
                reflection_table.register_mut(stringify!($t), &MaskedStorage::<$t>::default());
                let typ = <$t>::make_type(thread);
                resource_table.register_component::<$t>(typ);
            )+
        }
        $(
            world.register::<$t>();
        )+
    }}
}

macro_rules! register {
    ($world: expr, $thread: expr, $($t: ty),+) => {{
        let ref mut world = $world;
        let ref thread = $thread;
        {
            world.res.entry().or_insert_with(|| ReflectionTable::default());
            world.res.entry().or_insert_with(|| ResourceTable::new());
            let mut reflection_table = world.res.fetch_mut::<ReflectionTable>();
            let mut resource_table = world.res.fetch_mut::<ResourceTable>();
            $(
                reflection_table.register(stringify!($t), &<$t>::default());
                let typ = thread.get_type::<$t>().unwrap_or_else(|| panic!("`{}` is missing", stringify!($t)));
                resource_table.register::<$t>(typ);
            )+
        }
    }}
}

fn register_struct_type<T>(vm: &Thread, name: &str)
where
    T: VmType + 'static,
{
    let name = ::gluon::base::symbol::Symbol::from(name);
    let typ = <T as ::gluon::vm::api::VmType>::make_type(vm);
    let alias = ::gluon::base::types::Alias::new(name.clone(), vec![], typ.clone());
    vm.register_type_as(name, alias.clone(), ::std::any::TypeId::of::<T>())
        .unwrap();
}

pub fn main() -> Result<(), failure::Error> {
    /// Another resource
    #[derive(Debug, Default, Getable, Pushable, Clone, Component, VmType)]
    struct Bar;

    impl_clone_marshal!(Bar);

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

    struct Movement;

    impl<'a> System<'a> for Movement {
        type SystemData = (WriteStorage<'a, Pos>, ReadStorage<'a, Vel>);

        fn run(&mut self, (mut pos, vel): Self::SystemData) {
            for (mut pos, vel) in (&mut pos, &vel).join() {
                eprintln!("Updating movement {:?}", vel);
                pos.x += vel.x;
                pos.y += vel.y;
            }
        }
    }

    let mut world = World::new();
    init_resources(&mut world.res);

    let vm = VmBuilder::new()
        .import_paths(Some(vec!["src".into()]))
        .build();

    Compiler::new().run_expr::<()>(&vm, "", "let _ = import! gluon_component in ()")?;

    vm.register_type::<EntitiesRes>("EntitiesRes", &[])?;
    vm.register_type::<GluonEntity>("Entity", &[])?;
    vm.register_type::<LazyUpdate>("LazyUpdate", &[])?;
    vm.register_type::<ResourceTable>("ResourceTable", &[])?;
    vm.register_type::<ReflectionTable>("ReflectionTable", &[])?;

    register_struct_type::<DeltaTime>(&vm, "DeltaTime");

    register! {world, vm,
        DeltaTime,
        EntitiesRes,
        LazyUpdate,
        ReflectionTable,
        ResourceTable
    };

    register_components! {world, vm,
        Pos,
        Vel,
        Bar
    };

    world.res.entry().or_insert_with(|| DeltaTime(0.1));

    world
        .create_entity()
        .with(Pos { x: 1., y: 2. })
        .with(Bar)
        .build();
    world
        .create_entity()
        .with(Pos { x: 1., y: 2. })
        .with(Bar)
        .build();

    let mut dispatcher = DispatcherBuilder::new()
        .with(NormalSys, "normal", &[])
        .with(Movement, "movement", &[])
        .build();
    dispatcher.setup(&mut world.res);

    add_extern_module(&vm, "entity", load);

    let script = fs::read_to_string("src/gluon_system.glu")?;
    let (function, typ) = Compiler::new().run_expr(&vm, "update", &script)?;
    let script0 = create_script_sys(&vm, &world.res, function, &typ)?;

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
        world.maintain();
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use gluon::new_vm;

    fn test_world() -> World {
        let mut world = World::new();
        init_resources(&mut world.res);
        world
    }

    fn test_script_sys(
        vm: &Thread,
        res: &shred::World,
        script: &str,
    ) -> Result<DynamicSystem, failure::Error> {
        let (function, typ) = Compiler::new().run_expr(&vm, "update", script)?;
        create_script_sys(vm, res, function, &typ)
    }

    #[test]
    fn empty_system() {
        let mut world = test_world();

        let vm = new_vm();

        let script = r#"let f: () -> () = \x -> x in f"#;
        let script0 = test_script_sys(&vm, &world.res, script).unwrap();
        let mut scripts = DispatcherBuilder::new()
            .with(script0, "script0", &[])
            .build();

        scripts.dispatch(&mut world.res);
    }

    #[derive(Debug, Default, Clone, PartialEq, Getable, Pushable, Component, VmType)]
    struct Test(i32);

    #[test]
    fn update_component() {
        let mut world = test_world();
        let vm = new_vm();

        register_struct_type::<Test>(&vm, "Test");

        register_components! {world, vm,
           Test
        }

        let script = r#"let f: { test : Test } -> _ = \x -> { test =  x.test + 1 } in f"#;
        let script0 = test_script_sys(&vm, &world.res, script).unwrap();
        let mut scripts = DispatcherBuilder::new()
            .with(script0, "script0", &[])
            .build();

        let entity = world.create_entity().with(Test(1)).build();

        for _ in 0..2 {
            scripts.dispatch(&mut world.res);
        }

        assert_eq!(world.read_storage::<Test>().get(entity), Some(&Test(3)));
    }

    #[test]
    fn multiple_systems() {
        let mut world = test_world();
        let vm = new_vm();

        register_struct_type::<Test>(&vm, "Test");

        register_components! {world, vm,
            Test
        }

        let script = r#"let f: { test : Test } -> _ = \x -> { test = x.test + 1 } in f"#;
        let script0 = test_script_sys(&vm, &world.res, script).unwrap();
        let script = r#"let f: { test : Test } -> _ = \x -> { test = x.test + 10 } in f"#;
        let script1 = test_script_sys(&vm, &world.res, script).unwrap();
        let mut scripts = DispatcherBuilder::new()
            .with(script0, "script0", &[])
            .with(script1, "script1", &[])
            .build();

        let entity = world.create_entity().with(Test(1)).build();

        scripts.dispatch(&mut world.res);

        assert_eq!(world.read_storage::<Test>().get(entity), Some(&Test(12)));
    }

    #[test]
    fn missing_component() {
        let world = test_world();

        let vm = new_vm();

        let script = r#"let f: { test : Int } -> _ = \x -> { test =  x.test + 1 } in f"#;
        assert!(test_script_sys(&vm, &world.res, script).is_err());
    }

    #[test]
    fn wrong_argument_type() {
        let world = test_world();

        let vm = new_vm();

        let script = r#"let f: Int -> _ = \x -> () in f"#;
        assert!(test_script_sys(&vm, &world.res, script).is_err());
    }

    #[test]
    fn wrong_return_type() {
        let world = test_world();

        let vm = new_vm();

        let script = r#"let f: () -> Int = \x -> 1 in f"#;
        assert!(test_script_sys(&vm, &world.res, script).is_err());
    }

    #[test]
    fn tuple_argument() {
        let mut world = test_world();
        let vm = new_vm();

        register_struct_type::<Test>(&vm, "Test");

        register_components! {world, vm,
            Test
        }

        let script = r#"
            let f: (Test, Test) -> _ = \x -> { y = x._0 + 1 } in f
        "#;
        let script0 = test_script_sys(&vm, &world.res, script).unwrap();
        let mut scripts = DispatcherBuilder::new()
            .with(script0, "script0", &[])
            .build();

        let entity = world.create_entity().with(Test(1)).build();

        scripts.dispatch(&mut world.res);

        assert_eq!(world.read_storage::<Test>().get(entity), Some(&Test(2)));
    }

}
