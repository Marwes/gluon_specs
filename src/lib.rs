// in a real application you would use `fnv`
use std::{collections::HashMap, fmt, mem};

use failure;

use itertools::Itertools;

use specs::{
    storage::{MaskedStorage, Storage, UnprotectedStorage},
    world::EntitiesRes,
    BitSet, Component, Entity, Join, LazyUpdate,
};

use shred::{
    self,
    cell::{Ref, RefMut},
    Accessor, AccessorCow, CastFrom, DynamicSystemData, Fetch, MetaTable, Read, Resource,
    ResourceId, System, SystemData,
};

use gluon::{
    base::types::{ArcType, Type, TypeExt},
    vm::{
        self,
        api::{
            generic, scoped, Getable, Hole, OpaqueRef, OpaqueValue, OwnedFunction, Pushable,
            RuntimeResult, Userdata, ValueRef, VmType, WithVM,
        },
        internal::InternedStr,
        primitive, record,
        thread::ThreadInternal,
        ExternModule, Variants,
    },
    RootedThread, Thread,
};

use gluon_codegen::{Userdata, VmType};

pub use gluon;

type GluonAny = OpaqueValue<RootedThread, Hole>;

/// Some trait that all of your dynamic resources should implement.
/// This trait should be able to register / transfer it to the scripting framework.
pub trait MarshalTo {
    fn to_gluon<'a>(&'a self, thread: &Thread, proxies: &mut Vec<Box<Dropbox + 'a>>);
}

pub trait MarshalFrom: MarshalTo {
    fn from_gluon(&mut self, thread: &Thread, variants: Variants);

    fn new_value(thread: &Thread, variants: Variants) -> Self
    where
        Self: Sized;
}

impl<T> MarshalTo for T
where
    T: Userdata + VmType,
    T: ::std::fmt::Debug,
{
    fn to_gluon<'a>(&'a self, thread: &Thread, proxies: &mut Vec<Box<Dropbox + 'a>>) {
        let mut proxy = scoped::Ref::new(self);
        Pushable::push(&mut proxy, &mut thread.current_context()).unwrap();
        proxies.push(Box::new(proxy));
    }
}

#[macro_export]
macro_rules! impl_clone_marshal {
    ($ty: ty) => {
        impl $crate::MarshalTo for $ty {
            fn to_gluon<'a>(
                &'a self,
                thread: &gluon::Thread,
                _proxies: &mut Vec<Box<$crate::Dropbox + 'a>>,
            ) {
                $crate::gluon::vm::api::Pushable::push(self.clone(), &mut thread.current_context())
                    .unwrap()
            }
        }

        impl $crate::MarshalFrom for $ty {
            fn from_gluon(&mut self, thread: &gluon::Thread, variants: gluon::vm::Variants) {
                *self = Self::new_value(thread, variants);
            }

            fn new_value(thread: &gluon::Thread, variants: gluon::vm::Variants) -> Self {
                $crate::gluon::vm::api::Getable::from_value(thread, variants)
            }
        }
    };
}

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

pub struct Dependencies {
    read_type: ArcType,
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
pub struct DynamicSystem {
    dependencies: Dependencies,
    script: OwnedFunction<fn(GluonAny) -> GluonAny>,
}

impl<'a> System<'a> for DynamicSystem {
    type SystemData = ScriptSystemData<'a>;

    fn run(&mut self, data: Self::SystemData) {
        let meta = data.meta_table;
        let data_res = data.res;
        let mut writes = data.writes;

        let mask;

        let mut outputs = Vec::new();
        {
            let reads: Vec<_> = data
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
                .map(|res| Reflection::open(res, data_res.fetch()))
                .collect();

            let writes: Vec<_> = writes
                .iter()
                .map(|resource| {
                    let res = Box::as_ref(resource);

                    let res: &Reflection = meta
                        .reflections
                        .get(res)
                        .expect("Not registered in meta table");

                    Reflection::open(res, data_res.fetch())
                })
                .collect();
            mask = reflection_bitset(reads.iter().chain(writes.iter()).map(|b| &**b));

            let thread = self.script.vm().root_thread();

            for input in (GluonJoin {
                reads: &reads,
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

        let mut writes: Vec<_> = writes
            .iter_mut()
            .map(|resource| {
                let res = Box::as_mut(resource);

                let res: &mut ReflectionMut = meta
                    .reflections_mut
                    .get_mut(res)
                    .expect("Not registered in meta table");

                ReflectionMut::open_mut(res, data_res.fetch())
            })
            .collect();

        let writes = GluonJoinMut {
            writes: &mut writes,
            outputs,
            mask: &mask,
        };
        for () in writes.join() {}
    }

    fn accessor<'b>(&'b self) -> AccessorCow<'a, 'b, Self> {
        AccessorCow::Ref(&self.dependencies)
    }

    fn setup(&mut self, _res: &mut shred::World) {
        // this could call a setup function of the script
    }
}

pub trait Reflection {
    fn open<'a>(&'a self, entities: Fetch<'a, EntitiesRes>) -> Box<ReflectionStorage + 'a>;
}

pub trait ReflectionMut: Reflection {
    fn open_mut<'a>(
        &'a mut self,
        entities: Fetch<'a, EntitiesRes>,
    ) -> Box<ReflectionStorageMut + 'a>;
    fn add_component(lazy: &LazyUpdate, thread: &Thread, entity: Entity, value: Variants)
    where
        Self: Sized,
    {
        let _ = (lazy, thread, entity, value);
        unimplemented!()
    }
}

macro_rules! impl_reflection {
    ($($ty: ty),*) => {
        $(
            impl Reflection for $ty {
                fn open<'a>(&'a self, _entities: Fetch<'a, EntitiesRes>) -> Box<ReflectionStorage + 'a > {
                    Box::new(self)
                }
            }

            impl<'a> ReflectionStorage for &'a $ty {
                fn mask(&self) -> Option<&BitSet> {
                    None
                }
                unsafe fn get(&self, _index: u32) -> &MarshalTo {
                    *self
                }
            }

        )*
    }
}

impl_reflection! { ResourceTable, ReflectionTable, GluonEntities, GluonLazyUpdate }

impl Reflection for EntitiesRes {
    fn open<'a>(&'a self, _entities: Fetch<'a, EntitiesRes>) -> Box<ReflectionStorage + 'a> {
        Box::new(self)
    }
}

impl<'a> ReflectionStorage for &'a EntitiesRes {
    fn mask(&self) -> Option<&BitSet> {
        None
    }
    unsafe fn get(&self, _index: u32) -> &MarshalTo {
        mem::transmute::<&EntitiesRes, &GluonEntities>(self)
    }
}

impl Reflection for LazyUpdate {
    fn open<'a>(&'a self, _entities: Fetch<'a, EntitiesRes>) -> Box<ReflectionStorage + 'a> {
        Box::new(self)
    }
}

impl<'a> ReflectionStorage for &'a LazyUpdate {
    fn mask(&self) -> Option<&BitSet> {
        None
    }
    unsafe fn get(&self, _index: u32) -> &MarshalTo {
        mem::transmute::<&LazyUpdate, &GluonLazyUpdate>(self)
    }
}

impl<T> Reflection for MaskedStorage<T>
where
    T: Component + MarshalTo + Send + Sync,
{
    fn open<'a>(&'a self, entities: Fetch<'a, EntitiesRes>) -> Box<ReflectionStorage + 'a> {
        Box::new(Storage::new(entities, self))
    }
}

impl<T> ReflectionMut for MaskedStorage<T>
where
    T: Component + MarshalFrom + Send + Sync,
{
    fn open_mut<'a>(
        &'a mut self,
        entities: Fetch<'a, EntitiesRes>,
    ) -> Box<ReflectionStorageMut + 'a> {
        Box::new(Storage::new(entities, self))
    }

    fn add_component(lazy: &LazyUpdate, thread: &Thread, entity: Entity, value: Variants)
    where
        Self: Sized,
    {
        lazy.insert(entity, T::new_value(thread, value));
    }
}

pub trait ReflectionStorage {
    fn mask(&self) -> Option<&BitSet>;
    unsafe fn get(&self, index: u32) -> &MarshalTo;
}

impl<'a, T, D> ReflectionStorage for Storage<'a, T, D>
where
    T: Component + MarshalTo + Send + Sync,
    D: std::ops::Deref<Target = MaskedStorage<T>>,
{
    fn mask(&self) -> Option<&BitSet> {
        Some(Storage::mask(self))
    }
    unsafe fn get(&self, index: u32) -> &MarshalTo {
        self.unprotected_storage().get(index)
    }
}

pub trait ReflectionStorageMut: ReflectionStorage {
    unsafe fn get_mut(&mut self, index: u32) -> &mut MarshalFrom;
}

impl<'a, T> ReflectionStorageMut for Storage<'a, T, &'a mut MaskedStorage<T>>
where
    T: Component + MarshalFrom + Send + Sync,
{
    unsafe fn get_mut(&mut self, index: u32) -> &mut MarshalFrom {
        self.unprotected_storage_mut().get_mut(index)
    }
}

// Dummy trait to box something that just needs to be dropped later
pub trait Dropbox {}

impl<T> Dropbox for T {}

struct GluonJoin<'a> {
    reads: &'a [Box<ReflectionStorage + 'a>],
    proxies: Vec<Box<Dropbox + 'a>>,
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
            let read = ReflectionStorage::get(&**reflection, index);
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

fn reflection_bitset<'a>(iter: impl IntoIterator<Item = &'a ReflectionStorage>) -> BitSet {
    iter.into_iter()
        .flat_map(|reflection| reflection.mask())
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

struct GluonJoinMut<'a, 'e: 'a> {
    writes: &'a mut [Box<ReflectionStorageMut + 'e>],
    outputs: Vec<GluonAny>,
    mask: &'a BitSet,
}

impl<'a, 'e: 'a> Join for GluonJoinMut<'a, 'e> {
    type Type = ();
    type Value = Self;
    type Mask = &'a BitSet;
    unsafe fn open(self) -> (Self::Mask, Self::Value) {
        (self.mask, self)
    }

    unsafe fn get(value: &mut Self::Value, index: u32) -> Self::Type {
        let GluonJoinMut {
            writes, outputs, ..
        } = value;

        // FIXME Don't rely on the indexes being sequential
        let value = &outputs[index as usize];
        let thread = value.vm();
        // call the script with the input
        match value.get_variant().as_ref() {
            ValueRef::Data(data) => {
                for (variant, write) in data.iter().zip(&mut **writes) {
                    write.get_mut(index).from_gluon(thread, variant);
                }
            }
            _ => panic!("Expected Data when writing the result of a script system"),
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
    match reflection_table.add_component.get(name) {
        Some(f) => RuntimeResult::Return(f(&lazy.0, vm, entity, component.get_variant())),
        None => RuntimeResult::Panic(format!("Unknown component `{}`", name)),
    }
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
pub struct ReflectionTable {
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
    pub fn register<R>(&mut self, _name: &str, r: &R)
    where
        R: Reflection + Resource + Sized,
        Reflection: CastFrom<R>,
    {
        self.reflections.register(r);
    }

    pub fn register_mut<R>(&mut self, name: &str, r: &R)
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
pub struct ResourceTable {
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

    pub fn register_component<T: Component>(&mut self, name: ArcType) {
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

pub struct ScriptSystemData<'a> {
    meta_table: Read<'a, ReflectionTable>,
    read_fields: Vec<InternedStr>,
    reads: Vec<ReadType<'a>>,
    writes: Vec<RefMut<'a, Box<Resource + 'static>>>,
    #[allow(unused)] // FIXME Clone this when running the system instead of re-fetching
    entities: Fetch<'a, EntitiesRes>,
    res: &'a shred::World,
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
            res,
        }
    }
}

pub fn create_script_system(
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
        table.get(r).cloned().ok_or_else(|| {
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
            reads: reads
                .iter()
                .map(|r| get_resource(r))
                .collect::<Result<_, _>>()?,
            writes: writes
                .iter()
                .map(|r| get_resource(r))
                .collect::<Result<_, _>>()?,
        },
        // just pass the function pointer
        script: function,
    };

    Ok(sys)
}

#[macro_export]
macro_rules! register_components {
    ($world: expr, $thread: expr, $($t: ty),+) => {{
        let ref mut world = $world;
        let ref thread = $thread;
        {
            let mut reflection_table = world.res.fetch_mut::<$crate::ReflectionTable>();
            let mut resource_table = world.res.fetch_mut::<$crate::ResourceTable>();
            $(
                reflection_table.register_mut(stringify!($t), &specs::storage::MaskedStorage::<$t>::default());
                let typ = <$t as gluon::vm::api::VmType>::make_type(thread);
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
            let mut reflection_table = world.fetch_mut::<$crate::ReflectionTable>();
            let mut resource_table = world.fetch_mut::<$crate::ResourceTable>();
            $(
                reflection_table.register(stringify!($t), &<$t>::default());
                let typ = thread.get_type::<$t>().unwrap_or_else(|| panic!("`{}` is missing", stringify!($t)));
                resource_table.register::<$t>(typ);
            )+
        }
    }}
}

pub fn init_resources(world: &mut shred::World, thread: &Thread) {
    world.entry().or_insert_with(|| ReflectionTable::default());
    world.entry().or_insert_with(|| ResourceTable::new());

    thread
        .register_type::<EntitiesRes>("EntitiesRes", &[])
        .unwrap();
    thread.register_type::<GluonEntity>("Entity", &[]).unwrap();
    thread
        .register_type::<LazyUpdate>("LazyUpdate", &[])
        .unwrap();
    thread
        .register_type::<ResourceTable>("ResourceTable", &[])
        .unwrap();
    thread
        .register_type::<ReflectionTable>("ReflectionTable", &[])
        .unwrap();

    register! {*world, thread,
        EntitiesRes,
        LazyUpdate,
        ReflectionTable,
        ResourceTable
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

    gluon::import::add_extern_module(thread, "entity", load);
}

#[cfg(test)]
mod tests {
    use super::*;

    use specs::{Builder, DispatcherBuilder};

    use specs_derive::Component;

    use gluon_codegen::{Getable, Pushable};

    use gluon::new_vm;

    fn test_world(thread: &Thread) -> specs::World {
        let mut world = specs::World::new();
        init_resources(&mut world.res, thread);
        world
    }

    fn test_script_sys(
        vm: &Thread,
        res: &shred::World,
        script: &str,
    ) -> Result<DynamicSystem, failure::Error> {
        let (function, typ) = gluon::Compiler::new().run_expr(&vm, "update", script)?;
        create_script_system(vm, res, function, &typ)
    }

    #[test]
    fn empty_system() {
        let vm = new_vm();
        let mut world = test_world(&vm);

        let script = r#"let f: () -> () = \x -> x in f"#;
        let script0 = test_script_sys(&vm, &world.res, script).unwrap();
        let mut scripts = DispatcherBuilder::new()
            .with(script0, "script0", &[])
            .build();

        scripts.dispatch(&mut world.res);
    }

    #[derive(Debug, Default, Clone, PartialEq, Getable, Pushable, Component, VmType)]
    #[gluon(newtype)]
    struct Test(i32);
    impl_clone_marshal!(Test);

    #[test]
    fn update_component() {
        let vm = new_vm();
        let mut world = test_world(&vm);

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
        let vm = new_vm();
        let mut world = test_world(&vm);

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
        let vm = new_vm();
        let world = test_world(&vm);

        let script = r#"let f: { test : Int } -> _ = \x -> { test =  x.test + 1 } in f"#;
        assert!(test_script_sys(&vm, &world.res, script).is_err());
    }

    #[test]
    fn wrong_argument_type() {
        let vm = new_vm();
        let world = test_world(&vm);

        let script = r#"let f: Int -> _ = \x -> () in f"#;
        assert!(test_script_sys(&vm, &world.res, script).is_err());
    }

    #[test]
    fn wrong_return_type() {
        let vm = new_vm();
        let world = test_world(&vm);

        let script = r#"let f: () -> Int = \x -> 1 in f"#;
        assert!(test_script_sys(&vm, &world.res, script).is_err());
    }

    #[test]
    fn tuple_argument() {
        let vm = new_vm();
        let mut world = test_world(&vm);

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
