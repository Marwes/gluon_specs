// in a real application you would use `fnv`
use std::{collections::HashMap, fmt, fs, mem, path::Path};

use failure;

use itertools::Itertools;

use specs::{
    storage::{MaskedStorage, Storage, UnprotectedStorage},
    world::{EntitiesRes, WorldExt},
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
    RootedThread, Thread, ThreadExt,
};

use gluon_codegen::{Trace, Userdata, VmType};

pub use gluon;

type GluonAny = OpaqueValue<RootedThread, Hole>;

/// Some trait that all of your dynamic resources should implement.
/// This trait should be able to register / transfer it to the scripting framework.
pub trait MarshalTo {
    fn to_gluon<'a>(&'a self, thread: &Thread, proxies: &mut Vec<Box<dyn Dropbox + 'a>>);
}

pub trait MarshalFrom: MarshalTo {
    fn from_gluon(&mut self, thread: &Thread, variants: Variants<'_>);

    fn new_value(thread: &Thread, variants: Variants<'_>) -> Self
    where
        Self: Sized;
}

impl<T> MarshalTo for T
where
    T: Userdata + VmType,
    T: ::std::fmt::Debug,
{
    fn to_gluon<'a>(&'a self, thread: &Thread, proxies: &mut Vec<Box<dyn Dropbox + 'a>>) {
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
                _proxies: &mut Vec<Box<dyn $crate::Dropbox + 'a>>,
            ) {
                $crate::gluon::vm::api::Pushable::push(self.clone(), &mut thread.current_context())
                    .unwrap()
            }
        }

        impl $crate::MarshalFrom for $ty {
            fn from_gluon(&mut self, thread: &gluon::Thread, variants: gluon::vm::Variants<'_>) {
                *self = Self::new_value(thread, variants);
            }

            fn new_value(thread: &gluon::Thread, variants: gluon::vm::Variants<'_>) -> Self {
                $crate::gluon::vm::api::Getable::from_value(thread, variants)
            }
        }
    };
}

#[derive(Userdata, Trace, VmType)]
#[gluon(vm_type = "EntitiesRes")]
#[gluon_trace(skip)]
#[repr(transparent)]
struct GluonEntities(EntitiesRes);

impl fmt::Debug for GluonEntities {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "GluonEntities(..)")
    }
}

#[derive(Userdata, Trace, Default, VmType)]
#[gluon(vm_type = "LazyUpdate")]
#[gluon_trace(skip)]
#[repr(transparent)]
struct GluonLazyUpdate(LazyUpdate);

impl fmt::Debug for GluonLazyUpdate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
pub struct ScriptSystem {
    dependencies: Dependencies,
    script: OwnedFunction<fn(GluonAny) -> GluonAny>,
}

impl<'a> System<'a> for ScriptSystem {
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

                    let res: &dyn Reflection = meta
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

                let res: &mut dyn ReflectionMut = meta
                    .reflections_mut
                    .get_mut(res)
                    .expect("Not registered in meta table");

                ReflectionMut::open_mut(res, data_res.fetch())
            })
            .collect();

        outputs.reverse();

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

impl ScriptSystem {
    pub fn from_file<P>(
        thread: &Thread,
        res: &shred::World,
        filename: &P,
    ) -> Result<ScriptSystem, failure::Error>
    where
        P: AsRef<Path> + ?Sized,
    {
        let module_name = gluon::base::filename_to_module(&filename.as_ref().display().to_string());
        let script = fs::read_to_string(filename)?;
        Self::from_script(thread, res, &module_name, &script)
    }

    pub fn from_script(
        thread: &Thread,
        res: &shred::World,
        name: &str,
        script: &str,
    ) -> Result<ScriptSystem, failure::Error> {
        let (function, typ) = thread.run_expr(name, script)?;
        ScriptSystem::new(thread, res, function, &typ)
    }

    pub fn new(
        thread: &Thread,
        res: &shred::World,
        function: OwnedFunction<fn(GluonAny) -> GluonAny>,
        update_type: &ArcType,
    ) -> Result<ScriptSystem, failure::Error> {
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

        let get_resource = |r| {
            table.get(r).cloned().ok_or_else(|| {
                failure::err_msg(format!(
                    "Missing resource `{}`. Existing resources: [{}]",
                    r,
                    table.map.keys().format(",")
                ))
            })
        };

        let sys = ScriptSystem {
            dependencies: Dependencies {
                thread: thread.root_thread(),
                read_type: read_type.clone(),
                reads: read_type
                    .row_iter()
                    .map(|field| &field.typ)
                    .map(|r| get_resource(r))
                    .collect::<Result<_, _>>()?,
                writes: write_type
                    .row_iter()
                    .map(|field| &field.typ)
                    .map(|r| get_resource(r))
                    .collect::<Result<_, _>>()?,
            },
            // just pass the function pointer
            script: function,
        };

        Ok(sys)
    }
}

pub trait Reflection {
    fn open<'a>(&'a self, entities: Fetch<'a, EntitiesRes>) -> Box<dyn ReflectionStorage + 'a>;
}

pub trait ReflectionMut: Reflection {
    fn open_mut<'a>(
        &'a mut self,
        entities: Fetch<'a, EntitiesRes>,
    ) -> Box<dyn ReflectionStorageMut + 'a>;
    fn add_component(lazy: &LazyUpdate, thread: &Thread, entity: Entity, value: Variants<'_>)
    where
        Self: Sized,
    {
        let _ = (lazy, thread, entity, value);
        unimplemented!()
    }
}

#[macro_export]
macro_rules! impl_reflection {
    ($($ty: ty),*) => {
        $(
            impl $crate::Reflection for $ty {
                fn open<'a>(&'a self, _entities: shred::Fetch<'a, specs::world::EntitiesRes>) -> Box<dyn $crate::ReflectionStorage + 'a > {
                    Box::new(self)
                }
            }

            impl<'a> $crate::ReflectionStorage for &'a $ty {
                fn mask(&self) -> Option<&specs::BitSet> {
                    None
                }
                unsafe fn get(&self, _index: u32) -> &dyn $crate::MarshalTo {
                    *self
                }
            }

        )*
    }
}

impl_reflection! { ResourceTable, ReflectionTable, GluonEntities, GluonLazyUpdate }

impl Reflection for EntitiesRes {
    fn open<'a>(&'a self, _entities: Fetch<'a, EntitiesRes>) -> Box<dyn ReflectionStorage + 'a> {
        Box::new(self)
    }
}

impl<'a> ReflectionStorage for &'a EntitiesRes {
    fn mask(&self) -> Option<&BitSet> {
        None
    }
    unsafe fn get(&self, _index: u32) -> &dyn MarshalTo {
        mem::transmute::<&EntitiesRes, &GluonEntities>(self)
    }
}

impl Reflection for LazyUpdate {
    fn open<'a>(&'a self, _entities: Fetch<'a, EntitiesRes>) -> Box<dyn ReflectionStorage + 'a> {
        Box::new(self)
    }
}

impl<'a> ReflectionStorage for &'a LazyUpdate {
    fn mask(&self) -> Option<&BitSet> {
        None
    }
    unsafe fn get(&self, _index: u32) -> &dyn MarshalTo {
        mem::transmute::<&LazyUpdate, &GluonLazyUpdate>(self)
    }
}

impl<T> Reflection for MaskedStorage<T>
where
    T: Component + MarshalTo + Send + Sync,
{
    fn open<'a>(&'a self, entities: Fetch<'a, EntitiesRes>) -> Box<dyn ReflectionStorage + 'a> {
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
    ) -> Box<dyn ReflectionStorageMut + 'a> {
        Box::new(Storage::new(entities, self))
    }

    fn add_component(lazy: &LazyUpdate, thread: &Thread, entity: Entity, value: Variants<'_>)
    where
        Self: Sized,
    {
        lazy.insert(entity, T::new_value(thread, value));
    }
}

pub trait ReflectionStorage {
    fn mask(&self) -> Option<&BitSet>;
    unsafe fn get(&self, index: u32) -> &dyn MarshalTo;
}

impl<'a, T, D> ReflectionStorage for Storage<'a, T, D>
where
    T: Component + MarshalTo + Send + Sync,
    D: std::ops::Deref<Target = MaskedStorage<T>>,
{
    fn mask(&self) -> Option<&BitSet> {
        Some(Storage::mask(self))
    }
    unsafe fn get(&self, index: u32) -> &dyn MarshalTo {
        self.unprotected_storage().get(index)
    }
}

pub trait ReflectionStorageMut: ReflectionStorage {
    unsafe fn get_mut(&mut self, index: u32) -> &mut dyn MarshalFrom;
}

impl<'a, T> ReflectionStorageMut for Storage<'a, T, &'a mut MaskedStorage<T>>
where
    T: Component + MarshalFrom + Send + Sync,
{
    unsafe fn get_mut(&mut self, index: u32) -> &mut dyn MarshalFrom {
        self.unprotected_storage_mut().get_mut(index)
    }
}

// Dummy trait to box something that just needs to be dropped later
pub trait Dropbox {}

impl<T> Dropbox for T {}

struct GluonJoin<'a> {
    reads: &'a [Box<dyn ReflectionStorage + 'a>],
    proxies: Vec<Box<dyn Dropbox + 'a>>,
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
        <GluonAny as Getable>::from_value(thread, variant.clone())
    }
}

fn reflection_bitset<'a>(iter: impl IntoIterator<Item = &'a dyn ReflectionStorage>) -> BitSet {
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

struct GluonJoinMut<'a, 'e> {
    writes: &'a mut [Box<dyn ReflectionStorageMut + 'e>],
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
        let value = outputs.pop().unwrap();
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

#[derive(Userdata, Trace, Debug, VmType)]
#[gluon_trace(skip)]
#[gluon(vm_type = "Entity")]
struct GluonEntity(Entity);

fn create_entity(entities: &GluonEntities) -> GluonEntity {
    GluonEntity(entities.0.create())
}

fn add_component(
    WithVM { vm, value: lazy }: WithVM<'_, &GluonLazyUpdate>,
    reflection_table: &ReflectionTable,
    name: &str,
    component: OpaqueRef<'_, generic::A>,
    entity: OpaqueRef<'_, GluonEntity>,
) -> RuntimeResult<(), String> {
    let entity = entity.0.clone();
    match reflection_table.add_component.get(name) {
        Some(f) => RuntimeResult::Return(f(&lazy.0, vm, entity, component.get_variant())),
        None => RuntimeResult::Panic(format!("Unknown component `{}`", name)),
    }
}

// necessary for `MetaTable`
unsafe impl<T> CastFrom<T> for dyn Reflection
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

unsafe impl<T> CastFrom<T> for dyn ReflectionMut
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

#[derive(Userdata, Trace, Default, VmType)]
#[gluon_trace(skip)]
#[gluon(vm_type = "ReflectionTable")]
struct ReflectionTable {
    reflections: MetaTable<dyn Reflection>,
    reflections_mut: MetaTable<dyn ReflectionMut>,
    add_component: HashMap<String, fn(&LazyUpdate, &Thread, Entity, Variants<'_>)>,
}

impl fmt::Debug for ReflectionTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ReflectionTable").finish()
    }
}

impl ReflectionTable {
    fn register<R>(&mut self, _name: &str, r: &R)
    where
        R: Reflection + Resource + Sized,
        dyn Reflection: CastFrom<R>,
    {
        self.reflections.register(r);
    }

    fn register_mut<R>(&mut self, name: &str, r: &R)
    where
        R: ReflectionMut + Resource + Sized,
        dyn ReflectionMut: CastFrom<R>,
    {
        self.register(name, r);

        self.reflections_mut.register(r);
        self.add_component
            .insert(name.to_string(), R::add_component);
    }
}

/// Maps resource names to resource ids.
#[derive(Userdata, Trace, Debug, Default, VmType)]
#[gluon_trace(skip)]
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
    Read(Ref<'a, Box<dyn Resource + 'static>>),
}

pub struct ScriptSystemData<'a> {
    meta_table: Read<'a, ReflectionTable>,
    read_fields: Vec<InternedStr>,
    reads: Vec<ReadType<'a>>,
    writes: Vec<RefMut<'a, Box<dyn Resource + 'static>>>,
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

pub fn register_component<T>(world: &mut specs::World, thread: &gluon::Thread, name: &str)
where
    T: Component + VmType + MarshalFrom + Send + Sync,
    T::Storage: Default,
{
    {
        let mut reflection_table = world.fetch_mut::<ReflectionTable>();
        let mut resource_table = world.fetch_mut::<ResourceTable>();
        reflection_table.register_mut(name, &specs::storage::MaskedStorage::<T>::default());
        let typ = <T as gluon::vm::api::VmType>::make_type(thread);
        resource_table.register_component::<T>(typ);
    }
    world.register::<T>();
}

pub fn register<T>(world: &mut shred::World, thread: &gluon::Thread, name: &str)
where
    T: Component + Reflection + VmType + MarshalFrom + Send + Sync + Default,
{
    let mut reflection_table = world.fetch_mut::<ReflectionTable>();
    let mut resource_table = world.fetch_mut::<ResourceTable>();
    reflection_table.register(name, &<T>::default());
    let typ = <T as gluon::vm::api::VmType>::make_type(thread);
    resource_table.register::<T>(typ);
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

    use specs::{Builder, DenseVecStorage, DispatcherBuilder};

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
    ) -> Result<ScriptSystem, failure::Error> {
        ScriptSystem::from_script(vm, res, "test", script)
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

        register_component::<Test>(&mut world, &vm, "Test");

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
    fn update_component_with_extra_entities() {
        let vm = new_vm();
        let mut world = test_world(&vm);

        register_component::<Test>(&mut world, &vm, "Test");

        let script = r#"let f: { test : Test } -> _ = \x -> { test =  x.test + 1 } in f"#;
        let script0 = test_script_sys(&vm, &world.res, script).unwrap();
        let mut scripts = DispatcherBuilder::new()
            .with(script0, "script0", &[])
            .build();

        world.create_entity().build();
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

        register_component::<Test>(&mut world, &vm, "Test");

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

        register_component::<Test>(&mut world, &vm, "Test");

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
