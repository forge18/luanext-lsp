use std::any::{Any, TypeId};
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Clone, Copy)]
#[allow(dead_code)]
pub enum ServiceLifetime {
    Transient,
    Singleton,
}

pub struct DiContainer {
    factories: HashMap<TypeId, FactoryFn>,
    lifetimes: HashMap<TypeId, ServiceLifetime>,
    singletons: HashMap<TypeId, Arc<dyn Any + Send + Sync>>,
}

type FactoryFn = Arc<dyn Fn(&mut DiContainer) -> Arc<dyn Any + Send + Sync> + Send + Sync>;

impl DiContainer {
    pub fn new() -> Self {
        Self {
            factories: HashMap::new(),
            lifetimes: HashMap::new(),
            singletons: HashMap::new(),
        }
    }

    pub fn register<T>(
        &mut self,
        factory: impl Fn(&mut DiContainer) -> T + 'static + Send + Sync,
        lifetime: ServiceLifetime,
    ) where
        T: Clone + Send + Sync + 'static,
    {
        let type_id = TypeId::of::<T>();
        let factory_fn: FactoryFn = Arc::new(move |_container| {
            let value = factory(_container);
            let arc: Arc<dyn Any + Send + Sync> = Arc::new(value);
            arc
        });
        self.factories.insert(type_id, factory_fn);
        self.lifetimes.insert(type_id, lifetime);
    }

    #[allow(dead_code)]
    pub fn register_with_factory<T>(
        &mut self,
        factory: impl Fn(&mut DiContainer) -> T + 'static + Send + Sync,
        lifetime: ServiceLifetime,
    ) where
        T: Clone + Send + Sync + 'static,
    {
        self.register(factory, lifetime);
    }

    pub fn resolve<T>(&mut self) -> Option<T>
    where
        T: Clone + Send + Sync + 'static,
    {
        let type_id = TypeId::of::<T>();

        let lifetime = self.lifetimes.get(&type_id).copied();

        match lifetime {
            Some(ServiceLifetime::Singleton) => {
                if let Some(singleton) = self.singletons.get(&type_id).cloned() {
                    return singleton.downcast::<T>().ok().map(|arc| (*arc).clone());
                }

                let factory = self.factories.get(&type_id).cloned();
                if let Some(factory) = factory {
                    let result = (factory)(self);
                    let clone = result.clone();
                    self.singletons.insert(type_id, result);
                    return clone.downcast::<T>().ok().map(|arc| (*arc).clone());
                }
            }
            Some(ServiceLifetime::Transient) | None => {
                let factory = self.factories.get(&type_id).cloned();
                if let Some(factory) = factory {
                    let result = (factory)(self);
                    return result.downcast::<T>().ok().map(|arc| (*arc).clone());
                }
            }
        }

        None
    }

    #[allow(dead_code)]
    pub fn is_registered<T>(&self) -> bool
    where
        T: 'static,
    {
        self.factories.contains_key(&TypeId::of::<T>())
    }

    #[allow(dead_code)]
    pub fn service_count(&self) -> usize {
        self.factories.len()
    }

    #[allow(dead_code)]
    pub fn singleton_count(&self) -> usize {
        self.singletons.len()
    }

    #[allow(dead_code)]
    pub fn clear(&mut self) {
        self.factories.clear();
        self.lifetimes.clear();
        self.singletons.clear();
    }
}

impl Default for DiContainer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    trait TestTrait: Send + Sync {
        fn get_value(&self) -> i32;
    }

    #[derive(Clone)]
    struct TestService {
        value: i32,
    }

    impl TestTrait for TestService {
        fn get_value(&self) -> i32 {
            self.value
        }
    }

    #[test]
    fn test_transient_service() {
        let mut container = DiContainer::new();
        let counter = AtomicUsize::new(0);
        container.register(
            move |_| {
                let val = counter.fetch_add(1, Ordering::SeqCst) + 1;
                TestService { value: val as i32 }
            },
            ServiceLifetime::Transient,
        );

        let service1 = container.resolve::<TestService>();
        let service2 = container.resolve::<TestService>();

        assert!(service1.is_some());
        assert!(service2.is_some());
        assert_ne!(service1.unwrap().value, service2.unwrap().value);
    }

    #[test]
    fn test_singleton_service() {
        let mut container = DiContainer::new();
        container.register(|_| TestService { value: 42 }, ServiceLifetime::Singleton);

        let service1 = container.resolve::<TestService>();
        let service2 = container.resolve::<TestService>();

        assert!(service1.is_some());
        assert!(service2.is_some());
        assert_eq!(service1.unwrap().value, service2.unwrap().value);
    }

    #[test]
    fn test_arc_singleton() {
        let mut container = DiContainer::new();
        container.register(
            |_| Arc::new(TestService { value: 99 }) as Arc<dyn TestTrait>,
            ServiceLifetime::Singleton,
        );

        let service1 = container.resolve::<Arc<dyn TestTrait>>();
        let service2 = container.resolve::<Arc<dyn TestTrait>>();

        assert!(service1.is_some());
        assert!(service2.is_some());
        assert_eq!(service1.unwrap().get_value(), service2.unwrap().get_value());
    }

    #[test]
    fn test_dependent_services() {
        struct DepService {
            value: i32,
        }

        impl DepService {
            fn new() -> Self {
                Self { value: 10 }
            }
        }

        struct MainService {
            dep: Arc<DepService>,
        }

        impl MainService {
            fn new(dep: Arc<DepService>) -> Self {
                Self { dep }
            }
        }

        impl TestTrait for MainService {
            fn get_value(&self) -> i32 {
                self.dep.value + 5
            }
        }

        let mut container = DiContainer::new();
        container.register(|_| Arc::new(DepService::new()), ServiceLifetime::Singleton);
        container.register(
            |container| {
                let dep = container.resolve::<Arc<DepService>>().unwrap();
                Arc::new(MainService::new(dep)) as Arc<dyn TestTrait>
            },
            ServiceLifetime::Singleton,
        );

        let service = container.resolve::<Arc<dyn TestTrait>>();
        assert_eq!(service.unwrap().get_value(), 15);
    }

    #[test]
    fn test_is_registered() {
        let mut container = DiContainer::new();
        assert!(!container.is_registered::<TestService>());

        container.register(|_| TestService { value: 1 }, ServiceLifetime::Transient);
        assert!(container.is_registered::<TestService>());
    }

    #[test]
    fn test_service_count() {
        let mut container = DiContainer::new();
        assert_eq!(container.service_count(), 0);

        container.register(|_| TestService { value: 1 }, ServiceLifetime::Transient);
        assert_eq!(container.service_count(), 1);

        #[derive(Clone)]
        struct OtherService {
            value: i32,
        }

        container.register(|_| OtherService { value: 2 }, ServiceLifetime::Singleton);
        assert_eq!(container.service_count(), 2);
    }

    #[test]
    fn test_singleton_count() {
        let mut container = DiContainer::new();
        assert_eq!(container.singleton_count(), 0);

        container.register(|_| TestService { value: 1 }, ServiceLifetime::Transient);
        let _ = container.resolve::<TestService>();
        assert_eq!(container.singleton_count(), 0);

        container.register(|_| TestService { value: 2 }, ServiceLifetime::Singleton);
        let _ = container.resolve::<TestService>();
        assert_eq!(container.singleton_count(), 1);
    }

    #[test]
    fn test_nonexistent_service() {
        let mut container = DiContainer::new();
        let service = container.resolve::<TestService>();
        assert!(service.is_none());
    }

    #[test]
    fn test_clear_container() {
        let mut container = DiContainer::new();
        container.register(|_| TestService { value: 1 }, ServiceLifetime::Transient);

        #[derive(Clone)]
        struct OtherService {
            value: i32,
        }

        container.register(|_| OtherService { value: 2 }, ServiceLifetime::Singleton);

        assert!(container.is_registered::<TestService>());
        assert_eq!(container.service_count(), 2);

        container.clear();

        assert!(!container.is_registered::<TestService>());
        assert_eq!(container.service_count(), 0);
        assert_eq!(container.singleton_count(), 0);
    }

    #[test]
    fn test_default_container() {
        let container = DiContainer::default();
        assert_eq!(container.service_count(), 0);
        assert_eq!(container.singleton_count(), 0);
    }
}
