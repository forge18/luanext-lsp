#![cfg(test)]

mod di_tests {
    use luanext_lsp::di::{DiContainer, ServiceLifetime};
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc;

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

    #[derive(Clone)]
    struct OtherService {
        value: i32,
    }

    impl TestTrait for OtherService {
        fn get_value(&self) -> i32 {
            self.value
        }
    }

    #[derive(Clone)]
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
    fn test_arc_trait_singleton() {
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
        container.register(|_| OtherService { value: 2 }, ServiceLifetime::Singleton);

        assert!(container.is_registered::<TestService>());
        assert_eq!(container.service_count(), 2);

        container.clear();

        assert!(!container.is_registered::<TestService>());
        assert_eq!(container.service_count(), 0);
        assert_eq!(container.singleton_count(), 0);
    }

    #[test]
    fn test_multiple_transient_resolves() {
        let mut container = DiContainer::new();
        let counter = AtomicUsize::new(0);
        container.register(
            move |_| {
                let new_val = counter.fetch_add(1, Ordering::SeqCst) + 1;
                TestService {
                    value: new_val as i32,
                }
            },
            ServiceLifetime::Transient,
        );

        let values: Vec<i32> = (0..10)
            .map(|_| container.resolve::<TestService>().unwrap().value)
            .collect();

        assert_eq!(values, (1..=10).collect::<Vec<_>>());
    }

    #[test]
    fn test_multiple_singleton_resolves() {
        let mut container = DiContainer::new();
        container.register(|_| TestService { value: 42 }, ServiceLifetime::Singleton);

        for _ in 0..10 {
            let service = container.resolve::<TestService>().unwrap();
            assert_eq!(service.value, 42);
        }
    }

    #[test]
    fn test_default_container() {
        let container = DiContainer::default();
        assert_eq!(container.service_count(), 0);
        assert_eq!(container.singleton_count(), 0);
    }

    #[test]
    fn test_service_registration_with_different_lifetimes() {
        let mut container = DiContainer::new();
        let counter = AtomicUsize::new(0);

        container.register(
            move |_| {
                let val = counter.fetch_add(1, Ordering::SeqCst) + 1;
                TestService { value: val as i32 }
            },
            ServiceLifetime::Transient,
        );
        container.register(|_| OtherService { value: 100 }, ServiceLifetime::Singleton);

        let transient1 = container.resolve::<TestService>();
        let singleton1 = container.resolve::<OtherService>();
        let transient2 = container.resolve::<TestService>();
        let singleton2 = container.resolve::<OtherService>();

        assert_ne!(transient1.unwrap().value, transient2.unwrap().value);
        assert_eq!(singleton1.unwrap().value, singleton2.unwrap().value);
    }
}
