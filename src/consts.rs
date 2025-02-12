use phf::phf_ordered_map;

pub struct Kobject {
    /// The name of a Kconfig that indicates the presence of this object's definition in
    /// case it is not available in all configurations.
    pub kconfig: Option<&'static str>,
    /// Whether it is permissible for the object to be located in user-accessible memory.
    pub user_accessible: bool,
    /// Whether this item can be dynamically allocated with `k_object_alloc()`. Keep this in sync
    /// with the switch statement in `z_impl_k_object_alloc()`.
    pub dyn_allocatable: bool,
}
macro_rules! kobject {
    (None, $user_accessible:expr, $dyn_allocatable:expr) => {
        Kobject {
            kconfig: None,
            user_accessible: $user_accessible,
            dyn_allocatable: $dyn_allocatable,
        }
    };
    ($kconfig:expr, $user_accessible:expr, $dyn_allocatable:expr) => {
        Kobject {
            kconfig: Some($kconfig),
            user_accessible: $user_accessible,
            dyn_allocatable: $dyn_allocatable,
        }
    };
}
/// Keys in this map are structs which should be recognized as kernel objects.
/// Key names in all caps do not correspond to a specific data type but instead indicate that
/// objects of its type are of a family of compatible data structures.
pub const KOBJECTS: phf::OrderedMap<&str, Kobject> = phf_ordered_map! {
    "k_mem_slab" => kobject!(None, false, true),
    "k_msgq" => kobject!(None, false, true),
    "k_mutex" => kobject!(None, false, true),
    "k_pipe" => kobject!(None, false, true),
    "k_queue" => kobject!(None, false, true),
    "k_poll_signal" => kobject!(None, false, true),
    "k_sem" => kobject!(None, false, true),
    "k_stack" => kobject!(None, false, true),
    "k_thread" => kobject!(None, false, true),
    "k_timer" => kobject!(None, false, true),
    "z_thread_stack_element" => kobject!(None, false, false),
    "device" => kobject!(None, false, false),
    "NET_SOCKET" => kobject!(None, false, false),
    "net_if" => kobject!(None, false, false),
    "sys_mutex" => kobject!(None, true, false),
    "k_futex" => kobject!(None, true, false),
    "k_condvar" => kobject!(None, false, true),
    "k_event" => kobject!("CONFIG_EVENTS", false, true),
    "ztest_suite_node" => kobject!("CONFIG_ZTEST", true, false),
    "ztest_suite_stats" => kobject!("CONFIG_ZTEST", true, false),
    "ztest_unit_test" => kobject!("CONFIG_ZTEST", true, false),
    "ztest_test_rule" => kobject!("CONFIG_ZTEST", true, false),
    "rtio" => kobject!("CONFIG_RTIO", false, false),
    "rtio_iodev" => kobject!("CONFIG_RTIO", false, false),
    "sensor_decoder_api" => kobject!("CONFIG_SENSOR_ASYNC_API", true, false),
};
pub const STACK_TYPE: &str = "z_thread_stack_element";
